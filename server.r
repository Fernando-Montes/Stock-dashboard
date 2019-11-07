shinyServer(function(input, output, session){
  # output$table = DT::renderDataTable({
  #   datatable(stocks_w_sec, rownames = F)
  # })
  
  observe({
    if (input$sector == "All sectors" & input$industry == "All industries") st = na.omit( unique( stockInfo$FullName ))
    else {
      if(input$industry == "All industries")
        st = na.omit(unique(stockInfo[stockInfo$Industry.Num %in% listAll[listAll$Sector.Name == input$sector,]$Industry.Num,]$FullName))
      else st = na.omit(unique(stockInfo[stockInfo$Industry.Num %in% listAll[listAll$Industry.Name == input$industry,]$Industry.Num,]$FullName))
    }
    st = sort(st)
    updateSelectizeInput(
      session, 'price1',
      choices = st
      # ,
      # selected = input$price1
    )
  })
  
  observe({
    if (input$sector == "All sectors") ind = na.omit(c("All industries", sort(as.vector(unique( listAll$Industry.Name )))))
    else ind = na.omit(c("All industries", sort(as.vector(unique( listAll[listAll$Sector.Name == input$sector,]$Industry.Name )))))
    updateSelectInput(
      session, 'industry',
      choices = ind,
      selected = "All industries")
  })

  stock <- reactive({
    if ( input$price1 %in% stockInfo$FullName ) stock <- stockInfo[stockInfo$FullName == input$price1, "Stock.SYM"]  
    else stock <- "A"
  })
  
  stockFull <- reactive({
    if ( input$price1 %in% stockInfo$FullName ) stock <- stockInfo[stockInfo$FullName == input$price1, "FullName"]  
    else stock <- " "
  })
  
  output$sectorText = renderText({
    sectorNum = stockInfo[stockInfo$Stock.SYM == stock(), "Sector.Num"]
    sectorName = as.character(listAll[listAll$Sector.Num == sectorNum,]$Sector.Name[1])
  })
  
  output$industryText = renderText({
    industryNum = stockInfo[stockInfo$Stock.SYM == stock(),]$Industry.Num[1]
    industryName = as.character(listAll[listAll$Industry.Num == industryNum,]$Industry.Name[1])
  })
  
  # ----------------------------- 
  # Stock Price Movement --------
  # -----------------------------
  stock_selected1 = reactive({
    fileName <- paste(targetPath, stock(), "-prices.RData", sep="")
    if ( class(try(load(file = fileName), silent = TRUE)) != "try-error") {
      temp = SYMB_prices
      SYMB_prices = cbind( index(temp), as.data.frame(temp) ) %>% na.omit(.) %>% unique(.) 
      if(input$sma == TRUE) 
        SYMB_prices = SYMB_prices %>% mutate(., SMA1 = SMA(Close, n = input$sma1)) %>% mutate(., SMA2 = SMA(Close, n = input$sma2)) 
      if(input$rsi == TRUE)  
        SYMB_prices = SYMB_prices %>% mutate(., RSI = RSI(Close, n = input$rsi1)) 
      SYMB_prices %>% column_to_rownames(.) 
    }
  })
  
  output$price = renderDygraph({
    tryCatch({
      if (input$rsi == TRUE) {
        dygraph(stock_selected1(), main = stock(), group = 'my_stocks') %>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y", label = "Price") %>% dyCandlestick() %>%
          dyAxis("y2", label = "RSI", independentTicks = TRUE) %>% dySeries("RSI", axis = 'y2') %>%
          dyLegend(show = 'always', hideOnMouseOut = T) %>%
          dyRangeSelector(retainDateWindow = T) %>%
          dyCSS("Misc/legend.css") }
      else {
        dygraph(stock_selected1(), main = stock(), group = 'my_stocks') %>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y", label = "Price") %>% dyCandlestick() %>%
          dyLegend(show = 'always', hideOnMouseOut = T) %>%
          dyRangeSelector(retainDateWindow = T) %>%
          dyCSS("Misc/legend.css")
      }
    }, error = function(e) {
      return()
    })
  })
  # -----------------------------
  # -----------------------------
  
  # ----------------------------- 
  # Indicators ------------------
  # -----------------------------
  
  indict_selected = reactive({
    if (input$sector == "All sectors" & input$industry == "All industries") flag = 1
    else {
      if(input$industry == "All industries")  flag = 1
      else flag = 2
    }
    temp = indict_b_sto_func(stock(), flag)
    temp[complete.cases(temp[, input$indict]), ] %>% unique(.)
  })
  
  ret_day = reactive({
    if (input$sector == "All sectors" & input$industry == "All industries") flag = 1
    else {
      if(input$industry == "All industries")  flag = 1
      else flag = 2
    }
    rbind( indict_ave_func(indict_selected()), 
           indict_b_sec_func(indict_selected()), 
           indict_b_ind_func(indict_selected(), stock(), flag), 
           indict_selected()) %>% arrange(., industry) 
  })

  output$indText = renderText({
    temp = indict_selected()
    indDict = list("price" = "Latest stock price", "EV" = "Latest stock price x available number of shares", 
                   "EBITDA" = "Earnings before interest, taxes, depreciation, amortization and unusual expenses",
                   "EV.earning" = "Latest stock price x available number of shares / Earnings", 
                   "EV.EBITDA" = "Latest stock price x available number of shares / Earnings before interest, taxes, depreciation, amortization and unusual expenses", 
                   "EV.book" = "Latest stock price x available number of shares / Book value", 
                   "EV.revenue" = "Latest stock price x available number of shares / Revenue", 
                   "EV.cash" = "Latest stock price x available number of shares / Cash", 
                   "EquityAssets.liability" = "Assets + Equity / Liabilities")
    if(stock() %in% temp$SYM) {
      paste(indDict[[input$indict]], " equal to ", round(temp[temp$SYM == stock(), input$indict],2), " for stock ", stock(), ".", sep = "") 
    } else {
      paste('Stock ', stock(), " does not have the required information.",  sep = "") 
    }
  })
  
  output$tree = renderGvis({
      gvisTreeMap(ret_day(),
                  idvar = 'SYM', parentvar = 'industry',
                  sizevar = "price", colorvar = input$indict,
                  options = list(
                    showScale = T,
                    highlightOnMouseOver = T,
                    height = 300,
                    maxDepth = 1,
                    maxPostDepth = 2,
                    minColor = 'green',
                    maxColor = 'red'
                  ))
  })
  
  output$bar = renderPlotly({
    temp = indict_selected()
    color = rep('rgba(204,204,204,1)', dim(temp)[1])
    color[match(stock(), temp$SYM)] = 'rgba(204,45,38,1)'
    marker = list(color = color)
    value = temp[temp$SYM == stock(), input$indict]
    plot_ly(x = temp[,'SYM'],
            y = temp[, input$indict], type = 'bar', marker = marker ) %>%
      layout(
        shapes=list(type='line', x0= 0, x1=dim(temp)[1] , y0=value, y1=value, line=list(dash='dot', width=1)),
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = input$indict))
  })
  
  cor_selected = reactive({
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing data", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a closure to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    temp = sto_ind_func(stock(), updateProgress)
    select(temp, -Open, -High, -Low) %>% spread(., key = SYM, value = Close) %>% 
      select(., -Date) %>% cor(., use = "pairwise.complete.obs") %>% round(., 3)
  })
  
  output$corr = renderPlotly({
      plot_ly(x = rownames(cor_selected()), y = colnames(cor_selected()),
              z = cor_selected(), type = 'heatmap')
  })
  
  
  output$varImpTable = renderGvis({
    withProgress(message = 'Table in progress',
                 detail = '....', value = 0, {
                   temp = varImpPlot(input$MLModel, input$numdays)
                   gvisTable(temp, formats=list("Importance [%]"="#.##"), options=list(page='enable'))
                 })
  })
  
  output$indicPlot = renderPlotly({
    withProgress(message = 'Plot in progress',
                 detail = '....', value = 0, { 
                 ggplotly( indicatorXYPlot( stock(), input$numdays, input$indDisplay_x, input$indDisplay_y, input$indDisplay_c) )
                 })
  })
  
  # -----------------------------
  # -----------------------------
  
  # ----------------------------- 
  # Forecast --------------------
  # -----------------------------
  
  fit_data = reactive({
    stock_selected1() %>% select(., Close) 
  })
  
  pred_table_stock = reactive({
    prediction_table_stock(input$numdays, stock())
  })
  
  fit_data_pred = reactive({
    res = fit_data()
    temp = pred_table_stock()
    if(input$Ranger == TRUE) {
      pred = as.data.frame(temp$return_RF)
      colnames(pred) = c("Random Forest")
      rownames(pred) = temp$date.predicted
      pred = xts( pred, order.by=as.POSIXct(rownames(pred)) )
      res = xts( res, order.by=as.POSIXct(rownames(res)) )
      res = cbind(res, pred)
    }
    if(input$gbm == TRUE) {
      pred = as.data.frame(temp$return_GBM)
      colnames(pred) = c("GBM")
      rownames(pred) = temp$date.predicted
      pred = xts( pred, order.by=as.POSIXct(rownames(pred)) )
      res = tryCatch( { xts( res, order.by=as.POSIXct(rownames(res)) ) }, error = function(e) {return (res)} )
      res = cbind(res, pred)
    }
    if(input$glmnet == TRUE) {
      pred = as.data.frame(temp$return_GLMNET)
      colnames(pred) = c("GLMNET")
      rownames(pred) = temp$date.predicted
      pred = xts( pred, order.by=as.POSIXct(rownames(pred)) )
      res = tryCatch( { xts( res, order.by=as.POSIXct(rownames(res)) ) }, error = function(e) {return (res)} )
      res = cbind(res, pred)
    }
    fit_data_pred = res
  })
  
  fit_data_zoom = reactive({ 
    # Need to make sure selected dates are within dataframe, if not adjust
    maxDate = as.Date(rownames(fit_data())[nrow(fit_data())])
    date1 = input$date_range[1] 
    while (!(date1 %in% as.Date(rownames(fit_data()))) & date1 <= maxDate) {
      date1 = date1 + 1
    }
    date2 = input$date_range[2] 
    while (!(date2 %in% as.Date(rownames(fit_data()))) & date2 <= maxDate) {
      date2 = date2 + 1
    }
    if ( date2 > maxDate ) date2 = maxDate
    date1 = which( rownames(fit_data()) == date1  )
    date2 = which( rownames(fit_data()) == date2  )
    fit_data()[date1:date2, , FALSE]
  })
  
  fit_model = reactive({
    fit_data_zoom() %>%
      as.ts(.) %>%
      Arima(., order = c(input$ar, input$diff, input$ma), method="ML")
  })
  
  output$forecastplot = renderDygraph({
    tryCatch({
      withProgress(message = 'Plot in progress',
                   detail = '....', value = 0, {
                     if(input$ARIMA == F) {
                       dygraph(fit_data_pred(), main = stock()) %>%  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(show = 'always', hideOnMouseOut = T) %>%
                         dyRangeSelector(retainDateWindow = T) %>% dyAxis("x", valueRange = c(1, 200))
                     } else {
                       start = as.Date(rownames(tail(fit_data_zoom(),1))) 
                       end   = as.Date(rownames(tail(fit_data_zoom(),1))) + days(input$numdays) + 5 #5 extra days...
                       add_dates =  seq(from=start, to=end, by='day')
                       add_dates = add_dates[!weekdays(add_dates) %in% c('Saturday', 'Sunday')]
                       forecast_n = length(add_dates)
                       
                       forec = forecast(fit_model(), h = forecast_n)
                       fitted = as.data.frame(forec$fitted)
                       colnames(fitted) = c("Fitted")
                       rownames(fitted) = rownames(fit_data_zoom())
                       
                       #convert elements of time series FORECAST to dataframe for plotting
                       forecast = with(forec, data.frame(Predicted=forec$mean,
                                                         upper80=forec$upper[,1],
                                                         lower80=forec$lower[,1],
                                                         upper95=forec$upper[,2],
                                                         lower95=forec$lower[,2]))
                       # add_dates =  seq(as.Date(rownames(fit_data_zoom())[length(rownames(fit_data_zoom()))])+1,
                       #                  as.Date(rownames(fit_data_zoom())[length(rownames(fit_data_zoom()))])+2*forecast_n-1, 1)
                       # add_dates = add_dates[!weekdays(add_dates) %in% c('Saturday', 'Sunday')]
                       # add_dates = add_dates[1:forecast_n]
                       # rownames(forecast) = add_dates
                       
                       
                       # add_dates = add_dates[1:forecast_n]
                       rownames(forecast) = add_dates
                       
                       #construct DYGRAPH Visualisation
                       exisData = tryCatch( { xts( fit_data_pred(), order.by=as.POSIXct(rownames(fit_data_pred())) ) }, 
                                            error = function(e) {return (fit_data_pred())} )
                       fitted = xts( fitted, order.by=as.POSIXct(rownames(fitted)) )
                       forecast = xts(forecast, order.by=as.POSIXct(rownames(forecast)) )
                       all <- cbind(exisData, fitted, forecast)
                       
                       dygraph(all, stock()) %>%
                         dySeries(c("lower80", "Predicted", "upper80"), label = "80% ARIMA") %>%
                         dySeries(c("lower95", "Predicted", "upper95"), label = "95% ARIMA") %>%
                         dyLegend(show = 'always', hideOnMouseOut = T) %>%
                         dyRangeSelector(retainDateWindow = T)
                     }
                   })
    }, error = function(e) {
      return()
    })
  })
  
  # ARIMA ----
  
  output$resid = renderPlot({
    p1 = ggplot() + geom_point(aes(x = 1:dim(fit_data_zoom())[1],
                              y = residuals(fit_model())), alpha = 0.7) +
      geom_smooth(aes(x = 1:dim(fit_data_zoom())[1], y = residuals(fit_model()))) +
      ylab('Residuals') + xlab('period')
    p2 = ggplot(data = data.frame(res = residuals(fit_model())), aes(x = res)) +
      geom_histogram(aes(y = ..density..),
                     bins = 30,
                     col = 'red', alpha = 0.5, fill = 'red') +
      geom_density(aes(y = ..density..), col = 'blue') + xlab('Residuals') + coord_flip()
    grid.arrange(p1,p2,ncol=2)
  })
  
  output$accu = renderTable({
    accuracy(forecast(fit_model(), h = 50))
  })
  
  output$acf = renderPlot({
    if (input$diff == 0) {
      layout(matrix(1:2, ncol = 1), widths = 1, heights = c(3.0,4.5), respect = FALSE)
      par(mar = c(0, 4.1, 0.5, 2.1))
      pacf(fit_data_zoom(), main = '', axes = F, frame.plot=TRUE)
      Axis(side=2, labels=TRUE)
      par(mar = c(4.1, 4.1, 0, 2.1))
      Acf(fit_data_zoom(), main = '', axes = T, frame.plot=TRUE)
    }
    else {
      layout(matrix(1:2, ncol = 1), widths = 1, heights = c(3.0,4.5), respect = FALSE)
      par(mar = c(0, 4.1, 0.5, 2.1))
      pacf(diff(ts(fit_data_zoom()), differences = input$diff), main = '', axes = F, frame.plot=TRUE)
      Axis(side=2, labels=TRUE)
      par(mar = c(4.1, 4.1, 0, 2.1))
      Acf(diff(ts(fit_data_zoom()), differences = input$diff), main = '', axes = F, frame.plot=TRUE)
      Axis(side=2, labels=TRUE) 
    }
  })
  
  # Ranger ----
  
  listValidation = reactive({ validation_plots(input$numdays) })
  predTable = reactive ({ prediction_table(input$numdays) })
  
  output$validRanger = renderPlotly({
    withProgress(message = 'Plot in progress',
                 detail = '....', value = 0, {
                   tmp = listValidation()
                   ggplotly( tmp$validation_plot_RF )
                 })
  })
  
  output$validTextRanger = renderText({ tmp = listValidation()
    paste('Validation results between stock price gains and Random Forest prediction. Spearmans coefficient is ', 
          tmp$coeff_text_RF, sep = "") 
  })
  
  output$residRanger = renderPlotly({
    withProgress(message = 'Plot in progress',
                 detail = '....', value = 0, { tmp = listValidation()
                   ggplotly( tmp$residual_validation_plot_RF )
                 })
  })
  
  output$tableRanger =renderGvis({
    temp = predTable()
    temp = temp$RF
    gvisTable(temp, formats=list("Predicted gain [%]"="#.##"), options=list(page='enable'))
  })
  
  output$tableTextRanger <- renderValueBox({
    temp = predTable()
    temp = temp$RF
    if ( stock() %in% temp$Stock )  
      valueBox( 
        value = tags$p( paste0(temp[temp$Stock == stock(), "Predicted gain [%]"] %>% round(., 2), "%"), style = "font-size: 50%;" ),
        subtitle = tags$p( paste0("Gain/loss prediction ", input$numdays, " days from now for stock ", stock()) ),
        color = "light-blue"
      )
    else valueBox( 
      value = tags$p( "No information", style = "font-size: 50%;" ), 
      subtitle = tags$p( paste0("Gain/loss prediction ", input$numdays, " days from now for stock ", stock()) ),
      # icon = icon("list"),
      color = "light-blue"
    )
  })
  
  output$rankTextRanger <- renderValueBox({
    withProgress(message = 'Calculation in progress',
                 detail = '....', value = 0, {
                   temp = predTable()
                   temp = temp$RF
                   if ( stock() %in% temp$Stock )  
                     valueBox(
                       value = tags$p( paste0(round(100-100.*match(stock(), temp$Stock)/length(temp$Stock),1), "%"), style = "font-size: 50%;" ),
                       paste0("Rank among ", length(temp$Stock), " stocks with available information."), 
                       color = "light-blue"
                     )
                   else valueBox(
                     value = tags$p( "No information", style = "font-size: 50%;" ),
                     paste0("Rank among ", length(temp$Stock), " stocks with available information."), 
                     color = "light-blue"
                   )
                 })
  })
  
  # GBM ----

  output$validGbm = renderPlotly({
    withProgress(message = 'Plot in progress',
                 detail = '....', value = 0, { tmp = listValidation()
                   ggplotly( tmp$validation_plot_GBM )
                 })
  })
  
  output$validTextGbm = renderText({ tmp = listValidation()
    paste('Validation results between stock price gains and GBM prediction. Spearmans coefficient is ', 
          tmp$coeff_text_GBM, sep = "") 
  })
  
  output$residGbm = renderPlotly({ tmp = listValidation()
    withProgress(message = 'Plot in progress',
                 detail = '....', value = 0, {
                   ggplotly( tmp$residual_validation_plot_GBM )
                 })
  })
  
  output$tableGbm =renderGvis({
    temp = predTable()
    temp = temp$GBM
    gvisTable(temp, formats=list("Predicted gain [%]"="#.##"), options=list(page='enable'))
  })
  
  output$tableTextGbm <- renderValueBox({
    temp = predTable()
    temp = temp$GBM
    if ( stock() %in% temp$Stock )  
      valueBox( 
        value = tags$p( paste0(temp[temp$Stock == stock(), "Predicted gain [%]"] %>% round(., 2), "%"), style = "font-size: 50%;" ),
        subtitle = tags$p( paste0("Gain/loss prediction ", input$numdays, " days from now for stock ", stock()) ),
        color = "light-blue"
      )
    else valueBox( 
      value = tags$p( "No information", style = "font-size: 50%;" ), 
      subtitle = tags$p( paste0("Gain/loss prediction ", input$numdays, " days from now for stock ", stock()) ),
      # icon = icon("list"),
      color = "light-blue"
    )
  })
  
  output$rankTextGbm <- renderValueBox({
    withProgress(message = 'Calculation in progress',
                 detail = '....', value = 0, {
                   temp = predTable()
                   temp = temp$GBM
                   if ( stock() %in% temp$Stock )  
                     valueBox(
                       value = tags$p( paste0(round(100-100.*match(stock(), temp$Stock)/length(temp$Stock),1), "%"), style = "font-size: 50%;" ),
                       paste0("Rank among ", length(temp$Stock), " stocks with available information."), 
                       # icon = icon("list"),
                       color = "light-blue"
                     )
                   else valueBox(
                     value = tags$p( "No information", style = "font-size: 50%;" ),
                     paste0("Rank among ", length(temp$Stock), " stocks with available information."), 
                     # icon = icon("list"),
                     color = "light-blue"
                   )
                 })
  })
  
  # GLMNET ----
  
  output$validGlmnet = renderPlotly({ tmp = listValidation()
    withProgress(message = 'Plot in progress',
                 detail = '....', value = 0, {
                   ggplotly( tmp$validation_plot_GLMNET )
                 })
  })
  
  output$validTextGlmnet = renderText({ tmp = listValidation()
    paste('Validation results between stock price gains and GLMNET prediction. Spearmans coefficient is ', 
          tmp$coeff_text_GLMNET, sep = "") 
  })
  
  output$residGlmnet = renderPlotly({ tmp = listValidation()
    withProgress(message = 'Plot in progress',
                 detail = '....', value = 0, {
                   ggplotly( tmp$residual_validation_plot_GLMNET )
                 })
  })
  
  output$tableGlmnet =renderGvis({
    temp = predTable()
    temp = temp$GLMNET
    gvisTable(temp, formats=list("Predicted gain [%]"="#.##"), options=list(page='enable'))
  })
  
  output$tableTextGlmnet <- renderValueBox({
    temp = predTable()
    temp = temp$GLMNET
    if ( stock() %in% temp$Stock )  
      valueBox( 
        value = tags$p( paste0(temp[temp$Stock == stock(), "Predicted gain [%]"] %>% round(., 2), "%"), style = "font-size: 50%;" ),
        subtitle = tags$p( paste0("Gain/loss prediction ", input$numdays, " days from now for stock ", stock()) ),
        color = "light-blue"
      )
    else valueBox( 
      value = tags$p( "No information", style = "font-size: 50%;" ), 
      subtitle = tags$p( paste0("Gain/loss prediction ", input$numdays, " days from now for stock ", stock()) ),
      color = "light-blue"
    )
  })
  
  output$rankTextGlmnet <- renderValueBox({
    withProgress(message = 'Calculation in progress',
                 detail = '....', value = 0, {
                   temp = predTable()
                   temp = temp$GLMNET
                   if ( stock() %in% temp$Stock )  
                     valueBox(
                       value = tags$p( paste0(round(100-100.*match(stock(), temp$Stock)/length(temp$Stock),1), "%"), style = "font-size: 50%;" ),
                       paste0("Rank among ", length(temp$Stock), " stocks with available information."), 
                       color = "light-blue"
                     )
                   else valueBox(
                     value = tags$p( "No information", style = "font-size: 50%;" ),
                     paste0("Rank among ", length(temp$Stock), " stocks with available information."), 
                     color = "light-blue"
                   )
                 })
  })
  
  # -----------------------------
  # -----------------------------
  
  # ----------------------------- 
  # Media -----------------------
  # -----------------------------
  
  news.plot = reactive({
    withProgress(message = 'Downloading news.. .. may take a minute..',
                 detail = '....', value = 0, {
                   media.news.plot(stock(), stockFull())
                 })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$media.news <- renderUI({
    output = tagList()
    temp = news.plot()
    for (i in 1:min(10, dim(temp)[1]) ) { 
      output[[i]] = tagList()
      output[[i]][[1]] = temp$title[i]
      output[[i]][[2]] = a(temp$url[i], href=temp$url[i])  
      output[[i]][[3]] = tags$br()
    }
    output
    })
  
  #output$media.news = renderPlot({
  #  wordcloud_rep(words = news.plot()$word, freq = news.plot()$freq, scale = c(2.8, 0.3), random.order = F,
  #            min.freq = 1, max.words=60, colors=brewer.pal(8, "Dark2"), fixed.asp = T, use.r.layout = TRUE,
  #            rot.per = 0)
  #})
  
  tweets.plot = reactive({
    withProgress(message = 'Downloading tweets.. may take a minute..',
                 detail = '....', value = 0, {
                   media.tweets.plot(stock())
                 })
  })
  
  output$media.twitter = renderPlot({
  wordcloud(words = tweets.plot()$word, freq = tweets.plot()$freq, scale = c(3.0, 0.9), random.order = F,
            min.freq = 1, max.words=60, colors=brewer.pal(8, "Dark2"), fixed.asp = T, use.r.layout = TRUE,
            rot.per = 0)
  })
  
  # -----------------------------
  # -----------------------------
  
})