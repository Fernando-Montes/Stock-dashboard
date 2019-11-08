shinyUI(
  dashboardPage (#skin = "black",
    dashboardHeader(title = 'Stock market'),
    dashboardSidebar(
      sidebarMenu(
        menuItem('Price Movement Signals', tabName = 'sig', icon = icon('line-chart')),
        list ( menuItem('Forecast', tabName = 'for', icon = icon('area-chart')),
               tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: light-blue}")),
               #selectInput('numdays', 'Days into the future', c('5','10','30')) ),
               sliderTextInput(inputId = "numdays", label = "Days into the future:", grid = TRUE, force_edges = TRUE, choices = c('5','10','30')) ),
        menuItem('Indicators', tabName = 'ind', icon = icon('signal')),
        menuItem('Media', tabName = 'med', icon = icon('commenting'))
      ),
      selectizeInput('sector', h5('Sectors'),
                     choices = na.omit(c("All sectors", sort(unique( stockInfo$Sector )))),
                     selected = "All sectors"),
      selectizeInput('industry', h5('Industry'),
                     choices = na.omit(c("All industries", sort(unique( stockInfo$Industry )))),
                     selected = "All industries"),
      selectizeInput('price1', h5('Stock'),
                     choices = na.omit(sort(unique(stockInfo$FullName))),
                     options = list(maxOptions = 10000) ),
      conditionalPanel( condition = "input.sector == 'All sectors'",
                        fluidRow(
                          box( background = 'black', solidHeader = F, 'Sector' ),
                          box( background = 'black', solidHeader = F, textOutput('sectorText')  ) ) ),
      conditionalPanel( condition = "input.industry == 'All industries'",
                        fluidRow(
                          box( background = 'black', solidHeader = F, 'Industry' ),
                          box( background = 'black', solidHeader = F, textOutput('industryText')  ) ) ),
      sidebarMenu(
        menuItem('About', tabName = 'abo', icon = icon('question-circle'))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'sig',
                fluidRow(
                  box(title = 'Stock Price Movement', status = 'primary', solidHeader = T, width = 12, height = "410px", dygraphOutput('price', height = 350)) ),
                fluidRow( 
                  tabBox(title = tagList(shiny::icon("line-chart"), "Signals"), width = 12, height = "395px",
                         tabPanel('SMA',
                                  column( width = 1, checkboxInput("sma", "") ),
                                  column( width = 5, includeMarkdown("Info/sma.md") ),
                                  column( width = 6, conditionalPanel( condition = "input.sma == true", 
                                                                       box(title = 'SMA days', status = 'primary', solidHeader = T, 'Move bar to adjust time period',
                                                                           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                                                                           tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: purple}")),
                                                                           sliderInput('sma1', h5('SMA1'), min = 10, max = 200, value = 30),
                                                                           sliderInput('sma2', h5('SMA2'), min = 10, max = 200, value = 100)) ) )
                         ), 
                         tabPanel('RSI',
                                  column( width = 1, checkboxInput("rsi", "") ),
                                  column( width = 6, includeMarkdown("Info/rsi.md") ),
                                  column( width = 5, conditionalPanel( condition = "input.rsi == true",
                                                                       box(title = 'RSI days', status = 'primary', solidHeader = T, 'Move bar to adjust time period',
                                                                           tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: gray}")),
                                                                           sliderInput('rsi1', h5('RSI'), min = 5, max = 50, value = 20) ) ) )
                         )
                  )
                ) 
        ),
        tabItem(tabName = 'ind',
                fluidRow(
                  column( width = 3, height = "410px",
                          box(title = 'Indicator selection:', 
                              status = 'primary', solidHeader = T, height = "410px", width = 12,
                              selectizeInput('indict', '', 
                                             choices = c("price", "EV", "EBITDA", "EV.earning", "EV.EBITDA", 
                                                         "EV.book", "EV.revenue", "EV.cash", "EquityAssets.liability"),
                                             selected = 'price'),
                              textOutput('indText') 
                              )
                  ),
                  column( width = 9, height = "410px",
                          tabBox(title = tagList(shiny::icon("signal"), "Indicators"), height = "410px", width = 12,
                                 tabPanel('Tree map', 'Size indicates stock price and color indicates indicator value. 
                                              Green means low and red high indicator value.', htmlOutput('tree') 
                                 ), 
                                 tabPanel('Industry', 'Indicators for companies within the same industry.',
                                          plotlyOutput('bar', height = "325")
                                 ),
                                 tabPanel('Correlation', 'Stock price Pearsons correlation coefficients for companies within same industry as selected company.
                                      Correlation is calculated using stock prices from 2017-01-01 until now. A positive (negative) correlation factor
                                      indicates both prices tend to increase (decrease) in tandem.',
                                      plotlyOutput('corr', height = "285")
                                 )
                          )
                  )
                ),
                fluidRow( 
                  box(width = 12, 
                      column( width = 6, height = "410px",
                              selectInput('MLModel', label = h4("Forecast model indicator importance"), 
                                          choices = list("Random Forest", "Gradient Boosting", "Generalized Linear Model"), 
                                          selected = "Random Forest"),
                              box( htmlOutput('varImpTable'), width = 12 )
                      ),
                      column( width = 6, height = "410px",
                              dropdownButton( selectInput("indDisplay_x", label = h5("x-axis"), 
                                                          choices = IndList, selected = 'Stock price/max price last 6 months'),
                                              selectInput("indDisplay_y", label = h5("y-axis"), 
                                                          choices = IndList, selected = 'EV/EBITDA'),
                                              selectInput("indDisplay_c", label = h5("color"), 
                                                          choices = IndList, selected = 'Gain/loss Random Forest prediction [%]'),
                                              circle = FALSE, status = "status", icon = icon("gear"), size = 'default',
                                              label = 'Click to select indicators to display',
                                              tooltip = TRUE, up = TRUE ),
                              box(width = 12, plotlyOutput('indicPlot'), height = 480)
                      )
                  )
                )
        ),
        tabItem(tabName = 'for',
                fluidRow(
                  box(title = 'Stock Price Movement',status = 'primary', solidHeader = T, width = 12, height = "410px", 
                      dygraphOutput('forecastplot', height = 350)) ),
                fluidRow( 
                  tabBox(title = tagList(shiny::icon("area-chart"), "Time Series"), width = 12, 
                         tabPanel('ARIMA',
                                  fluidRow(
                                    column( width = 1, checkboxInput("ARIMA", "") ),
                                    column( width = 5, includeMarkdown("Info/ARIMA.md") ),
                                    column( width = 6, 
                                            conditionalPanel( condition = "input.ARIMA == true", 
                                                              box(title = 'ARIMA(p,d,q)', solidHeader = T, status = 'primary', width = 12,
                                                                  box(
                                                                    sliderInput('ar', 'Autoregression (p)', min = 0, max = 3, value = 1),
                                                                    sliderInput('diff', 'Differencing (d)', min = 0 , max = 3, value = 0) ),
                                                                  box (
                                                                    sliderInput('ma', 'Moving-average (q)', min = 0 , max = 3, value = 0),
                                                                    sliderInput("date_range", "Fit date range", 
                                                                                min = as.Date("2016-02-01"), max = Sys.Date(), 
                                                                                value = c(as.Date("2016-02-25"), Sys.Date()) ) )
                                                              )
                                            ) 
                                    )
                                  ),
                                  fluidRow(  
                                    conditionalPanel( condition = "input.ARIMA == true", 
                                                      box('Residuals between stock price and ARIMA fit', width = 6, plotOutput('resid'), height = 440),
                                                      box('Auto and partial correlation between time series and its own lag', width = 6, 
                                                          plotOutput('acf'), height = 440 ) 
                                    )
                                  ),
                                  fluidRow(
                                    conditionalPanel( condition = "input.ARIMA == true", 
                                                      box( 'Forecast accuracy: Mean Error (ME), Root Mean Squared Error (RMSE),
                                                            Mean Absolute Error (MAE), Mean Percentage Error (MPE), 
                                                            Mean Absolute Percentage Error (MAPE), Mean Absolute Scaled Error (MASE), 
                                                            Autocorrelation of errors at lag 1 (ACF1).', width = 12, tableOutput('accu') )
                                    )  
                                  )
                         ),
                         tabPanel('Random Forest',
                                  fluidRow(
                                    column( width = 1, checkboxInput("Ranger", "", value = TRUE) ),
                                    column( width = 3, includeMarkdown("Info/Ranger.md") ),
                                    column( width = 6, 
                                            conditionalPanel( condition = "input.Ranger == true", 
                                                              valueBoxOutput("tableTextRanger", width= 6), 
                                                              valueBoxOutput("rankTextRanger", width= 6)
                                            ) 
                                    )
                                  ),
                                  fluidRow(
                                    conditionalPanel( condition = "input.Ranger == true",
                                                      box( textOutput('validTextRanger'), width = 4,
                                                           plotlyOutput('validRanger'), height = 480),
                                                      box('Residuals between stock price gains and Random forest predition using the same time period used when
                                                          creating the model.',
                                                          width = 4, plotlyOutput('residRanger'), height = 480),
                                                      box( "Predicted returns for stocks with available information",
                                                             width = 4, htmlOutput('tableRanger') )
                                                      
                                    )
                                  )
                         ),
                         tabPanel('Gradient Boosting',
                                  fluidRow(
                                    column( width = 1, checkboxInput("gbm", "", value = TRUE) ),
                                    column( width = 3, includeMarkdown("Info/gbm.md") ),
                                    column( width = 6,
                                            conditionalPanel( condition = "input.gbm == true",
                                                              valueBoxOutput("tableTextGbm", width= 6),
                                                              valueBoxOutput("rankTextGbm", width= 6)
                                            )
                                    )
                                  ),
                                  fluidRow(
                                    conditionalPanel( condition = "input.gbm == true",
                                                      box( textOutput('validTextGbm'), width = 4,
                                                           plotlyOutput('validGbm'), height = 480),
                                                      box('Residuals between stock price gains and Random forest predition using the same time period used when
                                                          creating the model.',
                                                          width = 4, plotlyOutput('residGbm'), height = 480),
                                                      box( "Predicted returns for stocks with available information",
                                                           width = 4, htmlOutput('tableGbm') )
                                    )
                                  )
                         ),
                         tabPanel('Generalized Linear Model',
                                  fluidRow(
                                    column( width = 1, checkboxInput("glmnet", "", value = TRUE) ),
                                    column( width = 3, includeMarkdown("Info/glmnet.md") ),
                                    column( width = 6,
                                            conditionalPanel( condition = "input.glmnet == true",
                                                              valueBoxOutput("tableTextGlmnet", width= 6),
                                                              valueBoxOutput("rankTextGlmnet", width= 6)
                                            )
                                    )
                                  ),
                                  fluidRow(
                                    conditionalPanel( condition = "input.glmnet == true",
                                                      box( textOutput('validTextGlmnet'), width = 4,
                                                           plotlyOutput('validGlmnet'), height = 480),
                                                      box('Residuals between stock price gains and Random forest predition using the same time period used when
                                                          creating the model.',
                                                          width = 4, plotlyOutput('residGlmnet'), height = 480),
                                                      box( "Predicted returns for stocks with available information",
                                                           width = 4, htmlOutput('tableGlmnet') )
                                    )
                                  )
                         )
                  ) 
                )
        ),
        tabItem(tabName = 'med', uiOutput("BoxPlaceholder"),
                fluidRow(
                  box(title = 'Recent news', solidHeader = T, status = 'primary', width = 7, uiOutput('media.news')),
                  box(title = 'Words in recent tweets', solidHeader = T, status = 'primary', width = 5, plotOutput('media.twitter', height = '500px'))
                )
        ),
        tabItem( tabName = 'abo', includeMarkdown("Info/About.md") )
      )
    )
  )
)
    