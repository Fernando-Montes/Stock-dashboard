# Loading functions to be used ----------------------------------------

targetPath <- "~/Dropbox/Courses/R/StockModel-2/ArchiveFin/"
# Load data frame with Sector.Name, Sector.Num, Industry.Name, Industry.Num into listAll
load(file = "~/Dropbox/Courses/R/StockModel-2/SectorIndustryInfo.RData")
# Load stock, sector and industry information into StockInfoAll
load(file = "~/Dropbox/Courses/R/StockModel-2/ArchiveFin/StockInfo.RData")

# Function that returns data frame with the most recent indicators (within 90 days )
# for all stocks within the entity (depending on flag) an input stock it belongs to
# If flag = 0, all stocks for all sectors and industries
# If flag = 1, all stocks within a given sector
# If flag = 2, all stocks within a given industry
date.today = Sys.Date()                   # today's date
indict_b_sto_func = function(stock, flag) {

  date.file = most_recent_date() # Date that has the most recent files
  fileName <- paste(targetPath, date.file, "+5d.Rdata", sep = "") 
  load(file = fileName)
  # Creating vector of stocks in the given industry
  if (flag == 0) {
    ListStocks = as.character(stockInfo$Stock.SYM)
  } else {
    # Finding industry number and name that stock belongs to
    industryNum = stockInfo[stockInfo$Stock.SYM == stock,]$Industry.Num[1]
    industryName = as.character(listAll[listAll$Industry.Num == industryNum,]$Industry.Name[1])
    # Finding sector number and name that stock belongs to
    sectorNum = listAll[listAll$Industry.Num == industryNum,]$Sector.Num[1]
    sectorName = as.character(listAll[listAll$Sector.Num == sectorNum,]$Sector.Name[1])
    if (flag == 1) {
      ListStocks = as.character(stockInfo[stockInfo$Sector.Num == sectorNum,]$Stock.SYM)
    } else ListStocks = as.character(stockInfo[stockInfo$Industry.Num == industryNum,]$Stock.SYM)
  }
  
  # Preparing data frame with stock indicators
  industryStocks = data.frame(SYM = character(0),                  # Stock symbol
                              price = numeric(0),                  # Stock price
                              EV = numeric(0),                     # Enterprise value = stock price * number of shares
                              EBITDA = numeric(0),                 # EBITDA
                              EV.earning = numeric(0),             # Enterprise value / earnings
                              EV.EBITDA = numeric(0),              # Enterprise value / EBITDA
                              EV.book = numeric(0),                # Enterprise value / book value
                              EV.revenue = numeric(0),             # Enterprise value / revenue
                              EV.cash = numeric(0),                # Enterprise value / cash
                              EquityAssets.liability = numeric(0), # Equity + Assets / Liability
                              sector = character(0),               # Sector name
                              industry = character(0),             # Industry name
                              stringsAsFactors=FALSE
  )
  
  for(i in 1:length(ListStocks)) {      # Adding info for each stock 
    # print(i)
    # Adding name of each stock 
    industryStocks[i,"SYM"] = ListStocks[i] #---
    # Adding industry of the stock
    industryStocks[i,"industry"] = ifelse( flag == 2, industryName, 
        as.character(listAll[listAll$Industry.Num == stockInfo[stockInfo$Stock.SYM == ListStocks[i],]$Industry.Num[1],]$Industry.Name[1]) ) #---
    # Adding sector of the stock
    industryStocks[i,"sector"] = ifelse( flag > 0, sectorName,
       as.character(listAll[listAll$Industry.Name == industryStocks[i,"industry"],]$Sector.Name[1]) ) #---
    # Checking that the indicator file was created within the last 20 days   
    if ( date.today - date.file < 20  ) {
      temp = table.model[table.model$Stock.SYM == ListStocks[i], ]
      temp = temp[1,]
      if ( dim(temp)[1]>0 ) {  # If the indicators exist in table.model
        # Stock price 
        industryStocks[i,"price"] = temp$Price.Model.end #---
        # Enterprise value
        industryStocks[i,"EV"] = temp$Ev #---
        # EBITDA = (net income + interest income + income before tax - income after tax  
        #           + depreciation/amortization + unusual expense ) 
        industryStocks[i,"EBITDA"] = ifelse(temp$Ev.ebitda!=0, temp$Ev/temp$Ev.ebitda, NA) #---
        # Ev/earning = price/diluted normalized EPS  
        industryStocks[i,"EV.earning"] = temp$Ev.earning #---
        # Ev/ebitda = EV/(net income + interest income + income before tax - income after tax 
        #                + depreciation/amortization + unusual expense) 
        industryStocks[i,"EV.EBITDA"] = temp$Ev.ebitda #---
        # Ev/book = EV/total equity 
        industryStocks[i,"EV.book"] = temp$Ev.book #---
        # Ev/revenue = EV/Total Revenue 
        industryStocks[i,"EV.revenue"] = temp$Ev.revenue #---
        # Ev/cash = EV/Cash and Short Term Investments 
        industryStocks[i,"EV.cash"] = temp$Ev.cash #---
        # Price.equity.debt = price/Total Equity/Total Debt 
        industryStocks[i,"EquityAssets.liability"] = temp$EquityAssets.liability #---
      }
    }
  }
  industryStocks = na.exclude(industryStocks)
  return (industryStocks)  
}

# Function that returns data frame with indicators for all industries selected
# data frame industryStocks (created by function indict_w_sec_func)
indict_b_ind_func = function(industryStocks, stock, flag) {
  
  # Creating vector of stocks in the given industry
  # Finding industry number and name that stock belongs to
  industryNum = stockInfo[stockInfo$Stock.SYM == stock,]$Industry.Num[1]
  industryName = as.character(listAll[listAll$Industry.Num == industryNum,]$Industry.Name[1])
  # Finding sector number and name that stock belongs to
  sectorNum = listAll[listAll$Industry.Num == industryNum,]$Sector.Num[1]
  sectorName = as.character(listAll[listAll$Sector.Num == sectorNum,]$Sector.Name[1])
  
  # Preparing data frame with stock indicators
  industryIndict = data.frame(SYM = character(0),                  # Stock symbol
                              price = numeric(0),                  # Stock price
                              EV = numeric(0),                     # Enterprise value = stock price * number of shares
                              EBITDA = numeric(0),                 # EBITDA
                              EV.earning = numeric(0),             # Enterprise value / earnings
                              EV.EBITDA = numeric(0),              # Enterprise value / EBITDA
                              EV.book = numeric(0),                # Enterprise value / book value
                              EV.revenue = numeric(0),             # Enterprise value / revenue
                              EV.cash = numeric(0),                # Enterprise value / cash
                              EquityAssets.liability = numeric(0), # Equity + Assets / Liability
                              sector = character(0),               # Sector name
                              industry = character(0),             # Industry name
                              stringsAsFactors=FALSE
  )
  
  industryList = unique(industryStocks$industry)
  
  for(i in 1:length(industryList)) {      # Adding info for each industry
    # Adding name of each industry
    industryIndict[i,"SYM"] = as.character(industryList[i]) #---
    # Adding "industry" that is now the sector
    industryIndict[i,"industry"] = ifelse ( flag > 0, sectorName, 
                                            as.character(listAll[listAll$Industry.Name == as.character(industryList[i]),]$Sector.Name[1]) ) #---
    # Adding sector of the stock
    industryIndict[i,"sector"] = industryIndict[i,"industry"]
    # Adding average stock price 
    industryIndict[i,"price"] = mean(industryStocks[industryStocks$industry == industryList[i], "price"], na.rm = T) #---
    # Enterprise value
    industryIndict[i,"EV"] = mean(industryStocks[industryStocks$industry == industryList[i], "EV"], na.rm = T) #---
    # EBITDA = (net income + interest income + income before tax - income after tax 
    #          + depreciation/amortization + unusual expense) 
    industryIndict[i,"EBITDA"] = mean(industryStocks[industryStocks$industry == industryList[i], "EBITDA"], na.rm = T) #---  
    # Ev/earning = price/diluted normalized EPS 
    industryIndict[i,"EV.earning"] = mean(industryStocks[industryStocks$industry == industryList[i], "EV.earning"], na.rm = T) #---
    # Ev/ebitda = EV/(net income + interest income + income before tax - income after tax 
    #                + depreciation/amortization + unusual expense) 
    industryIndict[i,"EV.EBITDA"] = mean(industryStocks[industryStocks$industry == industryList[i], "EV.EBITDA"], na.rm = T) #---
    # Ev/book = EV/total equity 
    industryIndict[i,"EV.book"] = mean(industryStocks[industryStocks$industry == industryList[i], "EV.book"], na.rm = T) #---
    # Ev/revenue = EV/Total Revenue 
    industryIndict[i,"EV.revenue"] = mean(industryStocks[industryStocks$industry == industryList[i], "EV.revenue"], na.rm = T) #---
    # Ev/cash = EV/Cash and Short Term Investments 
    industryIndict[i,"EV.cash"] = mean(industryStocks[industryStocks$industry == industryList[i], "EV.cash"], na.rm = T) #---
    # Price.equity.debt = price/Total Equity/Total Debt 
    industryIndict[i,"EquityAssets.liability"] = mean(industryStocks[industryStocks$industry == industryList[i], "EquityAssets.liability"], na.rm = T) #---
  }
  return (industryIndict)  
}

# Function that returns data frame with indicators for all sectors selected
# data frame industryStocks (created by function indict_w_sec_func)
indict_b_sec_func = function(industryStocks) {
  
  # Preparing data frame with stock indicators
  sectorIndict = data.frame(SYM = character(0),                  # Stock symbol
                            price = numeric(0),                  # Stock price
                            EV = numeric(0),                     # Enterprise value = stock price * number of shares
                            EBITDA = numeric(0),                 # EBITDA
                            EV.earning = numeric(0),             # Enterprise value / earnings
                            EV.EBITDA = numeric(0),              # Enterprise value / EBITDA
                            EV.book = numeric(0),                # Enterprise value / book value
                            EV.revenue = numeric(0),             # Enterprise value / revenue
                            EV.cash = numeric(0),                # Enterprise value / cash
                            EquityAssets.liability = numeric(0), # Equity + Assets / Liability
                            sector = character(0),               # Sector name
                            industry = character(0),             # Industry name
                            stringsAsFactors=FALSE
  )
  
  sectorList = unique(industryStocks$sector)
  
  for(i in 1:length(sectorList)) {      # Adding info for each sector
    # Adding name of each sector
    sectorIndict[i,"SYM"] = as.character(sectorList[i]) #---
    # Adding "industry" which is now a global category
    sectorIndict[i,"industry"] = "All selected" #---
    # Adding sector
    sectorIndict[i,"sector"] = as.character(sectorList[i]) #---
    # Adding average stock price 
    sectorIndict[i,"price"] = mean(industryStocks[industryStocks$sector == sectorList[i], "price"], na.rm = T) #---
    # Enterprise value
    sectorIndict[i,"EV"] = mean(industryStocks[industryStocks$sector == sectorList[i], "EV"], na.rm = T) #---
    # EBITDA = (net income + interest income + income before tax - income after tax 
    #          + depreciation/amortization + unusual expense) 
    sectorIndict[i,"EBITDA"] = mean(industryStocks[industryStocks$sector == sectorList[i], "EBITDA"], na.rm = T) #---  
    # Ev/earning = price/diluted normalized EPS 
    sectorIndict[i,"EV.earning"] = mean(industryStocks[industryStocks$sector == sectorList[i], "EV.earning"], na.rm = T) #---
    # Ev/ebitda = EV/(net income + interest income + income before tax - income after tax 
    #                + depreciation/amortization + unusual expense) 
    sectorIndict[i,"EV.EBITDA"] = mean(industryStocks[industryStocks$sector == sectorList[i], "EV.EBITDA"], na.rm = T) #---
    # Ev/book = EV/total equity 
    sectorIndict[i,"EV.book"] = mean(industryStocks[industryStocks$sector == sectorList[i], "EV.book"], na.rm = T) #---
    # Ev/revenue = EV/Total Revenue 
    sectorIndict[i,"EV.revenue"] = mean(industryStocks[industryStocks$sector == sectorList[i], "EV.revenue"], na.rm = T) #---
    # Ev/cash = EV/Cash and Short Term Investments 
    sectorIndict[i,"EV.cash"] = mean(industryStocks[industryStocks$sector == sectorList[i], "EV.cash"], na.rm = T) #---
    # Price.equity.debt = price/Total Equity/Total Debt 
    sectorIndict[i,"EquityAssets.liability"] = mean(industryStocks[industryStocks$sector == sectorList[i], "EquityAssets.liability"], na.rm = T) #---
  }
  return (sectorIndict)  
}

# Function that returns data frame with indicators for an average of all stocks within the 
# entity (depending on flag) an input stock it belongs to
indict_ave_func = function(industryStocks) {
  
  # Preparing data frame with stock indicators
  allIndict = data.frame(SYM = character(0),                  # Stock symbol
                         price = numeric(0),                  # Stock price
                         EV = numeric(0),                     # Enterprise value = stock price * number of shares
                         EBITDA = numeric(0),                 # EBITDA
                         EV.earning = numeric(0),             # Enterprise value / earnings
                         EV.EBITDA = numeric(0),              # Enterprise value / EBITDA
                         EV.book = numeric(0),                # Enterprise value / book value
                         EV.revenue = numeric(0),             # Enterprise value / revenue
                         EV.cash = numeric(0),                # Enterprise value / cash
                         EquityAssets.liability = numeric(0), # Equity + Assets / Liability
                         sector = character(0),               # Sector name
                         industry = character(0),             # Industry name
                         stringsAsFactors=FALSE
  )

  # Adding name of each sector
  allIndict[1,"SYM"] = "All selected" #---
  # ignore industry
  # ignore sector
  # Adding average stock price 
  allIndict[1,"price"] = mean(industryStocks$price, na.rm = T) #---
  # Enterprise value
  allIndict[1,"EV"] = mean(industryStocks$EV, na.rm = T) #---
  # EBITDA = (net income + interest income + income before tax - income after tax 
  #          + depreciation/amortization + unusual expense) 
  allIndict[1,"EBITDA"] = mean(industryStocks$EBITDA, na.rm = T) #---  
  # Ev/earning = price/diluted normalized EPS 
  allIndict[1,"EV.earning"] = mean(industryStocks$EV.earning, na.rm = T) #---
  # Ev/ebitda = EV/(net income + interest income + income before tax - income after tax 
  #                + depreciation/amortization + unusual expense) 
  allIndict[1,"EV.EBITDA"] = mean(industryStocks$EV.EBITDA, na.rm = T) #---
  # Ev/book = EV/total equity 
  allIndict[1,"EV.book"] = mean(industryStocks$EV.book, na.rm = T) #---
  # Ev/revenue = EV/Total Revenue 
  allIndict[1,"EV.revenue"] = mean(industryStocks$EV.revenue, na.rm = T) #---
  # Ev/cash = EV/Cash and Short Term Investments 
  allIndict[1,"EV.cash"] = mean(industryStocks$EV.cash, na.rm = T) #---
  # Price.equity.debt = price/Total Equity/Total Debt 
  allIndict[1,"EquityAssets.liability"] = mean(industryStocks$EquityAssets.liability, na.rm = T) #---
  
  return (allIndict)  
}

# Function that given a stock returns a data frame with dates as rows and stock prices for all 
# companies within the same industry as columns
sto_ind_func = function(stock, updateProgress = NULL) {
  # Finding industry number and name that stock belongs to
  industryNum = stockInfo[stockInfo$Stock.SYM == stock,]$Industry.Num[1]
  industryName = as.character(listAll[listAll$Industry.Num == industryNum,]$Industry.Name[1])
  ListStocks = as.character(stockInfo[stockInfo$Industry.Num == industryNum,]$Stock.SYM)
  
  # Preparing data frame 
  matrix_prices = data.frame(Date = as.Date(character()),       # Date
                             Close = numeric(0),                # Stock price
                             SYM = character(0),                # Stock symbol
                             stringsAsFactors=FALSE
  )
  
  for(i in 1:length(ListStocks)) {      # Adding info for each stock 
    if (is.function(updateProgress)) {    # Call function to update progress
      text = paste0("Step ", i, " out of ", length(ListStocks))
      updateProgress(detail = text)
    }
    
    # Loading historical stock price data into SYMB_prices
    fileName <- paste(targetPath, ListStocks[i], "-prices.RData", sep="")
    if ( class(try(load(file = fileName), silent = TRUE)) != "try-error" ) { 
      temp = SYMB_prices
      temp = cbind(index(temp), as.data.frame(temp), ListStocks[i])
      colnames(temp) = c("Date", "Open", "High", "Low", "Close", "SYM")
      temp = temp[as.Date(temp$Date) > as.Date("2017-01-01"),] # Consider only prices after 2017-01-01
      if( dim(temp)[1]>100 ) matrix_prices = rbind( matrix_prices, temp )  # At least 100 observations have to exist
    }
  }
  row.names(matrix_prices) = NULL
  matrix_prices = matrix_prices %>% na.omit(.) %>% unique(.) 
  
  return (matrix_prices)  
}

# Return validation model plots
validation_plots = function(i) { # i is the number of days back used in the model
  date.file = most_recent_date() # Date that has the most recent files
  load(file = paste(targetPath, date.file, "-", i, "d-validation.Rdata", sep = ""))
  temp <- subset(my_val, select=c("actual.win.loss", "ranger_pred", "gbm_pred", "glmnet_pred"))
  colnames(temp) = c("actual", "RF", "GBM", "GLMNET")
  p = ggplot(temp, aes(x=RF, y=actual)) + geom_point(size = 0.3) + xlab("Predicted gain [%]") + ylab("Actual gain [%]")
  coeff = cor( x=temp$RF, y=temp$actual, method = "spearman") %>% round(., 3)  
  res_val = ggplot(data = data.frame(res = temp$actual - temp$RF), aes(x = res)) +
                                  geom_histogram(aes(y = ..density..), binwidth=0.5, col = 'red', alpha = 0.5, fill = 'red') +
                                  geom_density(aes(y = ..density..), col = 'blue') + xlab('Actual - Predicted') + coord_flip()
  resList = list('validation_plot_RF' = p + stat_density_2d(), 'coeff_text_RF' = coeff, 'residual_validation_plot_RF' = res_val)
  p = ggplot(temp, aes(x=GBM, y=actual)) + geom_point(size = 0.3) + xlab("Predicted gain [%]") + ylab("Actual gain [%]")
  coeff = cor( x=temp$GBM, y=temp$actual, method = "spearman") %>% round(., 3)  
  res_val = ggplot(data = data.frame(res = temp$actual - temp$GBM), aes(x = res)) +
    geom_histogram(aes(y = ..density..), binwidth=0.5, col = 'red', alpha = 0.5, fill = 'red') +
    geom_density(aes(y = ..density..), col = 'blue') + xlab('Actual - Predicted') + coord_flip()
  resList = c(resList, 
              list('validation_plot_GBM' = p + stat_density_2d(), 'coeff_text_GBM' = coeff, 'residual_validation_plot_GBM' = res_val))
  p = ggplot(temp, aes(x=GLMNET, y=actual)) + geom_point(size = 0.3) + xlab("Predicted gain [%]") + ylab("Actual gain [%]")
  coeff = cor( x=temp$GLMNET, y=temp$actual, method = "spearman") %>% round(., 3)  
  res_val = ggplot(data = data.frame(res = temp$actual - temp$GLMNET), aes(x = res)) +
    geom_histogram(aes(y = ..density..), binwidth=0.5, col = 'red', alpha = 0.5, fill = 'red') +
    geom_density(aes(y = ..density..), col = 'blue') + xlab('Actual - Predicted') + coord_flip()
  resList = c(resList, 
              list('validation_plot_GLMNET' = p + stat_density_2d(), 'coeff_text_GLMNET' = coeff, 'residual_validation_plot_GLMNET' = res_val))
  return (resList)
}

# Function that returns a stock table ranked according to the prediction in i number of
# days using MLmodel
prediction_table = function(i) {
  date.file = most_recent_date() # Date that has the most recent files
  load(file = paste(targetPath, date.file = most_recent_date(), "+", i, "d-pred.Rdata", sep = ""))
  RF = paste("ranger_pred_",i, sep='')
  GBM = paste("gbm_pred_",i, sep='')
  GLMNET = paste("glmnet_pred_",i, sep='')
  temp <- subset(table.pred, select=c("Stock.SYM", RF, GBM, GLMNET ))
  # Creating table with rankings from the different methods
  temp_RF <- subset(temp, select=c("Stock.SYM", RF))
  temp_RF <- temp_RF[order(temp_RF[, RF], decreasing = TRUE),] #Ranger
  colnames(temp_RF) = c("Stock", "Predicted gain [%]")
  temp_GBM <- subset(temp, select=c("Stock.SYM", GBM))
  temp_GBM <- temp_GBM[order(temp_GBM[, GBM], decreasing = TRUE),] #Ranger
  colnames(temp_GBM) = c("Stock", "Predicted gain [%]")
  temp_GLMNET <- subset(temp, select=c("Stock.SYM", GLMNET))
  temp_GLMNET <- temp_GLMNET[order(temp_GLMNET[, GLMNET], decreasing = TRUE),] #Ranger
  colnames(temp_GLMNET) = c("Stock", "Predicted gain [%]")
  resList = list("RF" = temp_RF, "GBM" = temp_GBM, "GLMNET" = temp_GLMNET)
  return ( resList )
}

# Function that returns the most recent previous date (within 10 days) of today's that has 
# quaterly financial data information
most_recent_date = function() {
  # Loading the most recent indicator table table.model
  temp = list.files(targetPath, pattern = "2019-*") # All the files that may contain indicator information
  diffDate = 20   # Obtain the most recent date less than 10 days
  for (i in 1:length(temp) ) {
    if( length(strsplit(temp[i],"")[[1]])==19 | length(strsplit(temp[i],"")[[1]])==20) { # Correct filename length 
      tempDate = as.Date(substr(temp[i],1,10)) # Extract date file was created
      if (date.today - tempDate < diffDate) { # Obtain the most recent date less than 20 days
        diffDate = date.today - tempDate 
        date.file = tempDate     
      }
    }
  }
  return(date.file)
}

# Function that returns a stock's prediction in i number of days
prediction_table_stock = function(numDays, stock) {
  stockReturns = data.frame(date.pred.made = as.Date(character()),        # date prediction was made
                            date.predicted = as.Date(character()),        # date where return is predicted
                            return_RF = numeric(0),                       # return ranger
                            return_GBM = numeric(0),                      # return gbm
                            return_GLMNET = numeric(0),                   # return glmnet
                            stringsAsFactors=FALSE
  )
  # Loading the most recent indicator table table.model
  temp = list.files( targetPath, pattern = paste("*", numDays, "d-pred.Rdata", sep = "") ) # Files that contain prediction information
  for (i in 1:length(temp) ) {
    if( length(strsplit(temp[i],"")[[1]])==24 | length(strsplit(temp[i],"")[[1]])==25 ) { # Correct filename length 
      date.file = as.Date(substr(temp[i],1,10)) # Extract date file was created
      load(file = paste(targetPath, date.file, "+", numDays, "d-pred.Rdata", sep = ""))
      RF = paste("ranger_pred_", numDays, sep='')
      GBM = paste("gbm_pred_", numDays, sep='')
      GLMNET = paste("glmnet_pred_", numDays, sep='')
      if ( stock %in% table.pred$Stock.SYM ) {
        stockReturns[i, "date.pred.made"] = as.Date(date.file)
        stockReturns[i, "date.predicted"] = as.Date(date.file) + days(numDays)
        stockReturns[i, "return_RF"] = table.pred[table.pred$Stock.SYM == stock, "Price.Model.end"] * (1 + 
                    table.pred[table.pred$Stock.SYM == stock, RF]/100.) 
        stockReturns[i, "return_GBM"] = table.pred[table.pred$Stock.SYM == stock, "Price.Model.end"] * (1 + 
                    table.pred[table.pred$Stock.SYM == stock, GBM]/100.) 
        stockReturns[i, "return_GLMNET"] = table.pred[table.pred$Stock.SYM == stock, "Price.Model.end"] * (1 + 
                    table.pred[table.pred$Stock.SYM == stock, GLMNET]/100.) 
      }
    }
  }
  stockReturns = na.exclude(stockReturns)
  stockReturns = stockReturns %>% distinct(., date.predicted, .keep_all = TRUE)
  return (stockReturns)
}

# Function that returns plots of important variables for a given model
indicatorXYPlot = function(stock, numDays, x, y, c) {
  date.file = most_recent_date() # Date that has the most recent files
  load(file = paste(targetPath, date.file, "+", numDays, "d-pred.Rdata", sep = ""))
  names = c('Stock symbol' = 'Stock.SYM',  
            'Price category [<1, <10, <100, >100]' = "Price.Category",                          
            'EV/earnings' = "Ev.earning",  'EV/EBITDA' = "Ev.ebitda",  'EV/Book value' = "Ev.book", 'EV/revenue' = "Ev.revenue",                  
            'EV/total cash' = "Ev.cash",  '(Equity + Assets)/Liabilities' = "EquityAssets.liability", 
            'Relative Strength Index last 10 days' = "rsi.10",  'Relative Strength Index last 50 days' = "rsi.50", 'DVO' = "dvo", 
            'Number of days from the most recent quarterly data available' = "timeDiffFin",
            'Sector number' = "SectorIndustry.Num", 'Stock price/min price last 6 months' = "Price.Model.end.low.ratio",
            'Stock price/max price last 6 months' = "Price.Model.end.high.ratio", 
            'Gain/loss Holt-Winters prediction [%]' = "predicted.hw.win.loss",     
            'Low 90% bound gain/loss Holt-Winters prediction [%]' = "predicted.hwLB.win.loss", 
            'Gain/loss ARIMA prediction [%]' = "predicted.arima.win.loss",
            'Stock price/ 200-day-average' = "Price.sma.200", 'Stock price/50-day-average' = "Price.sma.50",                
            'EV/earnings compared to sector average' = "Ev.earning.peers", 'EV/EBITDA compared to sector average' = "Ev.ebitda.peers",
            'EV/Book value compared to sector average' = "Ev.book.peers", 'EV/revenue compared to sector average' = "Ev.revenue.peers",            
            'EV/total cash compared to sector average' = "Ev.cash.peers", 
            '(Equity + Assets)/Liabilities compared to sector average' = "EquityAssets.liability.peers", 
            'Stock price/ 200-day-average compared to sector average' = "Price.sma.200.peers",  
            'Stock price/ 50-day-average compared to sector average' = "Price.sma.50.peers",           
            'Earnings compared to 1 year earlier' = "earning.histo", 'EBITDA compared to 1 year earlier' = "ebitda.histo",
            'book value compared to 1 year earlier' = "book.histo", 'Revenue compared to 1 year earlier' = "revenue.histo",              
            'Cash compared to 1 year earlier' = "cash.histo", 
            '(Equity + Assets)/Liabilities compared to 1 year earlier' = "equityAssets.liability.histo", 
            'Gain/loss Random Forest prediction [%]' = paste("ranger_pred_", numDays, sep = ""), 
            'Gain/loss GBM prediction [%]' = paste("gbm_pred_", numDays, sep = ""),
            'Gain/loss GLMNET prediction [%]' = paste("glmnet_pred_", numDays, sep = "") 
            )
  data <- data.frame(table.pred['Stock.SYM'], table.pred[names[x]], table.pred[names[y]], table.pred[names[c]])
  colnames(data) = c("Stock", "x", "y", "color")
  XYplot =  if ( stock %in% data['Stock'][[1]] ) {
    plot_ly(data, x = ~x, y = ~y, color = ~color, type = 'scatter', mode = 'markers', 
                   text = ~paste("Stock: ", Stock, '<br>Pred [%]:', sprintf( '%.2f', color) )) %>% 
    add_trace( x = data[data$Stock == stock, 2], y = data[data$Stock == stock,3], color = data[data$Stock == stock, 4], 
               text = ~paste("Stock: ", stock, '<br>Pred [%]:', sprintf( '%.2f', color) ),
               marker = list( size = 20, line = list( color = 'rgb(1, 1, 1)', width = 2)),
               size = 30, showlegend = FALSE) %>% 
      layout(xaxis = list(title = x, hoverformat = '.2f'), yaxis = list(title = y, hoverformat = '.2f'), 
             title = c, font = list(size = 10)) %>% colorbar(title = ' ') 
  } else {
    plot_ly(data, x = ~x, y = ~y, color = ~color, type = 'scatter', mode = 'markers', 
            text = ~paste("Stock: ", Stock, '<br>Pred [%]:', sprintf( '%.2f', color) )) %>% 
      layout(xaxis = list(title = x, hoverformat = '.2f'), yaxis = list(title = y, hoverformat = '.2f'), 
             title = paste(c, '<br>No info selected stock'), font = list(size = 9)) %>% colorbar(title = ' ') 
  }
  return(XYplot)
}


varImpPlot = function(MLmodel, numDays) {
  names = list('Stock.SYM' = 'Stock symbol',  
            "Price.Category" = 'Price category [<1, <10, <100, >100]',                          
            "Ev.earning" = 'EV/earnings',  "Ev.ebitda" = 'EV/EBITDA',  "Ev.book" = 'EV/Book value', "Ev.revenue" = 'EV/revenue',                  
            "Ev.cash" = 'EV/total cash',  "EquityAssets.liability" = '(Equity + Assets)/Liabilities', 
            "rsi.10" = 'Relative Strength Index last 10 days',  "rsi.50" = 'Relative Strength Index last 50 days', 'dvo' = 'DVO', 
            "timeDiffFin" = 'Number of days from the most recent quarterly data available',
            "SectorIndustry.Num" = 'Sector number', "Price.Model.end.low.ratio" = 'Stock price/min price last 6 months',
            "Price.Model.end.high.ratio" = 'Stock price/max price last 6 months', 
            "predicted.hw.win.loss" = 'Gain/loss Holt-Winters prediction [%]',     
            "predicted.hwLB.win.loss" = 'Low 90% bound gain/loss Holt-Winters prediction [%]', 
            "predicted.arima.win.loss" = 'Gain/loss ARIMA prediction [%]',
            "Price.sma.200" = 'Stock price/ 200-day-average', "Price.sma.50" = 'Stock price/50-day-average',                
            "Ev.earning.peers" = 'EV/earnings compared to sector average', "Ev.ebitda.peers" = 'EV/EBITDA compared to sector average',
            "Ev.book.peers" = 'EV/Book value compared to sector average', "Ev.revenue.peers" = 'EV/revenue compared to sector average',            
            "Ev.cash.peers" = 'EV/total cash compared to sector average', 
            "EquityAssets.liability.peers" = '(Equity + Assets)/Liabilities compared to sector average', 
            "Price.sma.200.peers" = 'Stock price/ 200-day-average compared to sector average',  
            "Price.sma.50.peers" = 'Stock price/ 50-day-average compared to sector average',           
            "earning.histo" = 'Earnings compared to 1 year earlier', "ebitda.histo" = 'EBITDA compared to 1 year earlier',
            "book.histo" = 'book value compared to 1 year earlier', "revenue.histo" = 'Revenue compared to 1 year earlier',              
            "cash.histo" = 'Cash compared to 1 year earlier', 
            "equityAssets.liability.histo" = '(Equity + Assets)/Liabilities compared to 1 year earlier'
  )
  date.file = most_recent_date() # Date that has the most recent files
  load(file = paste(targetPath, date.file, "-", numDays, "d-varImp.Rdata", sep = ""))
  MLmodelSel = c("Random Forest" = 1, "Gradient Boosting" = 2, "Generalized Linear Model" = 3)
  tmp = variableImportance[[MLmodelSel[MLmodel]]]$importance
  col1tmp = rownames(tmp)[order(tmp$Overall, decreasing = TRUE)]
  col1 = c()
  for (i in 1:length(col1tmp) ) {
    col1 = append( col1, names[col1tmp[i]][[1]] )
  }
  col2 = 100*tmp[col1tmp,]/sum(tmp[col1tmp,])
  df = tibble(col1, col2)
  colnames(df) = c("Indicator", "Importance [%]")
  return( df )
}

IndList = list('Price category [<1, <10, <100, >100]', 'EV/earnings',  'EV/EBITDA',  'EV/Book value', 
               'EV/revenue', 'EV/total cash',  '(Equity + Assets)/Liabilities', 
               'Relative Strength Index last 10 days', 'Relative Strength Index last 50 days',  'DVO', 
               'Number of days from the most recent quarterly data available', 'Sector number', 
               'Stock price/min price last 6 months', 'Stock price/max price last 6 months', 
               'Gain/loss Holt-Winters prediction [%]',  'Low 90% bound gain/loss Holt-Winters prediction [%]', 
               'Gain/loss ARIMA prediction [%]', 'Stock price/ 200-day-average', 'Stock price/50-day-average', 
               'EV/earnings compared to sector average', 'EV/EBITDA compared to sector average', 
               'EV/Book value compared to sector average', 'EV/revenue compared to sector average', 
               'EV/total cash compared to sector average', '(Equity + Assets)/Liabilities compared to sector average', 
               'Stock price/ 200-day-average compared to sector average', 
               'Stock price/ 50-day-average compared to sector average', 'Earnings compared to 1 year earlier', 
               'EBITDA compared to 1 year earlier', 'book value compared to 1 year earlier', 
               'Revenue compared to 1 year earlier', 'Cash compared to 1 year earlier', 
               '(Equity + Assets)/Liabilities compared to 1 year earlier', 
               'Gain/loss Random Forest prediction [%]', 'Gain/loss GBM prediction [%]',
               'Gain/loss GLMNET prediction [%]'  
)
