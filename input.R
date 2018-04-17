# Loading functions to be used ----------------------------------------

targetPath <- "~/Dropbox/Courses/R/StockModel-I/ArchiveFin/"
# Load data frame with Sector.Name, Sector.Num, Industry.Name, Industry.Num into listAll
load(file = "~/Dropbox/Courses/R/StockModel-I/SectorIndustryInfo.RData")
# Load stock, sector and industry information into StockInfoAll
load(file = "~/Dropbox/Courses/R/StockModel-I/ArchiveFin/StockInfo.RData")

# Function that returns data frame with the most recent indicators (within 90 days )
# for all stocks within the entity (depending on flag) an input stock it belongs to
# If flag = 0, all stocks for all sectors and industries
# If flag = 1, all stocks within a given sector
# If flag = 2, all stocks within a given industry
date.today = Sys.Date()                   # today's date
indict_b_sto_func = function(stock, flag) {

  date.file = most_recent_date() # Date that has the most recent files
  fileName <- paste(targetPath, date.file, "+1m.Rdata", sep = "") 
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
# companies within the same industry as colums
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

# Return plots
validation_plot = function(i, MLmodel) { # i is the number of months back used in the model
  date.file = most_recent_date() # Date that has the most recent files
  load(file = paste(targetPath, date.file, "-", i, "m-validation.Rdata", sep = ""))
  temp <- subset(my_val, select=c("actual.win.loss", paste(MLmodel, "_pred", sep="")))
  colnames(temp) = c("actual", "pred")
  p = ggplot(temp, aes(x=pred, y=actual)) + geom_point(size = 0.3) + 
    xlab("Predicted gain [%]") + ylab("Actual gain [%]")
  return(  p + stat_density_2d() )
}

coeff_text = function(i, MLmodel, CoeffMethod) { # i is the number of months back used in the model
  date.file = most_recent_date() # Date that has the most recent files
  load(file = paste(targetPath, date.file, "-", i, "m-validation.Rdata", sep = ""))
  temp <- subset(my_val, select=c("actual.win.loss", paste(MLmodel, "_pred", sep="")))
  colnames(temp) = c("actual.win.loss", "ML")
  return( cor( x=temp$ML, y=temp$actual.win.loss, method = CoeffMethod) %>% round(., 3) )
}

residual_validation_plot = function(i, MLmodel) { # i is the number of months back used in the model
  date.file = most_recent_date() # Date that has the most recent files
  load(file = paste(targetPath, date.file, "-", i, "m-validation.Rdata", sep = ""))
  temp <- subset(my_val, select=c("actual.win.loss", paste(MLmodel, "_pred", sep="")))
  colnames(temp) = c("actual", "pred")
  return( ggplot(data = data.frame(res = temp$actual - temp$pred), aes(x = res)) +
    geom_histogram(aes(y = ..density..),
                   # bins = 30,
                   binwidth=0.5, 
                   col = 'red', alpha = 0.5, fill = 'red') +
    geom_density(aes(y = ..density..), col = 'blue') + xlab('Actual - Predicted') + coord_flip() )
}

# Function that returns a stock table ranked according to the prediction in i number of
# months using MLmodel
prediction_table = function(i, MLmodel) {
  date.file = most_recent_date() # Date that has the most recent files
  load(file = paste(targetPath, date.file = most_recent_date(), "+", i, "m-pred.Rdata", sep = ""))
  # Creating table with rankings from the different methods
  temp <- table.pred[order(table.pred[, paste(MLmodel, "_pred_", i, sep="")], decreasing = TRUE),] #Ranger
  temp <- subset(temp, select=c("Stock.SYM", paste(MLmodel, "_pred_", i, sep="")))
  colnames(temp) = c("Stock", "Predicted gain [%]")
  return ( temp )
}

# Function that returns the most recent previous date (within 10 days) of today's that has 
# quaterly financial data information
most_recent_date = function() {
  # Loading the most recent indicator table table.model
  temp = list.files(targetPath, pattern = "2018*") # All the files that may contain indicator information
  diffDate = 20   # Obtain the most recent date less than 10 days
  for (i in 1:length(temp) ) {
    if( length(strsplit(temp[i],"")[[1]])==19 ) { # Correct filename length 
      tempDate = as.Date(substr(temp[i],1,10)) # Extract date file was created
      if (date.today - tempDate < diffDate) { # Obtain the most recent date less than 20 days
        diffDate = date.today - tempDate 
        date.file = tempDate     
      }
    }
  }
  return(date.file)
}

# Function that returns a stock's prediction in i number of months using MLmodel. 
# If flag = 0, prediction is ranking place
# If flag = 1, prediction is % gain/loss
# If flag = 2, prediction is price stock
prediction_table_stock = function(month, MLmodel, stock, flag) {
  stockReturns = data.frame(date.pred.made = as.Date(character()),        # date prediction was made
                            date.predicted = as.Date(character()),        # date where return is predicted
                            return = numeric(0),                          # return
                            stringsAsFactors=FALSE
  )
  # Loading the most recent indicator table table.model
  temp = list.files( targetPath, pattern = paste("*", month, "m-pred.Rdata", sep = "") ) # Files that contain prediction information
  for (i in 1:length(temp) ) {
    if( length(strsplit(temp[i],"")[[1]])==24 ) { # Correct filename length 
      date.file = as.Date(substr(temp[i],1,10)) # Extract date file was created
      load(file = paste(targetPath, date.file, "+", month, "m-pred.Rdata", sep = ""))
      
      if ( stock %in% table.pred$Stock.SYM ) {
        table.ordered <- table.pred[order(table.pred[, paste(MLmodel, "_pred_", month, sep="")], decreasing = TRUE),]
        stockReturns[i, "date.pred.made"] = as.Date(date.file)
        stockReturns[i, "date.predicted"] = as.Date(date.file) %m+% months(month)
        if ( flag == 0 ) { stockReturns[i, "return"] = round(100-100.*match(stock, table.ordered$Stock.SYM)/length(table.ordered$Stock.SYM),1) }
        else if ( flag == 1 ) { stockReturns[i, "return"] = table.pred[table.pred$Stock.SYM == stock, paste(MLmodel, "_pred_", month, sep = "")] }
        else { stockReturns[i, "return"] = table.pred[table.pred$Stock.SYM == stock, "Price.Model.end"] * (1 + 
                    table.pred[table.pred$Stock.SYM == stock, paste(MLmodel, "_pred_", month, sep = "")]/100.) }
      }
    }
  }
  stockReturns = na.exclude(stockReturns)
  stockReturns = stockReturns %>% distinct(., date.predicted, .keep_all = TRUE)
  return (stockReturns)
}
