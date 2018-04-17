source("~/Dropbox/Courses/R/Shiny-Stock-Dashboard/TwitterAuth.R")

# ----------------------------------------------------------------
# Returns a corpus with the news of the stock between the specified dates
corpus.function <- function(stock) {
  corpus <- WebCorpus( YahooFinanceSource(stock, 
                                          curlOpts = curlOptions(followlocation = TRUE, maxconnects = 20, maxredirs = 20)
  ))
  test = ""
  for (j in 1:length(corpus)) {
    test = paste(test, corpus[[j]]$content)
  }
  test = trimws(test)
  myCorpus = Corpus(VectorSource(test))
  return(myCorpus)
}

# Returns a data frame with words and the frequencies they are used in recent news referring
# to the stock.
media.news.plot = function(stock) {
  # Obtaining corpus from YahooFinance
  myCorpus = corpus.function(stock)
  # Corpus processing
  myCorpusW = tm_map(myCorpus, content_transformer(tolower))
  myCorpusW = tm_map(myCorpusW, removePunctuation)
  myCorpusW = tm_map(myCorpusW, removeNumbers)
  myCorpusW = tm_map(myCorpusW, removeWords, c(stopwords("english"), 'free', 'report', 'stock'))
  dtm <- TermDocumentMatrix(myCorpusW)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  smallbubbles = head(d, 100)
  return(smallbubbles)
}

# Returns a data frame with words and the frequencies they are used in recent tweets referring
# to the stock.
media.tweets.plot = function(stock) {
  # harvest some tweets
  some_tweets = searchTwitter(paste("$",stock), n=1500, lang="en")
  # get the text
  some_txt = sapply(some_tweets, function(x) x$getText())
  
  # remove retweet entities
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  # remove at people
  some_txt = gsub("@\\w+", "", some_txt)
  # remove html links
  some_txt = gsub("https://t.co/\\w+", "", some_txt)
  # remove unnecessary things
  some_txt = gsub("\n", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  #some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # remove numbers
  some_txt = gsub("[[:digit:]]", "", some_txt)
  # define "tolower error handling" function 
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply 
  some_txt = sapply(some_txt, try.error)
  
  # Lower case
  some_txt = tolower(some_txt)
  some_txt = iconv(some_txt, "ASCII", "UTF-8", sub="")
  # remove NAs in some_txt
  some_txt = some_txt[!is.na(some_txt)]
  names(some_txt) = NULL
  # remove stopwords
  emo.docs = removeWords(some_txt, c(stopwords("english"), 'free', 'report', 'stock'))
  # remove repeating sequences
  emo.docs = unique(emo.docs)
  # create corpus
  myCorpusW = Corpus(VectorSource(emo.docs))
  
  myCorpusW = tm_map(myCorpusW, content_transformer(tolower))
  dtm <- TermDocumentMatrix(myCorpusW)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  smallbubbles = head(d, 100)
  return(smallbubbles)
}




