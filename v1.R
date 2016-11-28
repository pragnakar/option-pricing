##clear memory
rm(list = ls())


# clear screen 
clear_screen <- function() {

    cat("\014")

}

################### get data 

library(RCurl)
 library(jsonlite)
 library(plyr)

fixJSON <- function(json) {
    gsub('([^,{:]+):', '"\1":', json)
    
}

URL1 = 'http://www.google.com/finance/option_chain?q=%s&output=json'
URL2 = 'http://www.google.com/finance/option_chain?q=%s&output=json&expy=%d&expm=%d&expd=%d'


getOptionQuotes <- function(symbol) {
     url = sprintf(URL1, symbol)
     #
     chain = fromJSON(fixJSON(getURL(url)))
     #
     options = mlply(chain$expirations, function(y, m, d) {
         url = sprintf(URL2, symbol, y, m, d)
         expiry = fromJSON(fixJSON(getURL(url)))
         #
         expiry$calls$type = "Call"
         expiry$puts$type = "Put"
         #
         prices = rbind(expiry$calls, expiry$puts)
         #
         prices$expiry = sprintf("%4d-%02d-%02d", y, m, d)
         prices$underlying.price = expiry$underlying_price
         #
         prices
        
    })
     #
     options = cbind(data.frame(symbol), rbind.fill(options))
     #
     names(options) = c("price", "bid", "ask", "open.interest")
     #
     for (col in c("strike", "price", "bid", "ask"))
        options[, col] = as.numeric(options[, col])
    options[, "open.interest"] = suppressWarnings(as.integer(options[, "open.interest"]))
     #
     options[, c(1, 16, 15, 6, 10, 11, 17, 14, 12)]
    
}

AAPL = getOptionQuotes("AAPL")
