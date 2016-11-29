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

library(quantmod)

company_name = readline(" enter company name :\n")

stock_info = getOptionChain(company_name, Exp = NULL, src = "yahoo", "2016/2017")


month_wise_data = stock_info$Apr.21.2017
stock_info[[1]]
names(stock_info)
call_option_list = month_wise_data$calls
put_option_list = month_wise_data$puts


View(call_option_list)
View(put_option_list)

company_details = getQuote(company_name)
company_details$Last