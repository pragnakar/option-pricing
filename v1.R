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

#get the company name from user 
company_name = readline(" enter company name :\n")

stock_info = getOptionChain(company_name, Exp = NULL, src = "yahoo", "2016/2017")
######################################################

month_wise_data = stock_info$Apr.21.2017
#picking which month data one wants to work on 
list_of_contents = names(stock_info)
for (i in 1:length(list_of_contents)) {
    cat(i, ") ", list_of_contents[i], " \n")


}

input = as.integer(readline("enter on which month data you want to work on.\n type the row number : \n"))

a = stock_info$Dec.02.2016
View(a)
option_data = stock_info[[input]]
View(option_data)
names(option_data)

################################################ company data 
company_details = getQuote(company_name)
spot_price = company_details$Last # spot price 
#################################################### volatility 

#####################################################

# volatility of asset - sigma 

library(tseries)
data <- get.hist.quote(company_name, origin = )
price <- data$Close
ret <- log(lag(price)) - log(price)
sigma <- sd(ret) * sqrt(252) * 100
sigma <- sigma / 100
##########################################################################
# pick whether to work on call or put options 
choice = readline(" enter whether you want to work on \ncalls or options for call press 1 for put press 2:\n ")

if (choice == 1) {
    # call options 
call_option_list = option_data$calls
View(call_option_list)
data_option = call_option_list

strike = data_option$Strike # strike 
price_option = data_option$Last # option price 

expiry_date = as.Date( list_of_contents[input], format = "%b.%d.%Y")

year_to_expire = as.numeric(expiry_date - today) / 365 # time 
days_to_expire = as.numeric(expiry_date - today)


} else {
# put options 

put_option_list = option_data$puts
View(put_option_list)


}

stock_info[[1]]
names(stock_info)



View(call_option_list)
View(put_option_list)

company_details = getQuote(company_name)
company_details$Last