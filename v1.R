##clear memory
rm(list = ls())


# clear screen 
clear_screen <- function() {

    cat("\014")

}

################### get data 
library(quantmod)
#get the company name from user 
company_name = readline( prompt =" enter company name :")
stock_info = getOptionChain(company_name, Exp = NULL, src = "yahoo", "2016/2017")
######################################################
#picking which month data one wants to work on 
list_of_contents = names(stock_info)
for (i in 1:length(list_of_contents)) {
    cat(i, ") ", list_of_contents[i], " \n")


}
input = as.integer(readline("enter on which month data you want to work on.\n type the row number : \n"))
option_data = stock_info[[input]]
names(option_data)

################################################ company data 
company_details = getQuote(company_name)
spot_price = company_details$Last # !!!! spot price
cat("spot price: ", spot_price)
#################################################### volatility 

#####################################################

# volatility of asset - sigma 
today = Sys.Date()

previous_year = as.Date(seq(today, length = 2, by = "-1 years")[2])
library(tseries)
data <- get.hist.quote(company_name, start = previous_year)
price <- data$Close
ret <- log(lag(price)) - log(price)
sigma <- sd(ret) * sqrt(252) * 100
sigma <- sigma / 100 #!!!! sigma
cat(" sigma :", sigma)
################################################

# risk free rate 

risk_free_rate = 0.03
cat("risk free rate set as :", risk_free_rate)
######################################################
# date 
expiry_date = as.Date(list_of_contents[input], format = "%b.%d.%Y")

year_to_expire = as.numeric(expiry_date - today) / 365 # time 
days_to_expire = as.numeric(expiry_date - today)

cat(" time of option to mature : ", year_to_expire)
##########################################################################
# pick whether to work on call or put options 
choice = readline(" enter whether you want to work on \ncalls or options for call press 1 and for put press 2:\n ")

if (choice == 1) {
    # call options 
    call_option_list = option_data$calls
    View(call_option_list)
    input = as.numeric(readline("enter which option you want to work on : \n"))
    data_option = call_option_list[input,]

    strike = data_option$Strike # !!!! strike 
    price_option = data_option$Last # !!!!! option price 




    ############## price calculation 
    T = year_to_expire
    K = strike
    r = risk_free_rate
    S0 = spot_price

    d1 <- (log(S0 / K) + (r + sigma ^ 2 / 2) * T) / (sigma * sqrt(T))
    d2 <- d1 - sigma * sqrt(T)
    phid1 <- pnorm(d1)
    estimated_price <- S0 * phid1 - K * exp( - r * T) * pnorm(d2)
    cat(" estimated price of call option : ", estimated_price)
    cat(" price of call option on market ", price_option)






} else {
    # put options 

    put_option_list = option_data$puts
    View(put_option_list)
    input = as.numeric(readline("enter which option you want to work on : \n"))
    data_option = put_option_list[input,]

    strike = data_option$Strike # !!!! strike 
    price_option = data_option$Last # !!!!! option price 




    ############## price calculation 
    T = year_to_expire
    K = strike
    r = risk_free_rate
    S0 = spot_price

    d1 <- (log(S0 / K) + (r + sigma ^ 2 / 2) * T) / (sigma * sqrt(T));
    d2 <- d1 - sigma * sqrt(T);
    phimd1 <- pnorm( - d1)
    estimated_price <- -S0 * phimd1 + K * exp( - r * T) * pnorm( - d2);
    cat("estimated price of put option ", estimated_price)
    cat("price of put option on market ", price_option)




}


##################################################################################
# monte carlo simulation 
stock = S0
strike = K
TTM = T
rf = r
num.sim <- 100000R <- (rf - 0.5 * sigma ^ 2) * TTM
SD <- sigma * sqrt(TTM)TTM.price <- stock * exp(R + SD * rnorm(num.sim, 0, 1))

# call option 
TTM.call <- pmax(0, TTM.price - strike)
PV.call <- TTM.call * (exp( - rf * TTM))
mean(PV.call)

# put  option 

TTM.put <- pmax(0, strike - TTM.price)PV.put <- TTM.put * (exp( - rf * TTM))mean(PV.put)











##################################################################################

stock_info[[1]]
names(stock_info)



View(call_option_list)
View(put_option_list)

company_details = getQuote(company_name)
company_details$Last