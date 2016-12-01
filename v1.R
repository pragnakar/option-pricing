##clear memory
rm(list = ls())



###############################################################################
#The objective of this program is to compare price of options of various companies 
#with values estimated from Black –Scholes model and Monte Carlo simulation model

#In the end we generate data frames that show strike price of option along with
#the option prices estimated by Black –Scholes model and Monte Carlo simulation model

#The column name PRICE_IN_MARKET represents price of options on open market
#The column name ESTIMATED_PRICE is the price estimated from Black –Scholes model
#The column name SIMULATED_PRICE is the price estimated form Monte Carlo simulation model
#The column name STRIKE is the strike price of the option.

#All the values displayed in table are in US$
#load the function console() and run it 

###############################################################################



console <- function() {

    ################### get data 
    library(quantmod)
    ####################################################
    #notes
    cat("############################################\n")
    cat("\nwhile entering the company name\n")
    cat("enter the name that appear on stock indexes\n")
    cat("like for APPLE, the name on index is 'aapl' \n")
    cat("similarly for GOOGLE, the name on idex is 'googl'\n")
    cat("############################################\n\n")
    #####################################################
    #get the company name from user 
    company_name = readline(prompt = " enter company name :")
    cat("\nwait for 5 seconds for the data to be downloaded from YAHOO\n")
    stock_info = getOptionChain(company_name, Exp = NULL, src = "yahoo", "2016/2017")
    ######################################################
    ###### slect this to change month data 
    #picking which month data one wants to work on 
    cat("download the data of the company :: ", company_name, "\n")
    cat(" for the following months \n\n \n")
    list_of_contents = names(stock_info)
    for (i in 1:length(list_of_contents)) {
        cat(i, ") ", list_of_contents[i], " \n")


    }
    cat("\n\nenter on which month data you want to work on.\n type the row number : \n")
    input = as.integer(readline())
    option_data = stock_info[[input]]
    names(option_data)

    ################################################ company data 
    company_details = getQuote(company_name)
    spot_price = company_details$Last # !!!! spot price
    #################################################### volatility 
    cat("name of the company :",company_name,"\n")
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
    cat("sigma :", sigma,"\n")
    ################################################

    # risk free rate
    # intrest rate on three month US Treasury Bill

    risk_free_rate = 0.0033

    cat("risk free rate set as :", risk_free_rate,"\n")

    ######################################################
    ##### from here we perform operations on data  
    ######################################################
    # date 
    exp_date = list_of_contents[input]
    expiry_date = as.Date(list_of_contents[input], format = "%b.%d.%Y")

    year_to_expire = as.numeric(expiry_date - today) / 365 # time 
    days_to_expire = as.numeric(expiry_date - today)

    cat("time of option to mature : ", exp_date, "\n")
    cat("spot price is at ", spot_price,"\n")

    option_data = stock_info[[input]]
    ###########call option
    cat("\n\nview data frames call_options_on_market and  call_option_estimations \n\n")
    call_options_on_market = option_data$calls
    PRICE_IN_MARKET <- c()
    ESTIMATED_PRICE <- c()
    SIMULATED_PRICE <- c()
    STRIKE <- c()
    View(call_options_on_market)
    for (input in 1:nrow(call_options_on_market)) {
        data_option = call_options_on_market[input,]

        strike = data_option$Strike # !!!! strike 
        price_option = data_option$Last # !!!!! option price 
        STRIKE <- c(STRIKE, strike)



        ############## price calculation 
        T = year_to_expire
        K = strike
        r = risk_free_rate
        S0 = spot_price
        d1 <- (log(S0 / K) + (r + sigma ^ 2 / 2) * T) / (sigma * sqrt(T))
        d2 <- d1 - sigma * sqrt(T)
        phid1 <- pnorm(d1)
        estimated_price <- S0 * phid1 - K * exp( - r * T) * pnorm(d2)
        ESTIMATED_PRICE <- c(ESTIMATED_PRICE, round(estimated_price, 4))
        PRICE_IN_MARKET <- c(PRICE_IN_MARKET, price_option)
        ##########################################################
        # monte carlo simulation 
        num.sim <- 100000
        R <- (r - 0.5 * sigma ^ 2) * T
        SD <- sigma * sqrt(T)
        TTM.price <- S0 * exp(R + SD * rnorm(num.sim, 0, 1))

        # call option 
        TTM.call <- pmax(0, TTM.price - K)
        PV.call <- TTM.call * (exp( - r * T))
        SIMULATED_PRICE <- c(SIMULATED_PRICE, round(mean(PV.call), 4))
    }

 
    call_option_estimations <- data.frame(STRIKE, PRICE_IN_MARKET, ESTIMATED_PRICE, SIMULATED_PRICE)

    View(call_option_estimations)

    ####################### put option
    cat("view data frames put_options_on_market and  put_option_estimations \n\n")
    put_options_on_market = option_data$puts
    PRICE_IN_MARKET <- c()
    ESTIMATED_PRICE <- c()
    SIMULATED_PRICE <- c()
    STRIKE <- c()
    View(put_options_on_market)
    for (input in 1:nrow(put_options_on_market)) {

        data_option = put_options_on_market[input,]

        strike = data_option$Strike # !!!! strike 
        price_option = data_option$Last # !!!!! option price 
        STRIKE <- c(STRIKE, strike)



        ############## price calculation 
        T = year_to_expire
        K = strike
        r = risk_free_rate
        S0 = spot_price

        d1 <- (log(S0 / K) + (r + sigma ^ 2 / 2) * T) / (sigma * sqrt(T));
        d2 <- d1 - sigma * sqrt(T);
        phimd1 <- pnorm( - d1)
        estimated_price <- -S0 * phimd1 + K * exp( - r * T) * pnorm( - d2);
        ESTIMATED_PRICE <- c(ESTIMATED_PRICE, round(estimated_price, 4))

        PRICE_IN_MARKET <- c(PRICE_IN_MARKET, price_option)
        ##################################################################################
        # monte carlo simulation 
        num.sim <- 100000
        R <- (r - 0.5 * sigma ^ 2) * T
        SD <- sigma * sqrt(T)
        TTM.price <- S0 * exp(R + SD * rnorm(num.sim, 0, 1))
        TTM.put <- pmax(0, K - TTM.price)
        PV.put <- TTM.put * (exp( - r * T))
        SIMULATED_PRICE <- c(SIMULATED_PRICE, round(mean(PV.put), 4))
    }


    put_option_estimations <- data.frame(STRIKE, PRICE_IN_MARKET, ESTIMATED_PRICE, SIMULATED_PRICE)

    View(put_option_estimations)



}


#############################################################
#############################################################

# after loading the function run the console() function below 
console()


#############################################################