library(tidyverse)
library(lubridate)
library(forecast)
library(gridExtra)
library(quantmod)

#Load and filter data
load_ticker <- function(ticker, start, end) {
  
  #Conver start and end dates to date format
  start <- ymd(start)
  end <- ymd(end)
  
  #Load data
  ticker_data <- as.tibble(getSymbols(ticker, src = "yahoo", from = start, to = end, env = NULL))
  
  #Filter data
  ticker_data <- ticker_data %>%
    mutate(adj_close = .[[5]], volume = .[[4]]) %>%
    select(adj_close, volume) %>%
    as.ts()
  
  return(ticker_data)
}


#Makes linear models of specified time window (default 5) and returns coefficients:
decision <- function(ticker = "AAPL", start = "2017-10-01", end = "2017-10-30", time_window = 5, explanatory_plot = TRUE) {
  
  ticker_data = load_ticker(ticker, start, end)
  
  #Filters based on window
  model_data <- tail(ticker_data, time_window)
  
  #Models linear relationship on the prices
  fit_price <- tslm(adj_close ~ trend, data = model_data)
  
  #Models linear relationship on the volume
  fit_volume <- tslm(volume ~ trend, data = model_data)
  
  #Stores the coefficients
  model_coefs <- tibble("Price Model" = fit_price$coefficients, "Volume Model" = fit_volume$coefficients)
  
  #Returns the decision based on calculations
  if (model_coefs$`Price Model`[2] > 0 && model_coefs$`Volume Model`[2] < 0){
    print("The hype is over. SELL.")
  } else if (model_coefs$`Price Model`[2] > 0 && model_coefs$`Volume Model`[2] > 0){
    print("The hype is still existing. BUY/HOLD")
  } else if (model_coefs$`Price Model`[2] < 0 && model_coefs$`Volume Model`[2] < 0){
    print("The panic is over. BUY") 
  } else if (model_coefs$`Price Model`[2] < 0 && model_coefs$`Volume Model`[2] > 0){
    print("Panic mode. Everyone is selling - SHORT SOON OR KYS (Or wait, idc)")
  } else {
    warning("Wtf, both slopes are either 0 or something strange is happening")
  }
  
  #Plot graph explaining the decisions if TRUE
  if (explanatory_plot){
    
    #Create dates and remove weekends
    data_date <- tibble(date = seq(ymd(start), ymd(end), by = "day")) %>%
      mutate(wday = weekdays(date)) %>%
      filter(!(wday == "Saturday" | wday == "Sunday")) %>%
      select(date)
    
    #Adjustments as one stocks exclude "last date" from search for some reason
    data_date <- data_date[1:(nrow(data_date)-1),]
      
    #Add stock data to data_plot
    ticker_data = as.tibble(ticker_data) %>%
      mutate(date = data_date$date) %>%
      select(date, adj_close, volume)
    
    #Create fitted values data
    data_fitted <- tibble(date = tail(data_date, time_window)$date, fit_price = fit_price$fitted, fit_vol = fit_volume$fitted)
    
    
    #Create price plot
    plot_price <- ggplot(data = ticker_data) +
      geom_line(mapping = aes(x = date, y = adj_close), col = "darkblue") +
      geom_line(data = data_fitted, mapping = aes(date, y = fit_price), col = "red") +
      labs(x = "", y = "Adjusted Close", title = "Price Model") + 
      theme_minimal()
    
    #Create volume plot
    plot_vol <- ggplot(data = ticker_data) +
      geom_bar(mapping = aes(x = date, y = volume), fill = "darkgrey", stat = "identity") +
      geom_line(data = data_fitted, mapping = aes(date, y = fit_vol), col = "red", lwd = 1.2) +
      labs(x = "", y = "Volume", title = "Volume Model") + 
      theme_minimal()
    
    #Arrange and plot
    grid.arrange(plot_price, plot_vol, heights=c(2.5/4, 1.5/4))
    
    
  }

  
}

#-------------------Testing-------------------------

# Apple Stock
ticker_data = decision(ticker = "AAPL",time_window = 5)

#Day 1.
decision(ticker = "VRX", start = "2017-10-07", end = "2017-11-07", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: 6.49 %"
decision(ticker = "VIRT", start = "2017-10-01", end = "2017-10-30", time_window = 5, explanatory_plot = T)
#"The hype is over. SELL.", "FACT: 4.82 %"

#S&P 500: 0.12%

#Day 2.
decision(ticker = "SYNA", start = "2017-10-09", end = "2017-11-09", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: -2.23 %"
decision(ticker = "PCRX", start = "2017-10-09", end = "2017-11-09", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: -2.43 %"
decision(ticker = "VRTU", start = "2017-10-09", end = "2017-11-09", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: -0.31 %"
decision(ticker = "INGN", start = "2017-10-09", end = "2017-11-09", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: 2.46 %"
decision(ticker = "HALO", start = "2017-10-09", end = "2017-11-09", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: -3.07 %"

#S&P 500: -0.38%

#Day 3.
decision(ticker = "SYNA", start = "2017-10-09", end = "2017-11-10", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: -6.84 %"
decision(ticker = "PCRX", start = "2017-10-09", end = "2017-11-10", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: 3.89 %"
decision(ticker = "VRTU", start = "2017-10-09", end = "2017-11-10", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: -0.22 %"
decision(ticker = "INGN", start = "2017-10-09", end = "2017-11-10", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: -0.47%"
decision(ticker = "HALO", start = "2017-10-09", end = "2017-11-10", time_window = 5, explanatory_plot = T)
#"The hype is still existing. BUY/HOLD", "FACT: -5%"

#Total 2.67%
