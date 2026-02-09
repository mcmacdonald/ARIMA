


# 'backcast' to impute the number of alleged offenders and per capita rates for 1982

# function to backcast time series data
backcast <- function(x){
  
  # required packages
  require("forecast")
  
  # number of time points to project
  h <- 1
  
  # frequency
  f <- frequency(x)
  
  # reverse the time series
  rx <- stats::ts(rev(x), frequency = f)
  
  # forecast the reverse time series i.e., backcast
  y <- forecast::forecast(forecast::auto.arima(rx), h)
  
  # the missing data point
  mu <- y$mean[1] 
  
  # return 
  return(mu)
}
# use function to impute missing values

  # first, append missing values to data
  murders <- rbind(murders_1982, murders); rm(murders_1982)

  # backcast the number of mafia murders 
  murders_1982.mafia <- backcast(x = murders$mafia)
  
  # append imputed values
  murders$mafia[is.na(murders$mafia)] <- murders_1982.mafia
  
  # calculate homicide rates
  murders <- dplyr::mutate(murders, mafia_r = (mafia/pop) * 100000) # mafia homicides per 100,000 population

  
  # backcast the number of homicides, total
  murders_1982.total <- backcast(x = murders$total)
  
  # append imputed values
  murders$total[is.na(murders$total)] <- murders_1982.total
  
  # calculate homicide rates
  murders <- dplyr::mutate(murders, total_r = (total/pop) * 100000) # total homicides per 100,000 population





# close .r script


