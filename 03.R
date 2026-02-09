


# autoregressive integrated moving average models for time series data
# https://cran.r-project.org/web/packages/equatiomatic/vignettes/forecast-arima.html


# don't run
# install.packages(
# c("forecast", 
#   "tseries",
#   "equatiomatic"
#    )
#   )




# dependent variable - total number of homicides per 100,000 population in Sicily
y <- murders$total[murders$region=="SICILIA"]



# test assumption of stationarity
stationarity.test <- function(y){
  print(tseries::adf.test(y))
}
stationarity.test(diff(y, differences = 1))
stationarity.test(diff(y, differences = 2))
stationarity.test(diff(y, differences = 3))



# construct vector of independent variables
vectorize <- function(data, region, x){
  `%>%` <- magrittr::`%>%`
  data <- data %>% dplyr::filter(region == {{region}}) %>% dplyr::pull({{x}})
}
# total number of mafia homicides per 100,000 population
mafia <- vectorize(murders, region = "SICILIA", x = mafia_r)



# dummy code the effect for post-1992 mafia bombings
d_1992 <- c(rep(0, 10), rep(1, 26))



# vector of independent variables
x <- as.matrix(
  cbind(
    mafia,
    d_1992
    )
  )



# code to calculate the p-values for the ARIMA model
# code taken from: https://rpubs.com/yesantiara/ARIMA
pvalue <- function (x, digits = 4, se = T, ...){ 
  if (length(x$coef) > 0) {
    cat("\ncoefficients for the ARIMA model:\n")
    coef <- round(x$coef, digits = digits)
    
    # compute standard errors
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      
      # bind together
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      
      # t-statistic
      tstat <- coef[1,]/ses
      
      # calculation of the p-value
      p  <- 2 * stats::pt(abs(tstat), df=length(x$residuals)-1, lower.tail = F)
      
      # join
      coef <- rbind(coef, t=round(tstat,digits=digits),p.value=round(p,digits=digits))
      coef <- t(coef) # transpose 
      }
    print.default(coef, print.gap = 2)
    }
}



# function to estimate the model
model <- function(y, pdq, x){
  
  # required packages
  require(forecast)
  
  # time series model
  output <- forecast::Arima(
    stats::ts(y, freq = 1),
    order = pdq, 
    xreg = x,
    include.constant = FALSE
    )
  
  # print
  pvalue(output)
  
  # return
  return(output)
}
result <- model(
  y = y,
  pdq = c(
    3, # p = auto-regressive term(s)
    2, # d = the middle term "I" = 2 because y is second differenced; set I = 0 if y is differenced manually prior to estimation
    3  # q = moving average term(s)  
    ), 
  x = x
  )

# don't run
# extract the equation
# https://cran.r-project.org/web/packages/equatiomatic/vignettes/forecast-arima.html
# equatiomatic::extract_eq(spec1)





# range of the time series
anno <- murders$anno[murders$region == "SICILIA"]

# fitted values
fitted <- stats::fitted(result)

# plot fitted and actual values
plot(anno, y, 
     lwd = 2,
     pch = 21,  # circle with border
     bg = "white",  # white fill
     col = "black",  # black rim
     cex = 1.2,  # dot size
     main = "Homicides in Sicily, 1982-2017",
     xlab = "Year", ylab = "Homicide rate per 100,000 population",
     xlim = c(1980, 2020),
     ylim = c(0, 540)
     )
lines(anno, fitted, col = "red", lwd = 2)
abline(v = 1992, col = "blue", lty = 2, lwd = 2)
text(1992, 500, "1992 Bombings", pos = 4, col = "black", cex = 0.9)
legend(
  "topright", 
  c("Actual", "Fitted"), 
  col = c("black", "red"), 
  pch = c(21, NA), # circle with border for actual values, nothing for fitted values
  pt.bg = c("white", NA), # white fill for actual values
  lty = c(NA, 1), # no line for actual values, solid line for fitted values
  lwd = c(NA, 2),
  pt.cex = 1.2  # match dot size
  )

# set working directory
setwd("~/Desktop/")
# save as PNG
png("homicides.png", width = 1200, height = 800, res = 150)
# ... same plot code ...
dev.off()



# residual tests
forecast::checkresiduals(result); stats::shapiro.test(result$residuals)

# plot residuals
par(mfrow = c(1, 1))
plot(
  anno, 
  result$residuals, 
  type = "l",
  main = "ARIMA model residuals",
  xlab = "Year", 
  ylab = "Residuals"
  )
abline(
  h = 0, 
  col = "red", 
  lty = 2
  )




# close .r script




