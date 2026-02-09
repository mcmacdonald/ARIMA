


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





# construct plot data
plot_df <- data.frame(
  # time 
  anno = murders$anno[murders$region == "SICILIA"],
  # actual values of the homicide rates
  actual = y,
  # predicted values
  predicted <- stats::fitted(result)
  )

# plot
fig1 <- ggplot2::ggplot(
  plot_df, ggplot2::aes(x = anno)
  ) +
  ggplot2::geom_vline(
    xintercept = 1992, linetype = "dashed",
    color = "grey80", linewidth = 1
    ) +
  ggplot2::annotate(
    "text", x = 1992, y = 425, label = " \u2190 1992 assassinations of Judges Giovanni Falcone and Paolo Borsellino",
    hjust = -0.05, size = 2.5
    ) +
  ggplot2::geom_point(
    ggplot2::aes(y = actual, color = "Actual homicide rates"),
    shape = 21, fill = "white", size = 3, stroke = 1
    ) +
  ggplot2::geom_line(
    ggplot2::aes(y = fitted, color = "Predicted homicide rates"), 
    linewidth = 1
    ) +
  ggplot2::scale_color_manual(
      name = NULL,
      values = c("Actual homicide rates" = "black", "Predicted homicide rates" = "red"),
      breaks = c("Actual homicide rates", "Predicted homicide rates")
      ) +
  ggplot2::scale_x_continuous(limits = c(1980, 2020)) +
  ggplot2::scale_y_continuous(limits = c(0, 600)) +
  ggplot2::labs(
      title = "Homicides in Sicily, 1982-2017",
      x = "Year",
      y = "Homicide rate per 100,000 population"
      ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, face = "bold"),
    axis.title = ggplot2::element_text(size = 12),
    axis.text = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 8),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.background = ggplot2::element_rect(
        fill = "white", 
        color = "white"
        )
      )
ggplot2::ggsave(
  plot = fig1, 
  filename = "homicides.png",
  path = "~/Desktop/",
  width = 8, 
  height = 4, 
  dpi = 720
  )





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




