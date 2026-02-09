This code uses an autogressive integrated moving average (ARIMA) model to predict the changes in homicide rates in Sicily across parts of three decades, 1982-2017. T

To do so, I estimate a model that second differences the homicide rates, and contains three autoregssive (AR) terms and three moving average (MA) terms. 

I include an effect that models the changes rates of mafia homicides, given that mafia homicides likely represent a significant fraction of all homicides in any given year.

I also include an effect that splits the time series into pre-1992 and post-1992 segments. This effect is meant to model the crackdown on mafia activity after the assassination of Judges Giovanni Falcone and Paolo Borsellino in May and July, 1992.
