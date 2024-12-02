# Johansen 


#### Testing for cointegration with Johansen test ------------------------------
source("Projekt_kode_Pull_Crypto.R")

VARselect(ts_Training_all, lag.max = 10, type = "none")

Johansen_trace <- ca.jo(ts_Training_all, type="trace", K=6, ecdet="const", spec="longrun")
Johansen_eigen <- ca.jo(ts_Training_all, type="eigen", K=6, ecdet="const", spec="longrun")

print(summary(Johansen_trace))
print(summary(Johansen_eigen))


#### Model building ------------------------------------------------------------
# Build the model
Johansen_model <- VECM(ts_Training_all, lag = 6, r = 2, estim = ("ML"))
# Converting into VAR system
Johansen_model_Var <- vec2var(Johansen_trace, r=2)

## Diagnostic Tests ##
# Serial Correlation
serial.test(Johansen_model_Var, lags.pt = 7, type = "PT.asymptotic")

# ARCH Effects
arch.test(Johansen_model_Var, lags.multi = 15, multivariate.only = TRUE)

# Normality of Residuals
normality.test(Johansen_model_Var, multivariate.only = TRUE)

# Impulse Response Function

# Variance Decomposition


#### VECM Forecasting ----------------------------------------------------------
# Predicting x-dayahead 
forecast <- predict(Johansen_model_Var, n.ahead = 20)

fanchart(forecast, names = "Bitcoin", xlab = "time", ylab = "Bitcoin")
fanchart(forecast, names = "Ethereum", xlab = "time", ylab = "Ethereum")
fanchart(forecast, names = "Solana", xlab = "time", ylab = "Solana")
fanchart(forecast, names = "Ripple", xlab = "time", ylab = "Ripple")


i <- 4
# Combine forecast data
for (i in 1:4){
  # Producing data frame for plots
  forecast_df <- data.frame(
    Time = 1:20,
    Price = forecast$fcst[[i]][, 1],    
    Pricein_Lower = forecast$fcst[[i]][, 2],
    Pricein_Upper = forecast$fcst[[i]][, 3],
    Actual_prices = Validation_all[[i]][1:20]
   )
  # Plot Price Predictions
  p <- (ggplot(forecast_df, aes(x = Time)) +
    geom_line(aes(y = Price), color = "blue") +
    geom_line(aes(y=Actual_prices), color = "red") +
    geom_ribbon(aes(ymin = Pricein_Lower, ymax = Pricein_Upper), fill = "blue", alpha = 0.2) +
    labs(title = paste("20-Day", as.character(NameCryptos[i]), "Forecast"), x = "Days Ahead", y = "Value") +
    theme_minimal())
  plot(p)
  rm(p)
}




























