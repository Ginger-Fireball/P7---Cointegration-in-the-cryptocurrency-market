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



# Combine forecast data
forecast_df <- data.frame(
  Time = 1:20,
  Bitcoin = forecast$fcst$Bitcoin[, 1],    
  Bitcoin_Lower = forecast$fcst$Bitcoin[, 2],
  Bitcoin_Upper = forecast$fcst$Bitcoin[, 3],
  Actual_prices = Validation_all$Bitcoin[1:20]
  )


# Plot Bitcoin Forecast
ggplot(forecast_df, aes(x = Time)) +
  geom_line(aes(y = Bitcoin), color = "blue") +
  geom_line(aes(y=Actual_prices), color = "red") +
  geom_ribbon(aes(ymin = Bitcoin_Lower, ymax = Bitcoin_Upper), fill = "blue", alpha = 0.2) +
  labs(title = "20-Day Bitcoin Forecast", x = "Days Ahead", y = "Value") +
  theme_minimal()



