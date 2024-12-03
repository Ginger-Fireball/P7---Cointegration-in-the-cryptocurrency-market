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

# Plotting 20 day ahead predictions
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




#### Calculating the Prediction Errors -----------------------------------------
i <- 1


for (i in 1:validation_size){
  # Making the new model:
  Training_plus <- rbind(Training_all, Validation_all[i,])
  Johansen_trace <- ca.jo(Training_plus, type="trace", K=6, ecdet="const", spec="longrun")
  Johansen_model_Var <- vec2var(Johansen_trace, r=2)
  # Forecast the 5-day-ahead
  forecast <- predict(Johansen_model_Var, n.ahead = 5)
  ## Calculating MAE 
  Validation_all[i:(i+4),]
  
  
  ## Calculating RMSE
  
  ## Calculating MAPE
}


df_forecast - Validation_all[i:(i+4),]


forecast <- predict(Johansen_model_Var, n.ahead = 5)
forecast

Validation_all[i:(i+4),] - Validation_all[i:(i+4),]
 
df_forecast <- data.frame(Bitcoin = c(),
                          Ethereum = c(),
                          Solana = c(),
                          Ripple = c())


names(df_forecast) <- as.vector(NameCryptos)


c(NameCryptos)
NameCryptos
as.vector(NameCryptos)
df_forecast 

forecast$fsct$(as.character(NameCryptos[1]))

df_forecast <- cbind(df_forecast, forecast$fcst[[1]][,1])

df_forecast
View(df_forecast)

df_forecast - Validation_all[i:(i+4),]

as.character(NameCryptos[1])

forecast
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 