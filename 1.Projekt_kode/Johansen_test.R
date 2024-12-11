# Johansen 


#### Testing for cointegration with Johansen test ------------------------------

Johansen_lag_select <- VARselect(ts_Training_all, lag.max = 10, type = "none")
Johansen_lag_select
plot(Johansen_lag_select$criteria[1,])

# Lag order should be one less than var lag order aka: varlag -1, the function does take
# it into acount
# K The lag order of the series (levels) in the VAR
Johansen_trace <- ca.jo(ts_Training_all, type="trace", K=7, ecdet="none", spec="longrun")
Johansen_eigen <- ca.jo(ts_Training_all, type="eigen", K=7, ecdet="none", spec="longrun")

print(summary(Johansen_trace))
print(summary(Johansen_eigen))



#### Model building ------------------------------------------------------------
# Build the model
Johansen_model <- cajorls(Johansen_trace, r = 2, reg.number =  4)
Johansen_model

# Converting into VAR system
Johansen_model_Var <- vec2var(Johansen_trace, r=2)
Johansen_model_Var

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
Johan_forecasts <- predict(Johansen_model_Var, n.ahead = 20)

# Plotting 20 day ahead predictions
for (i in 1:4){
  # Producing data frame for plots
  Johan_forecasts_df <- data.frame(
    Time = 1:20,
    Price = Johan_forecasts$fcst[[i]][, 1],    
    Pricein_Lower = Johan_forecasts$fcst[[i]][, 2],
    Pricein_Upper = Johan_forecasts$fcst[[i]][, 3],
    Actual_prices = Validation_all[[i]][1:20]
   )
  crypto_now<-names(Johan_forecasts$fcst)
  
  given_training<-Training_all[,crypto_now[i]]
  
  historical_data <- data.frame(
    Time = -10:0,  # Example: Historical periods
    actual_data = tail(given_training,n = 11)
  )
  Johan_forecasts_df<-rbind(historical_data$actual_data[11],Johan_forecasts_df)
  Johan_forecasts_df[1,"Time" ] <- 0
  
  # Plot Price Predictions
  pdf(paste0("Billeder/20_day_ahead_",as.character(crypto_now[i]),"_from_johanson.pdf"))
  
  p <- (ggplot() +
    geom_line(data = historical_data, aes(x = Time, y = actual_data), color = "black") +
    geom_line(data = Johan_forecasts_df, aes(x = Time, y = Price), color = "blue") +
    geom_line(data = Johan_forecasts_df, aes(x = Time, y = Actual_prices), color = "red") +
    geom_ribbon(data = Johan_forecasts_df, aes(x = Time, ymin = Pricein_Lower, ymax = Pricein_Upper), fill = "blue", alpha = 0.2) +
    labs(title = paste("20-Day", as.character(NameCryptos[i]), "Forecast"), x = "Days Ahead", y = "Value") +
    theme_minimal())
  plot(p)
  dev.off()
  rm(p)
}




#### Calculating the Prediction Errors -----------------------------------------
MAE_1 <- NULL ; MAE_2 <- NULL ; MAE_3 <- NULL ; MAE_4 <- NULL ; MAE_5 <- NULL
RMSE_1 <- NULL ; RMSE_2 <- NULL ; RMSE_3 <- NULL ; RMSE_4 <- NULL ; RMSE_5 <- NULL
MAPE_1 <- NULL ; MAPE_2 <- NULL ; MAPE_3 <- NULL ; MAPE_4 <- NULL ; MAPE_5 <- NULL
Lags_optimal <- data.frame(matrix(NA, ncol = 4, nrow = (validation_size-5)))


for (i in 1:(validation_size-5)){
  # Extending the training data
  Training_plus <- rbind(Training_all, Validation_all[i,])
  # Optimal amount of lags
  lag_selection <- VARselect(Training_plus, lag.max = 10, type = "const")
  Lags_optimal[i , ] <- lag_selection$selection
  
  # Making the new model:
  Johansen_trace <- ca.jo(Training_plus, type="trace", K=Lags_optimal[i,1], ecdet="const", spec="longrun")
  Johansen_model_Var <- vec2var(Johansen_trace, r=2)
  

  
  # Forecast the 5-day-ahead and converting it into data frame
  forecasts <- predict(Johansen_model_Var, n.ahead = 5)
  ## Extract the `fcst` column from each cryptocurrency
  bitcoin_fcst <- forecasts$fcst$Bitcoin[, 1]
  ethereum_fcst <- forecasts$fcst$Ethereum[, 1]
  solana_fcst <- forecasts$fcst$Solana[, 1]
  ripple_fcst <- forecasts$fcst$Ripple[, 1]
  
  ## Combine into a data frame
  fcst_df <- data.frame(
    Bitcoin = bitcoin_fcst,
    Ethereum = ethereum_fcst,
    Solana = solana_fcst,
    Ripple = ripple_fcst
  )
  # Calculating Mean Absolute Error
  mae <- abs(Validation_all[i:(i+4),] - fcst_df)
  ## Saving the Values
  MAE_1 <- rbind(MAE_1, mae[1,])
  MAE_2 <- rbind(MAE_2, mae[2,])
  MAE_3 <- rbind(MAE_3, mae[3,])
  MAE_4 <- rbind(MAE_4, mae[4,])
  MAE_5 <- rbind(MAE_5, mae[5,])
  
  # Calculating Root Mean Square Error
  rmse <- (Validation_all[i:(i+4),] - fcst_df)^2
  ## Saving the Values
  RMSE_1 <- rbind(RMSE_1, rmse[1,])
  RMSE_2 <- rbind(RMSE_2, rmse[2,])
  RMSE_3 <- rbind(RMSE_3, rmse[3,])
  RMSE_4 <- rbind(RMSE_4, rmse[4,])
  RMSE_5 <- rbind(RMSE_5, rmse[5,])
  
  # Calculating Mean Absolute Percentage Error
  mape <- abs((Validation_all[i:(i+4),] - fcst_df) / Validation_all[i:(i+4),])
  ## Saving the Values
  MAPE_1 <- rbind(MAPE_1, mape[1,])
  MAPE_2 <- rbind(MAPE_2, mape[2,])
  MAPE_3 <- rbind(MAPE_3, mape[3,])
  MAPE_4 <- rbind(MAPE_4, mape[4,])
  MAPE_5 <- rbind(MAPE_5, mape[5,])
}

# combining the predictions errors into dataframes

# Mean Absolute Error
MAE_total <- NULL
for (i in 1:4){
  MAE <- NULL
  MAE <- cbind(MAE, mean(MAE_1[,i]))
  MAE <- cbind(MAE, mean(MAE_2[,i]))
  MAE <- cbind(MAE, mean(MAE_3[,i]))
  MAE <- cbind(MAE, mean(MAE_4[,i]))
  MAE <- cbind(MAE, mean(MAE_5[,i]))
  rownames(MAE) <- as.character(NameCryptos[i])
  MAE_total <- rbind(MAE_total, MAE)
}
colnames(MAE_total) <- c("one-day-ahead", "two-day-ahead", "three-day-ahead", 
                         "four-day-ahead", "five-day-ahead")
MAE_total
round(MAE_total, digits = 4)

# Root Mean Square Error
RMSE_total <- NULL
for (i in 1:4){
  RMSE <- NULL
  RMSE <- cbind(RMSE, sqrt(mean(RMSE_1[,i])))
  RMSE <- cbind(RMSE, sqrt(mean(RMSE_2[,i])))
  RMSE <- cbind(RMSE, sqrt(mean(RMSE_3[,i])))
  RMSE <- cbind(RMSE, sqrt(mean(RMSE_4[,i])))
  RMSE <- cbind(RMSE, sqrt(mean(RMSE_5[,i])))
  rownames(RMSE) <- as.character(NameCryptos[i])
  RMSE_total <- rbind(RMSE_total, RMSE)
}
colnames(RMSE_total) <- c("one-day-ahead", "two-day-ahead", "three-day-ahead", 
                          "four-day-ahead", "five-day-ahead")
RMSE_total
round(RMSE_total, digits = 4)

# Mean Absolute Percentage Error
MAPE_total <- NULL
for (i in 1:4){
  MAPE <- NULL
  MAPE <- cbind(MAPE, mean(MAPE_1[,i])*100)
  MAPE <- cbind(MAPE, mean(MAPE_2[,i])*100)
  MAPE <- cbind(MAPE, mean(MAPE_3[,i])*100)
  MAPE <- cbind(MAPE, mean(MAPE_4[,i])*100)
  MAPE <- cbind(MAPE, mean(MAPE_5[,i])*100)
  rownames(MAPE) <- as.character(NameCryptos[i])
  MAPE_total <- rbind(MAPE_total, MAPE)
}
colnames(MAPE_total) <- c("one-day-ahead", "two-day-ahead", "three-day-ahead", 
                          "four-day-ahead", "five-day-ahead")
MAPE_total
round(MAPE_total, digits = 4)


Ratio <- RMSE_total/MAE_total
print("RMSE/MAE Ratio")
print(Ratio)





