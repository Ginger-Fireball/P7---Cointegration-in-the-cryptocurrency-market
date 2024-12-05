# Engel Granger test

#makes it so the dat is saved as a 4 way timeseries
ts_Training_all<-ts(Training_all)

#### Engel Granger test, testing pairwise cointegration ------------------------
# Generate all permutations of two elements
crypto_pairs <- expand.grid(First = NameCryptos, Second = NameCryptos)

# Ensure columns are treated as characters
crypto_pairs$First <- as.character(crypto_pairs$First)
crypto_pairs$Second <- as.character(crypto_pairs$Second)

# Remove cases where both elements are the same
crypto_pairs <- crypto_pairs[crypto_pairs$First != crypto_pairs$Second, ]

# Print the resulting data frame
crypto_pairs


Engel_granger <- function(x,y){
  # Run the cointegration regression 
  cointegration_model <- lm(ts_Training_all[,x] ~ ts_Training_all[,y])
  
  # Extract residuals from the regression
  residuals <- resid(cointegration_model)
  
  # Perform ADF test on residuals
  adf_result <- adf.test(residuals)
  return(adf_result$p.value)
}

# Add a new column with the p-value using mutate()
crypto_pairs <- crypto_pairs %>%
  mutate(P_Value = mapply(Engel_granger, First, Second))

crypto_pairs <- crypto_pairs %>%
  mutate(P_Value_Good = (P_Value<0.05) )
# Print the updated data frame
print(crypto_pairs)

rm(crypto_pairs)
rm(lag_selection)


#### Model building Solana ~ Ethereum ------------------------------------------
Training_S_E <- cbind(ts_Training_all[,"Solana"], ts_Training_all[,"Ethereum"])
VARselect(Training_S_E, lag.max = 10, type = "const")

# VECM model is build, lag = aic(value) - 1
vecm_S_E <- VECM(Training_S_E, r=1, estim = c("2OLS"), lag = 6)
summary(vecm_S_E)

# 20-day-ahead predictins
forecast_S_E <- predict(vecm_S_E, n.ahead = 20)
forecast_S_E_df <- data.frame(forecast_S_E)
colnames(forecast_S_E_df) <- c("Solana" , "Ethereum")

# Plotting 20 day ahead predictions
S_E <- c("Solana", "Ethereum")
for (i in 1:2){
  # Producing data frame for plots
  forecast_df <- data.frame(
    Time = 1:20,
    Price <- forecast_S_E_df[i],    
    Pricein_Lower <- forecast_S_E_df[S_E[i]] - (1),  # 1.96
    Pricein_Upper <- forecast_S_E_df[S_E[i]] + (1 ), # 1.96
    Actual_prices <- Validation_all[S_E[i]][1:20,]
    )
  colnames(forecast_df) <- c("Time", "Price", "Pricein_Lower", "Pricein_Upper", "Actual_prices")
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


#### ## ## ####
View(data.frame(
c(mae[1,], mae[2,], mae[3,], mae[4,], mae[5,])
))
mae[1,]
MAE 

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


#### Model building Ripple ~ Ethereum ------------------------------------------



#### Model building Ethereum ~ Solana ------------------------------------------



#### Model building Ripple ~ Solana --------------------------------------------









