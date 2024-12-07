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
  residuals <- cointegration_model$residuals

  # Perform ADF test on residuals
  adf_result <- adf.test(residuals)
  
  
  # Crit value
  #adf_test <- ur.df(residuals, type = "none", lags = adf_result$parameter)
  
  return(c(adf_result$p.value,as.numeric(adf_result$statistic)))
}

#summary(adf_test)
# Add a new column with the p-value using mutate()
crypto_pairs <- crypto_pairs %>%
  mutate(P_Value = mapply(function(x,y) Engel_granger(x,y)[1], First, Second))

crypto_pairs <- crypto_pairs %>%
  mutate(P_Value_Good = (P_Value<0.05) )

crypto_pairs <- crypto_pairs %>%
  mutate(test_statistic = mapply(function(x,y) Engel_granger(x,y)[2], First, Second))

crypto_Cointegrated <- crypto_pairs %>%
  filter(P_Value_Good == TRUE)


#THis wil pirnt ACF, The reisduals on a graph and the density for it.
for (i in 1:4){
  # Making the residuals
  Responce_var <- crypto_Cointegrated[i,1]
  Predictor_var <- crypto_Cointegrated[i,2]
  Model_adf<-lm(ts_Training_all[,Responce_var] ~ ts_Training_all[,Predictor_var])
  model_adf_residuals <- Model_adf$residuals
  model_adf_residuals_df<-as.data.frame(model_adf_residuals)
  model_adf_residuals_df$index <- 1:nrow(model_adf_residuals_df)
  
 
  # plotting and saving them as pdf's:
  #df_ts_residuals
  ## Plotting the residual
  p1 <- ggplot(model_adf_residuals_df, aes(x = index, y = model_adf_residuals)) +
        geom_line() +
    labs(x = "Time", y = "Residuals") +
    geom_smooth(method = "lm",formula = y ~ x, colour = "red") + #laver en lige linje kan hjælpe med trend
    theme_minimal()

    ## Plotting the acf
  p2 <- ggAcf(model_adf_residuals) +
    theme_minimal() +
    geom_segment(size = 1.3) +
    labs(title = NULL)
 
  ## Plotting the residuals as histrogram
  p3 <- ggplot(model_adf_residuals_df, aes(x = model_adf_residuals_df[,1])) +
    geom_histogram(aes(y = ..density..), binwidth = max(model_adf_residuals_df[,1])/50, fill = "lightblue", color = "black") +
    stat_function(
      fun = dnorm, 
      args = list(mean = mean(model_adf_residuals_df[,1]), sd = sd(model_adf_residuals_df[,1])), 
     color = "red", 
      size = 0.5
    ) +
    labs(x = "Residuals", y = "Density") +
    theme_minimal()
  
  
  pdf(paste0("Billeder/plot_grid_ADF_", as.character(Responce_var),"-",as.character(Predictor_var), ".pdf"))
  pic<-plot_grid(
    plot_grid(p3, p2, ncol = 2 ),   # Top row: p1 and p2 side by side
    p1,                            # Second row: p3 spans the entire width
    ncol = 1,                      # Stack top row and p3 vertically
    rel_heights = c(1, 1)        # Adjust height proportions (optional)
  )
  print(pic)
  dev.off()
}



#crypto_pairs <- crypto_pairs %>%
#  mutate(Crit_value = mapply(function(x,y) Engel_granger(x,y)[3], First, Second) )

#crypto_pairs <- crypto_pairs %>%
#  mutate(Crit_value_Good = (test_statistic< as.numeric(-2.8) ) )

#crypto_pairs <- crypto_pairs %>%
#  mutate(Crit_value_Good = (test_statistic<Crit_value) )

# Print the updated data frame
print(crypto_pairs)

rm(crypto_pairs)
rm(lag_selection)
#------------------------------- Functions used -------------------------------#

#### Plotting 20 day ahead predictions -----------------------------------------
day_ahead_plot <- function(df, predict_choice){
  for (i in 1:2){
    # Producing data frame for plots
    forecast_df <- data.frame(
      Time = 1:20,
      Price <- df[i],    
      Pricein_Lower <- df[predict_choice[i]] - (0),  # 1.96
      Pricein_Upper <- df[predict_choice[i]] + (0), # 1.96
      Actual_prices <- Validation_all[predict_choice[i]][1:20,]
    )
    colnames(forecast_df) <- c("Time", "Price", "Pricein_Lower", "Pricein_Upper", "Actual_prices")
    # Plot Price Predictions
    p <- (ggplot(forecast_df, aes(x = Time)) +
            geom_line(aes(y = Price), color = "blue") +
            geom_line(aes(y=Actual_prices), color = "red") +
            geom_ribbon(aes(ymin = Pricein_Lower, ymax = Pricein_Upper), fill = "blue", alpha = 0.2) +
            labs(title = paste("20-Day", as.character(predict_choice[i]), "Forecast"), x = "Days Ahead", y = "Value") +
            theme_minimal())
    plot(p)
    rm(p)
  }
}

#### Calculating the Prediction Errors -----------------------------------------

Prediction_Error(S_E)

Prediction_Error <- function(predict_choice){
  MAE_1 <- NULL ; MAE_2 <- NULL ; MAE_3 <- NULL ; MAE_4 <- NULL ; MAE_5 <- NULL
  RMSE_1 <- NULL ; RMSE_2 <- NULL ; RMSE_3 <- NULL ; RMSE_4 <- NULL ; RMSE_5 <- NULL
  MAPE_1 <- NULL ; MAPE_2 <- NULL ; MAPE_3 <- NULL ; MAPE_4 <- NULL ; MAPE_5 <- NULL
  Lags_optimal <- data.frame(matrix(NA, ncol = 4, nrow = (validation_size-5)))
  
  for (i in 1:(validation_size-5)){
    # Extending data
    Training_plus <- rbind(Training_all, Validation_all[i,])
    Training_Granger <- cbind(ts_Training_all[,predict_choice[1]], ts_Training_all[,predict_choice[2]])
    
    #Optimal amount of lags
    lag_selection <- VARselect(Training_Granger, lag.max = 10, type = "const")
    Lags_optimal[i , ] <- lag_selection$selection
    
    # Making the new model:
    vecm_Granger <- VECM(Training_Granger, r=1, estim = c("2OLS"), lag = Lags_optimal[i,1]-1)
    
    # Forecast the 5-day-ahead and converting it into data frame
    forecast_Granger <- predict(vecm_Granger, n.ahead = 5)
    forecast_Granger_df <- data.frame(forecast_Granger)
    colnames(forecast_Granger_df) <- predict_choice
    
    # Calculating Mean Absolute Error
    mae <- abs(Validation_all[i:(i+4),predict_choice] - forecast_Granger_df) 
    ## Saving the Values
    MAE_1 <- rbind(MAE_1, mae[1,])
    MAE_2 <- rbind(MAE_2, mae[2,])
    MAE_3 <- rbind(MAE_3, mae[3,])
    MAE_4 <- rbind(MAE_4, mae[4,])
    MAE_5 <- rbind(MAE_5, mae[5,])
    
    # Calculating Root Mean Square Error
    rmse <- (Validation_all[i:(i+4),predict_choice] - forecast_Granger_df)^2
    ## Saving the Values
    RMSE_1 <- rbind(RMSE_1, rmse[1,])
    RMSE_2 <- rbind(RMSE_2, rmse[2,])
    RMSE_3 <- rbind(RMSE_3, rmse[3,])
    RMSE_4 <- rbind(RMSE_4, rmse[4,])
    RMSE_5 <- rbind(RMSE_5, rmse[5,])
    
    # Calculating Mean Absolute Percentage Error
    mape <- abs((Validation_all[i:(i+4),predict_choice] - forecast_Granger_df)
                / Validation_all[i:(i+4),predict_choice])
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
  for (i in 1:2){
    MAE <- NULL
    MAE <- cbind(MAE, mean(MAE_1[,i]))
    MAE <- cbind(MAE, mean(MAE_2[,i]))
    MAE <- cbind(MAE, mean(MAE_3[,i]))
    MAE <- cbind(MAE, mean(MAE_4[,i]))
    MAE <- cbind(MAE, mean(MAE_5[,i]))
    rownames(MAE) <- predict_choice[i]
    MAE_total <- rbind(MAE_total, MAE)
  }
  colnames(MAE_total) <- c("one-day-ahead", "two-day-ahead", "three-day-ahead", 
                           "four-day-ahead", "five-day-ahead")
  MAE_total
  round(MAE_total, digits = 4)
  
  # Root Mean Square Error
  RMSE_total <- NULL
  for (i in 1:2){
    RMSE <- NULL
    RMSE <- cbind(RMSE, sqrt(mean(RMSE_1[,i])))
    RMSE <- cbind(RMSE, sqrt(mean(RMSE_2[,i])))
    RMSE <- cbind(RMSE, sqrt(mean(RMSE_3[,i])))
    RMSE <- cbind(RMSE, sqrt(mean(RMSE_4[,i])))
    RMSE <- cbind(RMSE, sqrt(mean(RMSE_5[,i])))
    rownames(RMSE) <- predict_choice[i]
    RMSE_total <- rbind(RMSE_total, RMSE)
  }
  colnames(RMSE_total) <- c("one-day-ahead", "two-day-ahead", "three-day-ahead", 
                            "four-day-ahead", "five-day-ahead")
  RMSE_total
  round(RMSE_total, digits = 4)
  
  # Mean Absolute Percentage Error
  MAPE_total <- NULL
  for (i in 1:2){
    MAPE <- NULL
    MAPE <- cbind(MAPE, mean(MAPE_1[,i])*100)
    MAPE <- cbind(MAPE, mean(MAPE_2[,i])*100)
    MAPE <- cbind(MAPE, mean(MAPE_3[,i])*100)
    MAPE <- cbind(MAPE, mean(MAPE_4[,i])*100)
    MAPE <- cbind(MAPE, mean(MAPE_5[,i])*100)
    rownames(MAPE) <- predict_choice[i]
    MAPE_total <- rbind(MAPE_total, MAPE)
  }
  colnames(MAPE_total) <- c("one-day-ahead", "two-day-ahead", "three-day-ahead", 
                            "four-day-ahead", "five-day-ahead")
  MAPE_total
  round(MAPE_total, digits = 4)
  
  print("MAE")
  print(MAE_total)
  print("RMSE")
  print(RMSE_total)
  print("MAPE")
  print(MAPE_total)
}


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
S_E <- c("Solana", "Ethereum") 

day_ahead_plot(forecast_S_E_df, S_E)
Prediction_Error(S_E)



#### Model building Ripple ~ Ethereum ------------------------------------------
Training_R_E <- cbind(ts_Training_all[,"Ripple"], ts_Training_all[,"Ethereum"])
VARselect(Training_R_E, lag.max = 10, type = "const")

# VECM model is build, lag = aic(value) - 1
vecm_R_E <- VECM(Training_R_E, r=1, estim = c("2OLS"), lag = 6)
summary(vecm_R_E)

# 20-day-ahead predictins
forecast_R_E <- predict(vecm_R_E, n.ahead = 20)
forecast_R_E_df <- data.frame(forecast_R_E)
colnames(forecast_R_E_df) <- c("Ripple" , "Ethereum")
R_E <- c("Ripple", "Ethereum") 

day_ahead_plot(forecast_R_E_df, R_E)
Prediction_Error(R_E)

#### Model building Ethereum ~ Solana ------------------------------------------
Training_E_S <- cbind(ts_Training_all[,"Ethereum"], ts_Training_all[,"Solana"])
VARselect(Training_E_S, lag.max = 10, type = "const")

# VECM model is build, lag = aic(value) - 1
vecm_E_S <- VECM(Training_E_S, r=1, estim = c("2OLS"), lag = 6)
summary(vecm_E_S)

# 20-day-ahead predictins
forecast_E_S <- predict(vecm_E_S, n.ahead = 20)
forecast_E_S_df <- data.frame(forecast_E_S)
colnames(forecast_E_S_df) <- c("Ethereum" , "Solana")
E_S <- c("Ethereum", "Solana") 

day_ahead_plot(forecast_E_S_df, E_S)
Prediction_Error(E_S)

#### Model building Ripple ~ Solana --------------------------------------------
Training_R_S <- cbind(ts_Training_all[,"Ripple"], ts_Training_all[,"Solana"])
VARselect(Training_R_S, lag.max = 10, type = "const")

# VECM model is build, lag = aic(value) - 1
vecm_R_S <- VECM(Training_R_S, r=1, estim = c("2OLS"), lag = 6)
summary(vecm_R_S)

# 20-day-ahead predictins
forecast_R_S <- predict(vecm_R_S, n.ahead = 20)
forecast_R_S_df <- data.frame(forecast_R_S)
colnames(forecast_R_S_df) <- c("Ripple" , "Solana")
R_S <- c("Ripple", "Solana") 

day_ahead_plot(forecast_R_S_df, R_S)
Prediction_Error(R_S)











