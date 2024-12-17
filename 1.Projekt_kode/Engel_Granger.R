# Engel Granger test
#Training all er bare vores 4 tidsserier
#makes it so the data is saved as a 4 way timeseries
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
  mean(model_adf_residuals_df$model_adf_residuals)
 
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


# Print the updated data frame
#print(crypto_pairs)

rm(crypto_pairs)
#rm(lag_selection)
#------------------------------- Functions used -------------------------------#


#day_ahead_plot(Training_all,Validation_all,"Solana","Ethereum")

#### Plotting 20 day ahead predictions -----------------------------------------
day_ahead_plot <- function(Data_train,Data_vali,coin1,coin2){
  Data_train <- Training_all
  Data_vali <- Validation_all
  coin1 <- "Solana"
  coin2<- "Ethereum"
  
  ts_Training_coin<-ts(Data_train)
  Training_coins <- cbind(ts_Training_coin[,coin1], ts_Training_coin[,coin2])
  coins_select <- VARselect(Training_coins, lag.max = 10, type = "const")
  plot_Aic_lag<-as.data.frame(t(coins_select$criteria))
  pdf(paste0("Billeder/AIC_Lag_for",as.character(coin1),"_",as.character(coin2),".pdf"))
  pic<-ggplot(plot_Aic_lag, aes(x = 1:length(plot_Aic_lag[,1])),
              y = plot_Aic_lag[,1] ) +
    geom_point(aes(y = plot_Aic_lag[,1], colour = "AIC"),size = 2) +
    labs(x = "Lags", y = "AIC score",colour ="AIC Score") +
    scale_color_manual(
      values = c("AIC" = "red" ),
      labels = as.character("AIC Scores"),
      name = NULL
    ) +
    theme_minimal()+ 
    theme( legend.background = element_rect(fill = "grey",linetype = "solid",colour ="grey"),
           legend.position = c(.95, .95),
           legend.justification = c("right", "top"),
           legend.box.just = "right",
           legend.margin = margin(6, 6, 6, 6),
           #legend.key.height = unit(2, 'cm'),
           legend.text = element_text(size=10)
    ) 
  print(pic)
  dev.off()
  
  
  
  summary(vecm_coins)
  # VECM model is build, lag = aic(value) - 1
  vecm_coins <- VECM(Training_coins, r=1, estim = c("2OLS"), lag = ((as.numeric(coins_select$selection[1])-1)))
  #summary(vecm_coins) #if needed
  forecast_coins <- predict(vecm_coins, n.ahead = 20)
  forecast_coins_df <- data.frame(forecast_coins)
  colnames(forecast_coins_df) <- c(coin1 , coin2)
  #pull values out for prediction intervals
  forecasted_Coin1<-forecast_coins_df[,1]
  forecasted_Coin2<-forecast_coins_df[,2]
  #Confidence interval
  residuals_vecm <- residuals(vecm_coins)
  # Estimate the standard errors for each series (use standard deviation of residuals)
  h<-1:20
  se_coin1 <- sd(residuals_vecm[, 1])*sqrt(h)
  se_coin2 <- sd(residuals_vecm[, 2])*sqrt(h)
  z_score <- qnorm(0.975)  # 1.96 for 95% CI
  
  # Confidence intervals for Solana
  lower_coin1 <- forecasted_Coin1 - z_score * se_coin1
  upper_coin1 <- forecasted_Coin1 + z_score * se_coin1
  
  # Confidence intervals for Ethereum
  lower_coin2 <- forecasted_Coin2 - z_score * se_coin2
  upper_coin2 <- forecasted_Coin2 + z_score * se_coin2
  
  # Combine forecasts and CI into a data frame
  result_with_confi <- data.frame(
    Period = 1:20,
    Coin1_Forecast = forecasted_Coin1,
    Lower_Coin1 = lower_coin1,
    Upper_Coin1 = upper_coin1,
    Coin2_Forecast = forecasted_Coin2,
    Lower_Coin2 = lower_coin2,
    Upper_Coin2 = upper_coin2
  )
  #Giving actual values for the coins
  result_with_confi<- result_with_confi %>%
    mutate(actual_Coin1=head(Data_vali[,coin1],n = 20))
  result_with_confi<- result_with_confi %>%
    mutate(actual_Coin2=head(Data_vali[,coin2],n = 20))
  #making ekstra for color change in graph
  result_with_confi<- result_with_confi %>%
    mutate(actual_Coin1_1=actual_Coin1)
  
  result_with_confi<- result_with_confi %>%
    mutate(actual_Coin2_1=actual_Coin2)
  
  historical_data <- data.frame(
    Period = -10:0,  # Example: Historical periods
    actual_Coin1 = tail(Data_train[,coin1],n = 11),
    actual_Coin2 = tail(Data_train[,coin2],n = 11)
  )
  
  #For the first coin
  result_with_coin1 <- result_with_confi%>%
    subset(select = c(Period,Coin1_Forecast,Lower_Coin1,Upper_Coin1,actual_Coin1,actual_Coin1_1))
  history_with_coin1 <- historical_data%>%
    subset(select = c(Period,actual_Coin1))
  # Assign the same value to the entire 11th row
  result_with_coin1<-rbind(history_with_coin1$actual_Coin1[11],result_with_coin1)
  result_with_coin1[1,"Period" ] <- 0
  
  #for the second coin
  result_with_coin2 <- result_with_confi%>%
    subset(select = c(Period,Coin2_Forecast,Lower_Coin2,Upper_Coin2,actual_Coin2,actual_Coin2_1))
  history_with_coin2 <- historical_data%>%
    subset(select = c(Period,actual_Coin2))
  # Assign the same value to the entire 11th row
  result_with_coin2<-rbind(history_with_coin2$actual_Coin2[11],result_with_coin2)
  result_with_coin2[1,"Period" ] <- 0
  
  
  pdf(paste0("Billeder/20_day_ahed_",as.character(coin1),"_from_",as.character(coin1),"_",as.character(coin2),".pdf"))
  
  p2<-ggplot() +
    # Plot historical data (1st geom_line)
    geom_line(data = history_with_coin1, aes(x = Period, y = actual_Coin1, colour = "Past Values")) +  # Historical actual data
    # Plot forecasted data (2nd geom_line)
    geom_line(data = result_with_coin1, aes(x = Period, y = Coin1_Forecast, colour = "Predicted Values")) +  # Forecasted values
    # Plot actual values after Period = 0 (3rd geom_line)
    geom_line(data = result_with_coin1, aes(x = Period, y = actual_Coin1, colour = "Actual Values")) +  # Actual after forecasted period
    # Confidence intervals for forecasted data
    geom_ribbon(data = result_with_coin1, aes(x = Period, ymin = Lower_Coin1, ymax = Upper_Coin1), fill = "blue", alpha = 0.2) + 
    # Labels and theme
    labs(title = paste0("Forecast for ", coin1, " with Confidence Intervals"),
         x = "Period", y = "Price in USD") +
    theme_minimal() +
    
    scale_color_manual(
      values = c("Past Values" = "black" ,"Predicted Values"="blue" , "Actual Values"="red" ),
      breaks = c("Past Values","Predicted Values","Actual Values"),
      name = NULL
    ) +
    theme( 
      plot.title = element_text(hjust = 0.5),
      legend.background = element_rect(fill = "grey",linetype = "solid",colour ="grey"),
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      #legend.key.height = unit(2, 'cm'),
      legend.text = element_text(size=10)
    )  
  print(p2)
  dev.off()

  
  pdf(paste0("Billeder/20_day_ahed_",as.character(coin2),"_from_",as.character(coin1),"_",as.character(coin2),".pdf"))
  p3<-ggplot() +
    # Plot historical data (1st geom_line)
    geom_line(data = history_with_coin2, aes(x = Period, y = actual_Coin2, colour = "Past Values")) +  # Historical actual data
    # Plot forecasted data (2nd geom_line)
    geom_line(data = result_with_coin2, aes(x = Period, y = Coin2_Forecast, colour = "Predicted Values")) +  # Forecasted values
    # Plot actual values after Period = 0 (3rd geom_line)
    geom_line(data = result_with_coin2, aes(x = Period, y = actual_Coin2, colour = "Actual Values")) +  # Actual after forecasted period
    # Confidence intervals for forecasted data
    geom_ribbon(data = result_with_coin2, aes(x = Period, ymin = Lower_Coin2, ymax = Upper_Coin2), fill = "blue", alpha = 0.2) + 
    # Labels and theme
    labs(title = paste0("Forecast for ", coin2, " with Confidence Intervals"),
         x = "Period", y = "Price in USD") +
    theme_minimal() +
    
    scale_color_manual(
      values = c("Past Values" = "black","Predicted Values"="blue","Actual Values"="red" ),
      breaks = c("Past Values","Predicted Values","Actual Values"),
      name = NULL
    ) +
  theme( 
         plot.title = element_text(hjust = 0.5),
         legend.background = element_rect(fill = "grey",linetype = "solid",colour ="grey"),
         legend.position = c(.05, .95),
         legend.justification = c("left", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6),
         #legend.key.height = unit(2, 'cm'),
         legend.text = element_text(size=10)
  ) 
  print(p3)
  dev.off()
}
#### Calculating the Prediction Errors -----------------------------------------

  

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
S_E <- c("Ripple", "Solana")
day_ahead_plot(Training_all,Validation_all,"Solana","Ethereum")
Prediction_Error(S_E)



#### Model building Ripple ~ Ethereum ------------------------------------------
#R_E <- c("Ripple","Ethereum")
#day_ahead_plot(Training_all,Validation_all,"Ripple","Ethereum")
#Prediction_Error(R_E)

#### Model building Ethereum ~ Solana ------------------------------------------
#E_S <- c("Ethereum","Solana")
#day_ahead_plot(Training_all,Validation_all,"Ethereum","Solana")
#Prediction_Error(E_S)

#### Model building Ripple ~ Solana --------------------------------------------
#R_S <- c("Ripple", "Solana") 
#day_ahead_plot(Training_all,Validation_all,"Ripple", "Solana")
#Prediction_Error(R_S)











