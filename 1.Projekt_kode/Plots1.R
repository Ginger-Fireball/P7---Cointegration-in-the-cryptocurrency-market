#levels---------------
# Plotting prices in levels, with each Crypto having it's own plot
for (i in 1:4) {
  # Open pdf device
  pdf(paste0("Billeder/Crypto_in_levels_", as.character(NameCryptos[i]), ".pdf"))
  # Create the plot
  p <- ggplot(Training_all, aes(x = 1:nrow(Training_all), y = Training_all[, i])) +
    geom_line(aes(colour = as.character(NameCryptos[i]))) +
    labs(x = "Days", y = "Price in Levels") +
    scale_color_manual(
      values = c("Bitcoin" = "red", "Ethereum" = "darkgoldenrod1", "Solana" = "blue", "Ripple" = "green"),
      labels = as.character(NameCryptos[i]),
      name = NULL
    ) +
    theme(legend.position = "bottom")
  # Print to pdf device
  print(p)
  # Close pdf device
  dev.off()
}


# Plotting prices in percentage all in one graf (changes from the day before to today)
ggplot(Training_all_pro, aes(x = 1:nrow(Training_all_pro), y = Training_all_pro)) +
  geom_line(aes(y = Bitcoin, colour = "Bitcoin")) +
  geom_line(aes(y = Ethereum, colour = "Ethereum")) +
  geom_line(aes(y = Solana, colour = "Solana")) +
  geom_line(aes(y = Ripple, colour = "Ripple")) +
  labs(x = "Days", y = "Procent") +
  scale_color_manual(values = c("Bitcoin" = "red", "Ethereum" = "darkgoldenrod1","Solana"= "blue","Ripple"="green"),
                     labels = NameCryptos,
                     name = NULL) +
  theme(legend.position = "bottom")


#lags----------------------

#Number of lags in our model
lag_selection <- VARselect(Training_all, lag.max = 10, type = "const")
print(lag_selection$selection)
plot_Aic_lag<-as.data.frame(t(lag_selection$criteria))

pdf("Billeder/Crypto_lags.pdf")
ggplot(plot_Aic_lag, aes(x = 1:length(plot_Aic_lag[,1])),
       y = plot_Aic_lag[,1] ) +
  geom_point(aes(y = plot_Aic_lag[,1], colour = "darkred"),size = 2) +
  labs(x = "Lags", y = "AIC score") + 
  theme_minimal()+ 
  theme(legend.position = "none") 
dev.off()
#we use AIC


#I(1) and stationary----------------- 


#to check if they are stationary  with Argmented dickifuller test
ts_Training_all<-ts(Training_all)
adf.test(ts_Training_all[, "Bitcoin"])
adf.test(ts_Training_all[, "Ethereum"])
adf.test(ts_Training_all[, "Solana"])
adf.test(ts_Training_all[, "Ripple"])


# Plotting the residuals in two ways and the acf plot
for (i in 1:4){
  # Making the residuals
  ts_Training_all <- ts(Training_all[,i])
  ts_residuals <- diff(ts_Training_all)
  # plotting and saving them as pdf's:
  ## Plotting the residual
  pdf(paste0("Billeder/Residuals_", as.character(NameCryptos[i]), ".pdf"),width = 240,height = 100)
  p1 <- plot(ts_residuals) 
  print(p1)
  dev.off()
  ## Plotting the acf
  pdf(paste0("Billeder/acf_", as.character(NameCryptos[i]), ".pdf"))
  p2 <- acf(ts_residuals)
  print(p2)
  dev.off()
  ## Plotting the residuals as histrogram
  pdf(paste0("Billeder/Residuals_histrogram_", as.character(NameCryptos[i]), ".pdf"))
  p3 <- ggplot(as.data.frame(ts_residuals), aes(x = ts_residuals)) +
    geom_histogram(aes(y = ..density..), binwidth = max(ts_residuals)/50, fill = "lightblue", color = "black") +
    stat_function(
      fun = dnorm, 
      args = list(mean = mean(ts_residuals), sd = sd(ts_residuals)), 
      color = "red", 
      size = 0.5
    ) +
    labs(x = "Residuals", y = "Density") +
    theme_minimal()
  print(p3)
  dev.off()
}


# QQ-plot---------------------



for (i in 1:4){
  pdf(paste0("Billeder/qqplot_", as.character(NameCryptos[i]), ".pdf"))
  # Making time series
  ts_Training_all <- ts(Training_all[,i])
  ts_residuals <- diff(ts_Training_all)

  # Standardize the residuals
  std_residuals <- (ts_residuals - mean(ts_residuals)) / sd(ts_residuals)

  # QQ-plot
  qqnorm(std_residuals, main = "" , ylab = "Standardized Residuals")
  qqline(std_residuals, col = "black", lwd = 2)  
  dev.off()
}

#deleting nonessential global Variables------------
rm(std_residuals)
rm(ts_residuals)
rm(ts_Training_all)
rm(lag_selection)
rm(plot_Aic_lag)
rm(p)
rm(p2)
rm(p3)
















