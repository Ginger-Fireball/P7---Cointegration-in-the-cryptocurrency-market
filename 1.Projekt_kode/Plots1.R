#I(1) and stationary----------------- 
# Plotting prices in levels, with each Crypto having it's own plot
for (i in 1:4) {
  # Open pdf device
  pdf(paste0("Billeder/Crypto_in_levels_", as.character(NameCryptos[i]), ".pdf"))
  # Create the plot
  p <- ggplot(Training_all, aes(x = 1:nrow(Training_all), y = Training_all[, i])) +
    geom_line(aes(colour = as.character(NameCryptos[i]))) +
    labs(x = "Days", y = "Price in USD") +
    scale_color_manual(
      values = c("Bitcoin" = "red", "Ethereum" = "darkgoldenrod1", "Solana" = "blue", "Ripple" = "green"),
      labels = as.character(NameCryptos[i]),
      name = NULL
    ) +
    theme_minimal()+
    theme( legend.background = element_rect(fill = "grey",linetype = "solid",colour ="grey"),
           #legend.box.background = element_rect(color= NA),
      legend.position = c(.1, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      #legend.key.height = unit(2, 'cm'),
      legend.text = element_text(size=10)
    )
  # Print to pdf device
  print(p)
  # Close pdf device
  dev.off()
}


#to check if they are stationary  with Argmented dickifuller test
ts_Training_all<-ts(Training_all)
adf.test(ts_Training_all[, "Bitcoin"])
adf.test(ts_Training_all[, "Ethereum"])
adf.test(ts_Training_all[, "Solana"])
adf.test(ts_Training_all[, "Ripple"])


# Plotting the diff in two ways and the acf plot
for (i in 1:4){
  # Making the residuals
  ts_diffed <- diff(ts(Training_all[,i]))
  df_ts_diffed<-as.data.frame(ts_diffed)
  df_ts_diffed$x <- as.numeric(df_ts_diffed$x)
  # plotting and saving them as pdf's:
  
  ## Plotting the residual
  pdf(paste0("Billeder/Residuals_", as.character(NameCryptos[i]), ".pdf"),width = 240,height = 100)
  p1 <- ggplot(df_ts_diffed, aes(x = 1:length(x))) +
    geom_line(aes(y=x)) +
 labs(x = "Time", y = "Differenced") +
  theme_minimal()
  print(p1)
  dev.off()
  ## Plotting the acf
  pdf(paste0("Billeder/acf_", as.character(NameCryptos[i]), ".pdf"))
  p2 <- ggAcf(ts_diffed) +
  theme_minimal() +
    geom_segment(size = 1.3) +
    labs(title = NULL)
  print(p2)
  dev.off()
  ## Plotting the residuals as histrogram
  pdf(paste0("Billeder/Residuals_histrogram_", as.character(NameCryptos[i]), ".pdf"))
  p3 <- ggplot(df_ts_diffed, aes(x = x)) +
    geom_histogram(aes(y = ..density..), binwidth = max(df_ts_diffed$x)/50, fill = "lightblue", color = "black") +
    stat_function(
      fun = dnorm, 
      args = list(mean = mean(df_ts_diffed$x), sd = sd(df_ts_diffed$x)), 
      color = "red", 
      size = 0.5
    ) +
    labs(x = "Differenced", y = "Density") +
    theme_minimal()
  print(p3)
  dev.off()
  
  
  pdf(paste0("Billeder/plot_grid_", as.character(NameCryptos[i]), ".pdf"))
  pic<-plot_grid(
    plot_grid(p3, p2, ncol = 2 ),   # Top row: p1 and p2 side by side
    p1,                            # Second row: p3 spans the entire width
    ncol = 1,                      # Stack top row and p3 vertically
    rel_heights = c(1, 1)        # Adjust height proportions (optional)
  )
  print(pic)
  dev.off()
}

############################ Check error terms are good--------------

#Number of lags in our model
lag_selection <- VARselect(diff(ts_Training_all), lag.max = 10, type = "const")

print(lag_selection$selection)

plot_Aic_lag<-as.data.frame(t(lag_selection$criteria))

pdf("Billeder/Crypto_lags.pdf")

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
VAR_lag9 <- VAR(diff(ts_Training_all), p = as.numeric(lag_selection$selection[1]))
#we use AIC

Residuals_BTC <- residuals(VAR_lag9$varresult$Bitcoin)
Residuals_ETH <- residuals(VAR_lag9$varresult$Ethereum)
Residuals_XRP <- residuals(VAR_lag9$varresult$Ripple)
Residuals_SOL <- residuals(VAR_lag9$varresult$Solana)
Name_Residuals_Cryptos<-c("Residuals_BTC","Residuals_ETH","Residuals_SOL","Residuals_XRP")


  
# Ljung-box test
Box.test(Residuals_BTC, lag = 9, type = "Ljung-Box")
Box.test(Residuals_ETH, lag = 9, type = "Ljung-Box")
Box.test(Residuals_XRP, lag = 9, type = "Ljung-Box")
Box.test(Residuals_SOL, lag = 9, type = "Ljung-Box")

#i<-"Residuals_BTC"
for (i in Name_Residuals_Cryptos){
  # Making the residuals
  df<-as.data.frame(get(i))
  colnames(df)<-i
  # plotting and saving them as pdf's:
  
  ## Plotting the residual
  #pdf(paste0("Billeder/Residuals_", as.character(NameCryptos[i]), ".pdf"),width = 240,height = 100)
  p1 <- ggplot(df, aes(x = 1:length(Residuals_BTC))) +
    geom_line(aes(y=Residuals_BTC)) +
    labs(x = "Time", y = "Residuals") +
    theme_minimal()
  print(p1)
  #dev.off()
  ## Plotting the acf
  #pdf(paste0("Billeder/acf_", as.character(i), ".pdf"))
  p2 <- ggAcf(df) +
    theme_minimal() +
    geom_segment(size = 1.3) +
    labs(title = NULL)
  print(p2)
  #dev.off()
  ## Plotting the residuals as histrogram
  #pdf(paste0("Billeder/Residuals_histrogram_", as.character(NameCryptos[i]), ".pdf"))
  p3 <- ggplot(df, aes(x = Residuals_BTC)) +
    geom_histogram(aes(y = ..density..), binwidth = max(Residuals_BTC)/50, fill = "lightblue", color = "black") +
    stat_function(
      fun = dnorm, 
      args = list(mean = mean(Residuals_BTC), sd = sd(Residuals_BTC)), 
      color = "red", 
      size = 0.5
    ) +
    labs(x = "Residuals", y = "Density") +
    theme_minimal()
  print(p3)
  dev.off()
  
  
  pdf(paste0("Billeder/plot_grid_Residuals", as.character(i), ".pdf"))
  pic<-plot_grid(
    plot_grid(p3, p2, ncol = 2 ),   # Top row: p1 and p2 side by side
    p1,                            # Second row: p3 spans the entire width
    ncol = 1,                      # Stack top row and p3 vertically
    rel_heights = c(1, 1)        # Adjust height proportions (optional)
  )
  print(pic)
  dev.off()
}




# Serial test
serial.test(VAR_lag9)

# ARCH test
arch.test(VAR_lag9)

# Normality test
normality.test(VAR_lag9)




# QQ-plot---------------------
# Create the QQ-plot
j<-1
for (i in Name_Residuals_Cryptos){
  # Making the residuals
  df<-as.data.frame(get(i))
  colnames(df)<-i

# Standardizing the residuals
standardized_residuals <- (df[,1] - mean(df[,1])) / sqrt(var(df[,1]))

# Creating a data frame for ggplot
data <- data.frame(Residuals = standardized_residuals)

# Generating the QQ plot
pdf(paste0("Billeder/QQ-Plot_of_",NameCryptos[j],"_Residuals_plot.pdf"))
p<-ggplot(data, aes(sample = Residuals)) +
  ggtitle(paste0("QQ-Plot of ",NameCryptos[j]," Residuals")) +
  geom_qq(aes(colour = "QQ-sample")) +
  geom_qq_line(aes(colour = "QQ-theo")) + 
  theme_minimal() +
  
  scale_color_manual(
    values = c("QQ-sample" = "black","QQ-theo"="red" ),
    labels = as.character(c("Sample Quantiles","theoretical Quantiles")),
    name = NULL
  ) +
  
  
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  
  
  
  theme( legend.background = element_rect(fill = "grey",linetype = "solid",colour ="grey"),
         legend.position = c(.05, .95),
         legend.justification = c("left", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6),
         #legend.key.height = unit(2, 'cm'),
         legend.text = element_text(size=10)
  ) 
print(p)
dev.off()
j<-j+1
}


#deleting nonessential global Variables------------
rm(std_residuals)
rm(df_ts_diffed)
rm(tsu_residuals)
rm(ts_diffed)
#rm(lag_selection)
#rm(plot_Aic_lag)
rm(p)
rm(pic)
rm(p1)
rm(p2)
rm(p3)
rm(i)
rm(j)
#bliver ikke brugt 
# Plotting prices in percentage all in one graf (changes from the day before to today)
#ggplot(Training_all_pro, aes(x = 1:nrow(Training_all_pro), y = Training_all_pro)) +
#  geom_line(aes(y = Bitcoin, colour = "Bitcoin")) +
#  geom_line(aes(y = Ethereum, colour = "Ethereum")) +
#  geom_line(aes(y = Solana, colour = "Solana")) +
#  geom_line(aes(y = Ripple, colour = "Ripple")) +
#  labs(x = "Days", y = "Procent") +
#  scale_color_manual(values = c("Bitcoin" = "red", "Ethereum" = "darkgoldenrod1","Solana"= "blue","Ripple"="green"),
#                     labels = NameCryptos,
#                     name = NULL) +
#  theme(legend.position = "bottom")

