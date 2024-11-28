library(vars)     
library(tseries)


#Number of lags in our model
lag_selection <- VARselect(Training_all, lag.max = 10, type = "const")
print(lag_selection$selection)
plot_Aic_lag<-as.data.frame(t(lag_selection$criteria))


ggplot(plot_Aic_lag, aes(x = 1:length(plot_Aic_lag[,1])),
       y = plot_Aic_lag[,1] ) +
  geom_point(aes(y = plot_Aic_lag[,1], colour = "darkred"),size = 2) +
  labs(x = "Lags", y = "AIC score") + 
  theme_minimal()+ 
  theme(legend.position = "none") 
#we use AIC and it says 3 


#to check if they are stationary  
ts_Training_all<-ts(Training_all)
adf.test(ts_Training_all[, "Bitcoin"])
adf.test(ts_Training_all[, "Ethereum"])
adf.test(ts_Training_all[, "Solana"])
adf.test(ts_Training_all[, "Ripple"])


