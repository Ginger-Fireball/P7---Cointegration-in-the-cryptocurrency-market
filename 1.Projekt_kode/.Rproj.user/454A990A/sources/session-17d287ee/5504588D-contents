library(vars)     
library(tseries)


#Number of lags in our model
lag_selection <- VARselect(ts_Training_all, lag.max = 100, type = "const")
print(lag_selection$selection)
ggplot(lag_selection$criteria, aes(x = 1:length(lag_selection$criteria[1,])),
       y = lag_selection$criteria[1,] ) +
  geom_line(y = lag_selection$criteria[1,], colour = "Bitcoin" ) +
scale_color_manual(values = c("Bitcoin" = "red", "Ethereum" = "darkgoldenrod1","Solana"= "blue","Ripple"="green"),
labels = NameCryptos, name = NULL) +
theme(legend.position = "bottom")


lag_selection$criteria
#we use AIC and it says 3 
length(lag_selection)
View(lag_selection)

ts_Training_all<-ts(Training_all)
adf.test(ts_Training_all[, "BTC.USD.Adjusted"])
adf.test(ts_Training_all[, "ETH.USD.Adjusted"])
adf.test(ts_Training_all[, "SOL.USD.Adjusted"])
adf.test(ts_Training_all[, "XRP.USD.Adjusted"])

NameCryptos[2]
as.character(2983)
str(23)
1:4
lag_selection$criteria[1,]
View(lag_selection$criteria)


