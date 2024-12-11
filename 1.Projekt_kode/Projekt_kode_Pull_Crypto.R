
# Get Bitcoin data
Crypto<-function(stock,period1,period2){
  getSymbols(stock, src = "yahoo", from = period1, to = period2, auto.assign = FALSE)
}
#after the function will the date for the first data be written
#how far you can go back in time from yahoo
Bitcoin <- Crypto("BTC-USD",first_day, last_day)#2014-09-17
Ethereum <- Crypto("ETH-USD",first_day, last_day)#2017-11-09
Solana <- Crypto("SOL-USD",first_day, last_day)# 2020-04-10
Ripple <- Crypto("XRP-USD",first_day, last_day)#2017-11-09
#lsit for names of the Cryptos
NameCryptos <- list("Bitcoin","Ethereum","Solana","Ripple")

#cheeks for NA Values and prints out if good or not
for (i in NameCryptos) {
  df <- get(i)
  if (anyNA(df)) {cat(i, "have NA values")
  }
  else{cat(i, "is good to go 
")
  }
}

NameCryptos_adj<-list()
#we will now pull out only the adjusted prices for use in Cointegation
for (i in NameCryptos) {
  df <- get(i)
assign(paste0(i,".adj"),df[,6])
NameCryptos_adj <- append(NameCryptos_adj, as.character(paste0(i,".adj")))
}
#This saves all the adjusted in one Data Frame
Crypto_all_adj<-NULL
for (i in NameCryptos_adj ){
    df<-get(i)
    Crypto_all_adj<-cbind(Crypto_all_adj,df)
}
colnames(Crypto_all_adj) <- NameCryptos
Crypto_all_adj <- as.data.frame(Crypto_all_adj)

# Making Training and validation data set
Training_size <- round(0.9*nrow(Crypto_all_adj))
validation_size <- nrow(Crypto_all_adj) - Training_size

# Splitting the data
Training_all <- head(Crypto_all_adj, n = Training_size)
Validation_all <- tail(Crypto_all_adj, n = validation_size)

#this looks at makes it so we can see it in procentege 
Training_all_pro <- NULL
for (i in 1:4) {
  Training_all_pro<-cbind(Training_all_pro, (Training_all[,i]/Training_all[1,i]))
}
Training_all_pro<-as.data.frame(Training_all_pro)
colnames(Training_all_pro)<-NameCryptos

# Converting to timeseries
ts_Training_all <- ts(Training_all)












# Making VAR model 
Var_lag <- VARselect(diff(ts_Training_all), lag.max = 10, type = "none")
Var_lag

pdf(paste0("Billeder/AIC_diffed_VAR.pdf"))
plot(Var_lag$criteria["AIC(n)",], ylab = "AIC score", xlab = "lag order")
dev.off()

VAR_lag9 <- VAR(diff(ts_Training_all), p = 9)
VAR_lag9

# Residuals of VAR model
Residuals_BTC <- residuals(VAR_lag9$varresult$Bitcoin)
Residuals_ETH <- residuals(VAR_lag9$varresult$Ethereum)
Residuals_XRP <- residuals(VAR_lag9$varresult$Ripple)
Residuals_SOL <- residuals(VAR_lag9$varresult$Solana)



# Ljung-box test
Box.test(Residuals_BTC, lag = 9, type = "Ljung-Box")
Box.test(Residuals_ETH, lag = 9, type = "Ljung-Box")
Box.test(Residuals_XRP, lag = 9, type = "Ljung-Box")
Box.test(Residuals_SOL, lag = 9, type = "Ljung-Box")


# Serial test
serial.test(VAR_lag9)

# ARCH test
arch.test(VAR_lag9)

normality.test(VAR_lag9)


# Create the QQ-plot
qqnorm(Residuals_BTC, main = "QQ-Plot of Bitcoin Residuals")
qqline(Residuals_BTC, col = "red", lwd = 2)

qqnorm(Residuals_ETH, main = "QQ-Plot of Ethereum Residuals")
qqline(Residuals_ETH, col = "red", lwd = 2)

qqnorm(Residuals_XRP, main = "QQ-Plot of Ripple Residuals")
qqline(Residuals_XRP, col = "red", lwd = 2)

qqnorm(Residuals_SOL, main = "QQ-Plot of Solana Residuals")
qqline(Residuals_SOL, col = "red", lwd = 2)


#Removes all extra global variables so it doesn't get to crowded
rm(df)
rm(i)
for (i in NameCryptos_adj) {
  rm(list = i, envir = .GlobalEnv)
}

for (i in NameCryptos) {
  rm(list = i, envir = .GlobalEnv)
}
rm(NameCryptos_adj)
#Now they are ready to be imported to the main document

