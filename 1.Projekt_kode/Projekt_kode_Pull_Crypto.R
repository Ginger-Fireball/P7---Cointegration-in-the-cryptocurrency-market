
# Get Bitcoin data
Crypto<-function(stock,period1,period2){
  getSymbols(stock, src = "yahoo", from = period1, to = period2, auto.assign = FALSE)
}
#after the function will the date for the first data be written
#how far you can go back in time from yahoo
Bitcoin <- Crypto("BTC-USD",day_one,"2024-10-10")#2014-09-17
Ethereum <- Crypto("ETH-USD",day_one,"2024-10-10")#2017-11-09
Solana <- Crypto("SOL-USD",day_one,"2024-10-10")# 2020-04-10
Ripple <- Crypto("XRP-USD",day_one,"2024-10-10")#2017-11-09
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
Training_all <- tail(Crypto_all_adj, n = Training_size)
Validation_all <- head(Crypto_all_adj, n = validation_size)

#this looks at makes it so we can see it in procentege 
Training_all_pro <- NULL
for (i in 1:4) {
  Training_all_pro<-cbind(Training_all_pro, (Training_all[,i]/Training_all[1,i]))
}
Training_all_pro<-as.data.frame(Training_all_pro)
colnames(Training_all_pro)<-NameCryptos

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
