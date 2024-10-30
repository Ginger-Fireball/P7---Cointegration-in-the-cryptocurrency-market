
# Get Bitcoin data
Crypto<-function(stock,period1,period2){
  getSymbols(stock, src = "yahoo", from = period1, to = period2, auto.assign = FALSE)
}
#after the function will the date for the first data be written
#how far you can go back in time from yahoo
Bitcoin<-Crypto("BTC-USD",day_one,"2024-10-10")#2014-09-17
Ethereum<-Crypto("ETH-USD",day_one,"2024-10-10")#2017-11-09
Solana<-Crypto("SOL-USD",day_one,"2024-10-10")# 2020-04-10
Ripple<-Crypto("XRP-USD",day_one,"2024-10-10")#2017-11-09
#lsit for names of the Cryptos
NameCryptos<-list("Bitcoin","Ethereum","Solana","Ripple")

#cheeks for NA Values and prints out if good or not
for (i in NameCryptos) {
  df<-get(i)
  if (anyNA(df)) {cat(i,"have NA values")
  }
  else{cat(i,"is good to go 
")
  }
}
NameCryptos.adj<-list()
#we will now pull out only the adjusted prices for use in Cointegation
for (i in NameCryptos) {
  df<-get(i)
assign(paste0(i,".adj"),df[,6])
NameCryptos.adj<-append(NameCryptos.adj,as.character(paste0(i,".adj")))
}
#This saves all the adjusted in one Data Frame
Crypto.all.adj<-NULL
for (i in NameCryptos.adj ){
    df<-get(i)
    Crypto.all.adj<-cbind(Crypto.all.adj,df)
}
Crypto.all.adj<-as.data.frame(Crypto.all.adj)

#this looks at makes it so we can see it in procentege 
Crypto.all.adj.pro<-NULL
for (i in 1:4) {
  Crypto.all.adj.pro<-cbind(Crypto.all.adj.pro, (Crypto.all.adj[,i]/Crypto.all.adj[1,i]))
}
Crypto.all.adj.pro<-as.data.frame(Crypto.all.adj.pro)
colnames(Crypto.all.adj.pro)<-NameCryptos

rm(df)
rm(i)

#Now they are ready to mbe imported to the main document
