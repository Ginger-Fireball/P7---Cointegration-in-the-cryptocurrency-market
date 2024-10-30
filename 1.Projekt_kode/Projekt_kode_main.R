#Plibrary(quantmod)
library(quantmod)
library(ggplot2)
library(dplyr)
#start dag
day_one<-"2022-10-10" #skal være tidligst 2020-04-10
#Bruges til at trække Crypto priser 



#code
source("Projekt_kode_Pull_Crypto.R")


# Plot priser
ggplot(Crypto.all.adj.pro, aes(1:length(Solana.adj))) +
geom_line(aes(y = Bitcoin, colour = "Bitcoin")) +
  geom_line(aes(y = Ethereum, colour = "Ethereum")) +
  geom_line(aes(y = Solana, colour = "Solana")) +
  geom_line(aes(y = Ripple, colour = "Ripple")) +
  labs(x = "Days", y = "Procent") +
scale_color_manual(values = c("Bitcoin" = "red", "Ethereum" = "darkgoldenrod1","Solana"= "Blue","Ripple"="Green"),
                     labels = NameCryptos,
                     name = NULL) +
  theme(legend.position = "bottom")
  