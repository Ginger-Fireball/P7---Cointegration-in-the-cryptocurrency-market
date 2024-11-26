# Plot priser
ggplot(Crypto.all.adj.pro, aes(1:length(Bitcoin))) +
  geom_line(aes(y = Bitcoin, colour = "Bitcoin")) +
  geom_line(aes(y = Ethereum, colour = "Ethereum")) +
  geom_line(aes(y = Solana, colour = "Solana")) +
  geom_line(aes(y = Ripple, colour = "Ripple")) +
  labs(x = "Days", y = "Procent") +
  scale_color_manual(values = c("Bitcoin" = "red", "Ethereum" = "darkgoldenrod1","Solana"= "Blue","Ripple"="Green"),
                     labels = NameCryptos,
                     name = NULL) +
  theme(legend.position = "bottom")