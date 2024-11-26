
# Plotting prices in percentage all in one graf (changes from the day before to today)
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


# Plotting prices in levels, with each crypto having it's own plot
for (i in 1:4) {
  # Open pdf device
  pdf(paste0("Billeder/Crypto_in_levels_", as.character(NameCryptos[i]), ".pdf"))
  # Create the plot
  p <- ggplot(Crypto.all.adj, aes(x = 1:nrow(Crypto.all.adj), y = Crypto.all.adj[, i])) +
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



