#johansen test
source("Projekt_kode_Pull_Crypto.R")

Johan.test.crypto<-ca.jo(Crypto.all.adj.pro, type="trace", K=4, ecdet="none",spec="longrun")

print(summary(Johan.test.crypto))

vector_englegranger<-NUll
for (i in NameCryptos) {
  if ((i=="Bitcoin")==FALSE) { 
    
  }
  
  
}

# Generate all permutations of two elements
crypto_pairs <- expand.grid(First = NameCryptos, Second = NameCryptos)

# Ensure columns are treated as characters
crypto_pairs$First <- as.character(crypto_pairs$First)
crypto_pairs$Second <- as.character(crypto_pairs$Second)

# Remove cases where both elements are the same
crypto_pairs <- crypto_pairs[crypto_pairs$First != crypto_pairs$Second, ]

# Print the resulting data frame
crypto_pairs
