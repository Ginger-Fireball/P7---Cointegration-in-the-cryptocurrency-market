# Engel Granger test





#### Engel Granger test, testing pairwise cointegration ------------------------

# Generate all permutations of two elements
crypto_pairs <- expand.grid(First = NameCryptos, Second = NameCryptos)

# Ensure columns are treated as characters
crypto_pairs$First <- as.character(crypto_pairs$First)
crypto_pairs$Second <- as.character(crypto_pairs$Second)

# Remove cases where both elements are the same
crypto_pairs <- crypto_pairs[crypto_pairs$First != crypto_pairs$Second, ]

# Print the resulting data frame
crypto_pairs


Engel_granger <- function(x,y){
  # Run the cointegration regression 
  cointegration_model <- lm(ts_Training_all[,x] ~ ts_Training_all[,y])
  
  # Extract residuals from the regression
  residuals <- resid(cointegration_model)
  
  # Perform ADF test on residuals
  adf_result <- adf.test(residuals)
  return(adf_result$p.value)
}

# Add a new column with the p-value using mutate()
crypto_pairs <- crypto_pairs %>%
  mutate(P_Value = mapply(Engel_granger, First, Second))

crypto_pairs <- crypto_pairs %>%
  mutate(P_Value_Good = (P_Value<0.05) )
# Print the updated data frame
print(crypto_pairs)












#### Model 1 -------------------------------------------------------------------

#### Model 