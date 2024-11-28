
ts_Training_all <- ts(Training_all)

# Engel Granger test
library(lmtest)
library(forecast)


ts_Training_all <- ts(Training_all)

for (i in 1:4){
# Run the cointegration regression 
cointegration_model <- lm(ts_Training_all[,1] ~ ts_Training_all[,2])

# Extract residuals from the regression
residuals <- resid(cointegration_model)

# Perform ADF test on residuals
adf.test(residuals)
}


pair <- c(x=1, y=2)

Engel_granger <- function(x,y){
  # Run the cointegration regression 
  cointegration_model <- lm(ts_Training_all[,x] ~ ts_Training_all[,y])
  
  # Extract residuals from the regression
  residuals <- resid(cointegration_model)
  
  # Perform ADF test on residuals
  adf_result <- adf.test(residuals)
  print(adf_result$p.value)
  if (adf_result$p.value < 0.05){
    
  }
}
EGM_ NameCryptos[] NameCryptos[] 
EGM_Bitcoin_Ethereum

paste0("EGM_", as.character(NameCryptos[1]), "_" as.character(NameCryptos[2])) <- cointegration_model












