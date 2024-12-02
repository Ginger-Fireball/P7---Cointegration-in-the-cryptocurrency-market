#johansen test
source("Projekt_kode_Pull_Crypto.R")


VARselect(ts_Training_all, lag.max = 10, type = "none")

Johan.test.crypto <- ca.jo(ts_Training_all, type="trace", K=5, ecdet="const", spec="longrun")

print(summary(Johan.test.crypto))
#
