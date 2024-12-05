cat("\033[31mRULES FOR THE CODE IF YOU WANT TO CHANGE ANYTHING 

1. REMOVE ALL EXTRA GLOBAL VARIBELS YOU WONT BE 
   NEEDING AND YOU DONT THINK EVER WILL BE NEEDED
   
2. DONT YOU  DARE FUCKING TOUCH MY CODE\033[0m\n" )
library(quantmod)
library(ggplot2)
library(dplyr)
library(urca)
library(vars)     
library(tseries)
library(lmtest)
library(forecast)
library(tidyverse)
library(tsDyn)
library(cowplot)
library(rlang)
set.seed(420)
#start dag
first_day <- "2020-12-10" #skal være tidligst 2020-04-10
last_day <- "2024-10-10"
#Bruges til at trække Crypto priser 



#code to bring in the data and refine it into one data frame + check for NA values
source("Projekt_kode_Pull_Crypto.R")
#

source("Plots1.R")


source("Engel_Granger.R")



source("Tests_in_Code.R")
  


