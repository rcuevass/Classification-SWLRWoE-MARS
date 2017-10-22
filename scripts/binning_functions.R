# script containing functions for binning

BinVar_TOB <- function(x){
  if (x<=17 & !is.na(x)) {return(-0.7662)}
  if (x > 17 & x<=30 & !is.na(x)) {return(-0.3828)}
  if (x > 30 & x<=63 & !is.na(x)) {return(0.3784)}
  if (x > 63 & !is.na(x)) {return(1.1708)}
  if (is.na(x)) {return(0.0804)}
}

BinVar_Bal01 <- function(x){
  if (x<=406.51 & !is.na(x)) {return(0.5447)}
  if (x > 406.51 & x<=3181.91 & !is.na(x)) {return(-0.3282)}
  if (x > 3181.91 & !is.na(x)) {return(0.3059)}
  
}

BinVar_NonBankTradesDq01 <- function(x){
  if (x<=0 & !is.na(x)) {return(1.0251)}
  if (x > 0 & x<=1 & !is.na(x)) {return(-1.2070)}
  if (x > 1 & !is.na(x)) {return(-2.3845)}
}

BinVar_NonBankTradesDq02 <- function(x){
  if (x<=0 & !is.na(x)) {return(0.8176)}
  if (x > 0 & x<=1 & !is.na(x)) {return(-1.2022)}
  if (x > 1 & !is.na(x)) {return(-2.3176)}
}

# Adding silly bining vars

BinVar_NonBankTradesDq03 <- function(x){
  if (x<=0 & !is.na(x)) {return(0.7385)}
  if (x > 0 & !is.na(x)) {return(-1.7681)}
}

BinVar_NonBankTradesDq04 <- function(x){
  if (x<=0 & !is.na(x)) {return(0.5849)}
  if (x > 0 & !is.na(x)) {return(-1.6487)}
}

BinVar_NonBankTradesDq05 <- function(x){
  if (x<=0 & !is.na(x)) {return(0.5063)}
  if (x > 0 & !is.na(x)) {return(-1.5795)}
}

BinVar_NonBankTradesDq06 <- function(x){
  if (x<=0 & !is.na(x)) {return(0.3931)}
  if (x > 0 & !is.na(x)) {return(-1.4648)}
}


