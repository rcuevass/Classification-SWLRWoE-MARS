# This script has the main intention of comparing lasso regresion
# for SWLRWoE
# with Multivariate adaptive regression splines (MARS) for 
# classification

# Be sure to have installed the following packages
# - smbinning (allows for binning of variables)
# - glment (allows for lasso regression)
# - earth (for MARS)
# - hmeasure (for metrics of performance)
#
library(smbinning)
library(glmnet)
library(earth)
library(hmeasure)
# mltools can be useful in the future
#library(mltools)
#https://stackoverflow.com/questions/6104836/splitting-a-continuous-variable-into-equal-sized-groups
#bin_data(dg.train$NonBankTradesDq06, bins=5, binType = "quantile")
#bin_data(dg.train$NonBankTradesDq01, bins=4, binType = "explicit")
#bin_data(das$wt, bins=c(-Inf, 250, 322, Inf), binType = "explicit")


# Import data; included as part of smbinning
# This is a simulated dataset based on six months of information 
# collected by a Chilean Bank whose objective
# was to develop a credit scoring model to determine the probability 
# of default within the next 12 months. 
# The target variable is FlagGB, which represents the binary status
# of default (0) and not default(1)
data("chileancredit")

# Change name of dataset for simplicity
dg<-chileancredit
rm(chileancredit)

# Remove records where target is missing
dg<-dg[which(!is.na(dg$FlagGB)),]

# We check names of columns
names(dg)
# types of vars in dataframe
str(dg)

# Let's focus first on numeric variables only
dg <- dg[,sapply(dg,is.numeric)]
vars<-names(dg)
vars

# Select training and test sets
dg.train <- subset(dg,FlagSample==1) 
dg.test <- subset(dg,FlagSample==0)

# Let's do OPTIMAL binning on predictors
binResults_TOB<-smbinning(df=dg.train,y="FlagGB",
                          x="TOB",p=0.05)
binResults_Bal01<-smbinning(df=dg.train,y="FlagGB",
                            x="Bal01",p=0.05)
binResults_MtgBal01<-smbinning(df=dg.train,y="FlagGB",
                               x="MtgBal01",p=0.05)
binResults_NonBankTradesDq01<-smbinning(df=dg.train,y="FlagGB",
                                        x="NonBankTradesDq01",p=0.05)
binResults_NonBankTradesDq02<-smbinning(df=dg.train,y="FlagGB",
                                        x="NonBankTradesDq02",p=0.05)
binResults_NonBankTradesDq03<-smbinning(df=dg.train,y="FlagGB",
                                        x="NonBankTradesDq03",p=0.05)
binResults_NonBankTradesDq04<-smbinning(df=dg.train,y="FlagGB",
                                        x="NonBankTradesDq04",p=0.05)
binResults_NonBankTradesDq05<-smbinning(df=dg.train,y="FlagGB",
                                        x="NonBankTradesDq05",p=0.05)
binResults_NonBankTradesDq06<-smbinning(df=dg.train,y="FlagGB",
                                        x="NonBankTradesDq06",p=0.05)

# Select cuts in binning for each numeric variable
cuts_TOB<-binResults_TOB$cuts
cuts_Bal01<-binResults_Bal01$cuts
cuts_MtgBal01<-binResults_MtgBal01$cuts
cuts_NonBankTradesDq01<-binResults_NonBankTradesDq01$cuts
cuts_NonBankTradesDq02<-binResults_NonBankTradesDq02$cuts
cuts_NonBankTradesDq03<-binResults_NonBankTradesDq03$cuts
cuts_NonBankTradesDq04<-binResults_NonBankTradesDq04$cuts
cuts_NonBankTradesDq05<-binResults_NonBankTradesDq05$cuts
cuts_NonBankTradesDq06<-binResults_NonBankTradesDq06$cuts

# Only the following vars have more than one cut point
cuts_TOB
cuts_Bal01
cuts_NonBankTradesDq01
cuts_NonBankTradesDq02

# Get WoE for each of the above variables
woe_TOB<-binResults_TOB$ivtable$WoE[1:(length(cuts_TOB)+2)]
sum(is.na(dg.train$TOB))
sum(is.na(dg.test$TOB))
woe_TOB

woe_Bal01<-binResults_Bal01$ivtable$WoE[1:(length(cuts_Bal01)+1)]
sum(is.na(dg.train$Bal01))
sum(is.na(dg.test$Bal01))
woe_Bal01

woe_NonBankTradesDq01<-
  binResults_NonBankTradesDq01$ivtable$WoE[1:(length(cuts_NonBankTradesDq01)+1)]
sum(is.na(dg.train$NonBankTradesDq01))
sum(is.na(dg.test$NonBankTradesDq01))
woe_NonBankTradesDq01

woe_NonBankTradesDq02<-
  binResults_NonBankTradesDq02$ivtable$WoE[1:(length(cuts_NonBankTradesDq02)+1)]
sum(is.na(dg.train$NonBankTradesDq02))
sum(is.na(dg.test$NonBankTradesDq02))
woe_NonBankTradesDq02
cuts_NonBankTradesDq02

cuts_TOB
woe_TOB
BinVar_TOB <- function(x){
  if (x<=17 & !is.na(x)) {return(-0.7662)}
  if (x > 17 & x<=30 & !is.na(x)) {return(-0.3828)}
  if (x > 30 & x<=63 & !is.na(x)) {return(0.3784)}
  if (x > 63 & !is.na(x)) {return(1.1708)}
  if (is.na(x)) {return(0.0804)}
}

cuts_Bal01
woe_Bal01
BinVar_Bal01 <- function(x){
  if (x<=406.51 & !is.na(x)) {return(0.5447)}
  if (x > 406.51 & x<=3181.91 & !is.na(x)) {return(-0.3282)}
  if (x > 3181.91 & !is.na(x)) {return(0.3059)}
  
}

cuts_NonBankTradesDq01
woe_NonBankTradesDq01
BinVar_NonBankTradesDq01 <- function(x){
  if (x<=0 & !is.na(x)) {return(1.0251)}
  if (x > 0 & x<=1 & !is.na(x)) {return(-1.2070)}
  if (x > 1 & !is.na(x)) {return(-2.3845)}
}

cuts_NonBankTradesDq02
woe_NonBankTradesDq02
BinVar_NonBankTradesDq02 <- function(x){
  if (x<=0 & !is.na(x)) {return(0.8176)}
  if (x > 0 & x<=1 & !is.na(x)) {return(-1.2022)}
  if (x > 1 & !is.na(x)) {return(-2.3176)}
}


setLabel<-dg$FlagSample
target<-dg$FlagGB
##
TOB<-dg$TOB
TOB_WOE <- as.numeric(lapply(dg$TOB, BinVar_TOB))
##
Bal01<-dg$Bal01
Bal01_WOE <- as.numeric(lapply(dg$Bal01, BinVar_Bal01))
##
NonBankTradesDq01<-dg$NonBankTradesDq01
NonBankTradesDq01_WOE <- as.numeric(lapply(dg$NonBankTradesDq01,
                                           BinVar_NonBankTradesDq01))
##
NonBankTradesDq02<-dg$NonBankTradesDq02
NonBankTradesDq02_WOE <- as.numeric(lapply(dg$NonBankTradesDq02,
                                           BinVar_NonBankTradesDq02))

#
dh<-data.frame(TOB,Bal01,NonBankTradesDq01,NonBankTradesDq02,
               TOB_WOE,Bal01_WOE,NonBankTradesDq01_WOE,NonBankTradesDq02_WOE,
               target,setLabel)

# Check head of dataframe
head(dh,10)

# Select training and test sets
dh.train=subset(dh,setLabel==1) 
dh.test=subset(dh,setLabel==0)


# Execute regression
LRWOE <- cv.glmnet(x = as.matrix(dh.train[c("TOB_WOE","Bal01_WOE",
                                            "NonBankTradesDq01_WOE",
                                            "NonBankTradesDq02_WOE")]),
                   y = as.factor(dh.train$target),
                   alpha = 1,family = "binomial",
                   type.measure = "auc")

#check coefficients
coef(LRWOE,s=LRWOE$lambda.min)


# get predicted scores
scores<-predict(object = LRWOE,
                newx = as.matrix(dh.test[c("TOB_WOE","Bal01_WOE",
                                           "NonBankTradesDq01_WOE",
                                           "NonBankTradesDq02_WOE")]),
                type="response")

# get metrics
results <- HMeasure(dh.test$target,scores)
plotROC(results)
summary(results)


# We check missing values to do imputation before MARS
summary(dh[c("TOB","Bal01",
               "NonBankTradesDq01",
               "NonBankTradesDq02")])

# only TOB variables requires imputation; we do so with median
dh.train$TOB.imp <-
  ifelse(is.na(dh.train$TOB), median(dh.train$TOB, na.rm=TRUE),
         dh.train$TOB)

dh.test$TOB.imp <-
  ifelse(is.na(dh.test$TOB), median(dh.train$TOB, na.rm=TRUE),
         dh.test$TOB)

# We use MARS
earth_model<- earth(target ~ TOB.imp + Bal01 
                    + NonBankTradesDq01
                    + NonBankTradesDq02,
                    data = dh.train,
                    degree = 1)

#earth_model<- earth(target ~ TOB + Bal01, data = dh.train,na.action = na.fail)
ls(earth_model)
earth_model$coefficients
cat(format(earth_model), "\n")


plot(earth_model)
#plotd(earth_model)


dh.test[c("TOB.imp","Bal01",
          "NonBankTradesDq01",
          "NonBankTradesDq02")]

scores_earth<-predict(object = earth_model,
                      dh.test[c("TOB.imp","Bal01",
                                 "NonBankTradesDq01",
                                 "NonBankTradesDq02")],
                      type="response")

results_earth <- HMeasure(dh.test$target,scores_earth)
plotROC(results_earth)
summary(results_earth)

results$metrics$AUC
results_earth$metrics$AUC

results$metrics$KS
results_earth$metrics$KS
