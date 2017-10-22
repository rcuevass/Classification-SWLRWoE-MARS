# This script has the main intention of comparing lasso regresion
# for SWLRWoE

# Be sure to have installed the following packages
# - smbinning (allows for binning of variables)
# - glment (allows for lasso regression)
# - hmeasure (for metrics of performance)
#
library(smbinning)
library(glmnet)
library(hmeasure)
# mltools can be useful in the future
#library(mltools)
#https://stackoverflow.com/questions/6104836/splitting-a-continuous-variable-into-equal-sized-groups
#bin_data(dg.train$NonBankTradesDq06, bins=5, binType = "quantile")
#bin_data(dg.train$NonBankTradesDq01, bins=4, binType = "explicit")
#bin_data(das$wt, bins=c(-Inf, 250, 322, Inf), binType = "explicit")

source("./scripts/binning_functions.R")


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

cuts_Bal01
woe_Bal01

cuts_NonBankTradesDq01
woe_NonBankTradesDq01

cuts_NonBankTradesDq02
woe_NonBankTradesDq02

# the remaining variables have one cut; it makes the binning silly
woe_NonBankTradesDq03<-
  binResults_NonBankTradesDq03$ivtable$WoE[1:(length(cuts_NonBankTradesDq03)+1)]
sum(is.na(dg.train$NonBankTradesDq03))
sum(is.na(dg.test$NonBankTradesDq03))
cuts_NonBankTradesDq03
woe_NonBankTradesDq03


woe_NonBankTradesDq04<-
  binResults_NonBankTradesDq04$ivtable$WoE[1:(length(cuts_NonBankTradesDq04)+1)]
sum(is.na(dg.train$NonBankTradesDq04))
sum(is.na(dg.test$NonBankTradesDq04))
cuts_NonBankTradesDq04
woe_NonBankTradesDq04


woe_NonBankTradesDq05<-
  binResults_NonBankTradesDq05$ivtable$WoE[1:(length(cuts_NonBankTradesDq05)+1)]
sum(is.na(dg.train$NonBankTradesDq05))
sum(is.na(dg.test$NonBankTradesDq05))
cuts_NonBankTradesDq05
woe_NonBankTradesDq05

woe_NonBankTradesDq06<-
  binResults_NonBankTradesDq06$ivtable$WoE[1:(length(cuts_NonBankTradesDq06)+1)]
sum(is.na(dg.train$NonBankTradesDq06))
sum(is.na(dg.test$NonBankTradesDq06))
cuts_NonBankTradesDq06
woe_NonBankTradesDq06

# We add all variables; including Dq03 to Dq06
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

NonBankTradesDq02<-dg$NonBankTradesDq02
NonBankTradesDq02_WOE <- as.numeric(lapply(dg$NonBankTradesDq02,
                                           BinVar_NonBankTradesDq02))

NonBankTradesDq03<-dg$NonBankTradesDq03
NonBankTradesDq03_WOE <- as.numeric(lapply(dg$NonBankTradesDq03,
                                           BinVar_NonBankTradesDq03))

NonBankTradesDq04<-dg$NonBankTradesDq04
NonBankTradesDq04_WOE <- as.numeric(lapply(dg$NonBankTradesDq04,
                                           BinVar_NonBankTradesDq04))

NonBankTradesDq05<-dg$NonBankTradesDq05
NonBankTradesDq05_WOE <- as.numeric(lapply(dg$NonBankTradesDq05,
                                           BinVar_NonBankTradesDq05))

NonBankTradesDq06<-dg$NonBankTradesDq06
NonBankTradesDq06_WOE <- as.numeric(lapply(dg$NonBankTradesDq06,
                                           BinVar_NonBankTradesDq06))

length(NonBankTradesDq02)
length(NonBankTradesDq06)
#
dh<-data.frame(TOB,Bal01,NonBankTradesDq01,NonBankTradesDq02,
               NonBankTradesDq03,NonBankTradesDq04,
               NonBankTradesDq05,NonBankTradesDq06,
               TOB_WOE,Bal01_WOE,NonBankTradesDq01_WOE,NonBankTradesDq02_WOE,
               NonBankTradesDq03_WOE,NonBankTradesDq04_WOE,
               NonBankTradesDq05_WOE,NonBankTradesDq06_WOE,
               target,setLabel)

# Check head of dataframe
head(dh,10)

# Select training and test sets
dh.train=subset(dh,setLabel==1) 
dh.test=subset(dh,setLabel==0)


# Execute regression
LRWOE <- cv.glmnet(x = as.matrix(dh.train[c("TOB_WOE","Bal01_WOE",
                                            "NonBankTradesDq01_WOE",
                                            "NonBankTradesDq02_WOE",
                                            "NonBankTradesDq03_WOE",
                                            "NonBankTradesDq04_WOE",
                                            "NonBankTradesDq05_WOE",
                                            "NonBankTradesDq06_WOE")]),
                   y = as.factor(dh.train$target),
                   alpha = 1,family = "binomial",
                   type.measure = "auc")

#check coefficients
coef(LRWOE,s=LRWOE$lambda.min)


# get predicted scores
scores<-predict(object = LRWOE,
                newx = as.matrix(dh.test[c("TOB_WOE","Bal01_WOE",
                                           "NonBankTradesDq01_WOE",
                                           "NonBankTradesDq02_WOE",
                                           "NonBankTradesDq03_WOE",
                                           "NonBankTradesDq04_WOE",
                                           "NonBankTradesDq05_WOE",
                                           "NonBankTradesDq06_WOE")]),
                type="response")

# get metrics
results <- HMeasure(dh.test$target,scores)

# Save ROC to file
jpeg('./plots/ROC_SWLRWoE_extra_silly_vars.jpg')
plotROC(results)
dev.off()

summary(results)

# Create df with metrics of performance
metric_name<-c("AUC","KS")
metric_value<-c(results$metrics$AUC,results$metrics$KS)

dfSWLRWoEMetrics <- data.frame(metric_name,metric_value)
# Save data frame to csv file
write.csv(dfSWLRWoEMetrics,file = "./results/metrics_SWLRWoE.csv")

# Let's check final list of variables
#LR
coef(LRWOE,s=LRWOE$lambda.min)
dfCoeffLR <- as.data.frame(as.matrix(coef(LRWOE,s=LRWOE$lambda.min)))
write.csv(dfCoeffLR,file = "./results/coeff_SWLRWoE.csv")
