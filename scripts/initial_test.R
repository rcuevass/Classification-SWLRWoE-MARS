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

# Select training and test sets
dg.train=subset(dg,FlagSample==1) 
dg.test=subset(dg,FlagSample==0)

# We check names of columns
names(dg)
# Let's choose only two variables to explore ideas
binResults_TOB<-smbinning(df=dg.train,y="FlagGB",x="TOB",p=0.05)
binResults_Bal01<-smbinning(df=dg.train,y="FlagGB",x="Bal01",p=0.05)
# Select cuts in binning
cuts_TOB<-binResults_TOB$cuts
cuts_TOB
# Select WoE associated with each bin
woe_TOB<-binResults_TOB$ivtable$WoE[1:(length(cuts_TOB)+2)]
woe_TOB
cuts_Bal01<-binResults_Bal01$cuts
cuts_Bal01
View(binResults_Bal01$ivtable)
# Select WoE associated with each bin
woe_Bal01<-binResults_Bal01$ivtable$WoE[1:(length(cuts_Bal01)+1)]
woe_Bal01

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


setLabel<-dg$FlagSample
target<-dg$FlagGB
TOB<-dg$TOB
TOB_WOE <- as.numeric(lapply(dg$TOB, BinVar_TOB))
Bal01<-dg$Bal01
Bal01_WOE <- as.numeric(lapply(dg$Bal01, BinVar_Bal01))

dh<-data.frame(TOB,Bal01,TOB_WOE,Bal01_WOE,target,setLabel)
head(dh,10)
hist(dh$target)
# Select training and test sets
# Select training and test sets
dh.train=subset(dh,setLabel==1) 
dh.test=subset(dh,setLabel==0)



dh[c("TOB_WOE","Bal01_WOE")]
unique(dh.train$target)
as.factor(dh.train$target)

LRWOE <- cv.glmnet(x = as.matrix(dh.train[c("TOB_WOE","Bal01_WOE")]),
                   y = as.factor(dh.train$target),
                   alpha = 1,family = "binomial",
                   type.measure = "auc")

coef(LRWOE,s=LRWOE$lambda.min)

scores<-predict(object = LRWOE,
                newx = as.matrix(dh.test[c("TOB_WOE","Bal01_WOE")]),
                type="response")

scores

results <- HMeasure(dh.test$target,scores)
plotROC(results)
summary(results)

dh.train$TOB.imp.mean <-
  ifelse(is.na(dh.train$TOB), median(dh.train$TOB, na.rm=TRUE),
         dh.train$TOB)

dh.test$TOB.imp.mean <-
  ifelse(is.na(dh.test$TOB), median(dh.train$TOB, na.rm=TRUE),
         dh.test$TOB)

earth_model<- earth(target ~ TOB.imp.mean + Bal01, data = dh.train,
                    degree = 1)
#earth_model<- earth(target ~ TOB + Bal01, data = dh.train,na.action = na.fail)
ls(earth_model)
earth_model$coefficients
cat(format(earth_model), "\n")


plot(earth_model)
#plotd(earth_model)

scores_earth<-predict(object = earth_model,
                      (dh.test[c("TOB.imp.mean","Bal01")]),
                      type="response")


results_earth <- HMeasure(dh.test$target,scores_earth)
plotROC(results_earth)
summary(results_earth)

results$metrics$AUC
results_earth$metrics$AUC

results$metrics$KS
results_earth$metrics$KS
