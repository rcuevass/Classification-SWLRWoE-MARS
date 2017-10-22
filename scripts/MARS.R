# Multivariate adaptive regression splines (MARS) for 
# classification

# - earth (for MARS)
# - hmeasure (for metrics of performance)
# - smbinning (for binning; only used to get the data in this case)
#
library(smbinning)
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

# 
setLabel<-dg$FlagSample
target<-dg$FlagGB
##
TOB<-dg$TOB
##
Bal01<-dg$Bal01
##
NonBankTradesDq01<-dg$NonBankTradesDq01
#
NonBankTradesDq02<-dg$NonBankTradesDq02

NonBankTradesDq03<-dg$NonBankTradesDq03

NonBankTradesDq04<-dg$NonBankTradesDq04


NonBankTradesDq05<-dg$NonBankTradesDq05

NonBankTradesDq06<-dg$NonBankTradesDq06


#
dh<-data.frame(TOB,Bal01,NonBankTradesDq01,NonBankTradesDq02,
               NonBankTradesDq03,NonBankTradesDq04,
               NonBankTradesDq05,NonBankTradesDq06,
               target,setLabel)

# Check head of dataframe
head(dh,10)

# Select training and test sets
dh.train=subset(dh,setLabel==1) 
dh.test=subset(dh,setLabel==0)

# only TOB variables requires imputation; we do so with median but his can be changed to
# mean
dh.train$TOB.imp <-
  ifelse(is.na(dh.train$TOB), median(dh.train$TOB, na.rm=TRUE),
         dh.train$TOB)

dh.test$TOB.imp <-
  ifelse(is.na(dh.test$TOB), median(dh.train$TOB, na.rm=TRUE),
         dh.test$TOB)

# We use MARS
earth_model<- earth(target ~ TOB.imp + Bal01 
                    + NonBankTradesDq01
                    + NonBankTradesDq02 + NonBankTradesDq03
                    + NonBankTradesDq04
                    + NonBankTradesDq05
                    + NonBankTradesDq06,
                    data = dh.train,
                    degree = 1)

#earth_model<- earth(target ~ TOB + Bal01, data = dh.train,na.action = na.fail)
ls(earth_model)

cat(format(earth_model), "\n")
earth_model$coefficients



#plot(earth_model)
#plotd(earth_model)


scores_earth<-predict(object = earth_model,
                      dh.test[c("TOB.imp","Bal01",
                                "NonBankTradesDq01",
                                "NonBankTradesDq02",
                                "NonBankTradesDq03",
                                "NonBankTradesDq04",
                                "NonBankTradesDq05",
                                "NonBankTradesDq06")],
                      type="response")

results_earth <- HMeasure(dh.test$target,scores_earth)

# Save ROC to file
jpeg('./plots/ROC_MARS_allVars.jpg')
plotROC(results_earth)
dev.off()
summary(results_earth)

# Create df with metrics of performance
metric_name<-c("AUC","KS")
metric_value<-c(results_earth$metrics$AUC,results_earth$metrics$KS)

dfMARSMetrics <- data.frame(metric_name,metric_value)
# Save data frame to csv file
write.csv(dfMARSMetrics,file = "./results/metrics_MARS.csv")


# Let's check final list of variables
# MARS
earth_model$coefficients
dfCoeffMARS <- as.data.frame(as.matrix(earth_model$coefficients))
write.csv(dfCoeffMARS,file = "./results/coeff_MARS.csv")

