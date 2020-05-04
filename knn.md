# Case2


# Libs
library(dplyr)
library(vtreat)
library(caret)
library(caret)
library(rpart.plot) 
library(randomForest)
library(MLmetrics)

setwd("/cloud/project/personalFiles")

currentData   <- read.csv('CurrentCustomerMktgResults.csv')
newDataSource <- read.csv('householdVehicleData.csv') 
Axiom <- read.csv("householdAxiomData.csv")
Credit <- read.csv("householdCreditData.csv")

joinData <- left_join(currentData, newDataSource, by = c('HHuniqueID'))
joinData <- left_join(joinData, Credit)
joinData <- left_join(joinData, Axiom)

joinData$Y_AcceptedOffer <- as.factor(joinData$Y_AcceptedOffer)

set.seed(1234)
splitPercent <- round(nrow(joinData) %*% .6)
totalRecords <- 1:nrow(joinData)
idx          <- sample(totalRecords, splitPercent)

traindat <- joinData[idx,]
testdat <- joinData[-idx,]
names(treatedTrain)

xVars <- c( "DefaultOnRecord", "RecentBalance",
            "carMake" , "HHInsurance", "CarLoan", "headOfhouseholdGender", 
            "AffluencePurchases", "Age",                     
            "Job", "Marital", "Education")

yVar  <- "Y_AcceptedOffer"

plan  <-  designTreatmentsC(dframe = joinData, varlist = xVars, 
                            outcomename = yVar, outcometarget = "Accepted")

treatedTrain <- prepare(plan, traindat)
treatedTest  <- prepare(plan, testdat)

fit <- train(yVar ~ xVars, 
             treatedTrain,
             "knn",  
             preProcess = c("center","scale"), 
             tunelength = 5)
