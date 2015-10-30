##################################################################################
## Simple look at eligibility data
## ps October 2015
##
## Data used- 
## June 2015 dump of input data and likelihood scores from MSE feed
##
#################################################################################

#################################################################################
## set up

  ## options
  options(scipen=999)

  ## locations
  homeDir = '/Users/piers.stobbs/Documents/piers/Box Sync/datascience/eligibility/'
  dataDir = '/Users/piers.stobbs/Documents/piers/sandbox/'
  #setwd = homeDir
  homeDir = getwd()
  #dataDir = '/data/'
  
  ## libraries
  library(data.table)
  library(ggplot2)
  #library(lubridate)
  #detach("package:lubridate", unload=TRUE)
  
  ## data files
  ccUsersFile = 'creditcard_Jun15users.tsv'
  ccProductsFile = 'creditcard_Jun15Products.tsv'
  

  
## end set up
#################################################################################
  
  
  
#################################################################################
## explore users
  #load data
  ccUsersRaw = fread(paste(dataDir, ccUsersFile, sep=""))
  str(ccUsersRaw)
  summary(ccUsersRaw)
  
  #clean up fields
  ccUsersRaw[, DateOfSubmission:=as.Date(DateOfSubmission, "%Y-%m-%d")]
  ccUsersRaw[, SubmissionType:=as.factor(SubmissionType)]
  ccUsersRaw[, PostcodeSector:=as.factor(PostcodeSector)]
  setnames(ccUsersRaw, "YEAR(DateOfBirth)", "BirthYear")
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "0", TimeAtCurrentAddress:=  "0 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "1", TimeAtCurrentAddress:= "1 year"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "1 years", TimeAtCurrentAddress:= "1 year"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "2", TimeAtCurrentAddress:= "2 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "3", TimeAtCurrentAddress:= "3 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "4", TimeAtCurrentAddress:= "4 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "5", TimeAtCurrentAddress:= "5 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "6", TimeAtCurrentAddress:=  "6 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "7", TimeAtCurrentAddress:= "7 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "8", TimeAtCurrentAddress:= "8 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "9", TimeAtCurrentAddress:= "9 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "10", TimeAtCurrentAddress:= "10 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "11", TimeAtCurrentAddress:= "11 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "12", TimeAtCurrentAddress:= "12 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "13", TimeAtCurrentAddress:= "13 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "14", TimeAtCurrentAddress:= "14 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "15", TimeAtCurrentAddress:= "15 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "16+", TimeAtCurrentAddress:= "16+ years"]
  
  
  ccUsersRaw[, TimeAtCurrentAddress:=as.factor(TimeAtCurrentAddress)]
  levels(ccUsersRaw$TimeAtCurrentAddress) = c("0 years", "1 year", "2 years" , "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", 
                                                 "9 years" , "10 years", "11 years", "12 years", "13 years", "14 years", "15 years", "16+ years")
  
  ccUsersRaw[, CurrentBank:=as.factor(CurrentBank)]
  ccUsersRaw[, ResidentialStatus:=as.factor(ResidentialStatus)]
  ccUsersRaw[, EmploymentStatus:=as.factor(EmploymentStatus)]
  ccUsersRaw[, ProductType:=as.factor(ProductType)]
  ccUsersRaw[, LeadProduct:=as.factor(LeadProduct)]
  ccUsersRaw[, Result:=as.factor(Result)]
  ccUsersRaw[, Source:=as.factor(Source)]
  ccUsersRaw[, Age:=2016-BirthYear]
  
  br = seq(0,200000,10000)
  lab = paste(br/1000, "k", sep="")
  ccUsersRaw[, IncomeRange:=cut(ccUsersRaw[, Income], br, lab[2:(length(lab))])]
  
  #postcodes
  ccUsersRaw[, PostcodeSector]
  rexp <- "^(\\w+)\\s?(.*)$"
  ccUsersRaw[, PostcodeArea:=as.factor(toupper(sub(rexp,"\\1", ccUsersRaw[, PostcodeSector])))]
  #try and clean up
  #trim to 4 characters
  ccUsersRaw[, PostcodeArea:=substr(PostcodeArea,1,4)]
  #trim to 3 where 4th character is not numeric
  ccUsersRaw[grepl("[^0-9]", substr(ccUsersRaw[, PostcodeArea],4,4)), PostcodeArea:=substr(PostcodeArea,1,3)]
  ccUsersRaw[, PostcodeArea:=as.factor(PostcodeArea)]
  tabulate(ccUsersRaw[, PostcodeArea])
  
  tmp = table(ccUsersRaw[, PostcodeArea])
  length(tmp[tmp>10])
  
  str(ccUsersRaw)
  summary(ccUsersRaw)
  
  #filters to use:
  # Result == Success
  # !(is.na(IncomeRange)
  cleanIdx = (ccUsersRaw[, Result] == "Success") & (!is.na(ccUsersRaw[, IncomeRange]))
  ccUsersClean = ccUsersRaw[cleanIdx, ]
  levels(ccUsersClean[, PostcodeArea])
  
  #final list
  varFactorList = c("PostcodeArea", "TimeAtCurrentAddress", "CurrentBank", 
              "ResidentialStatus", "EmploymentStatus", "ProductType", "IncomeRange")
  varNumericList = c("Income", "BirthYear", "Age")
  
    
  #### distributions
  ggplot(ccUsersClean, aes(x=TimeAtCurrentAddress)) +
    geom_histogram(binwidth=.5, colour="black", fill="white") 
  
  ggplot(ccUsersClean, aes_string(x=varFactorList[7])) +
    geom_histogram(binwidth=.5, colour="black", fill="white") 
  
  ggplot(ccUsersClean, aes_string(x=varNumericList[3])) +
    geom_density() 
  
  
  
  
  
  
## end explore users
#################################################################################
  
  
  
  
#################################################################################
## explore products
  #load data
  ccProductsRaw = fread(paste(dataDir, ccProductsFile, sep=""))
  str(ccProductsRaw)
  summary(ccProductsRaw)
  
  #clean up fields
  #clean up fields
  ccProductsRaw[, Product:=as.factor(Product)]
  ccProductsRaw[, ProductType:=as.factor(ProductType)]
  ccProductsRaw[, ProductSet:=as.factor(ProductSet)]
  ccProductsRaw[, Likelihood:=as.factor(Likelihood)]
  
  levels(ccProductsRaw$Likelihood) = c(
    "95%", "90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%", 
    "Low", "Hard fail", "Not rated"
  )

  ccProductsRaw[, LikelihoodScore:=0]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "10%", LikelihoodScore:=0.1]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "20%", LikelihoodScore:=0.2]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "30%", LikelihoodScore:=0.3]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "40%", LikelihoodScore:=0.4]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "50%", LikelihoodScore:=0.5]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "60%", LikelihoodScore:=0.6]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "70%", LikelihoodScore:=0.7]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "80%", LikelihoodScore:=0.8]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "90%", LikelihoodScore:=0.9]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "95%", LikelihoodScore:=0.95]
  
  sum(ccProductsRaw[, LikelihoodScore]==0)
  table(ccProductsRaw[, LikelihoodScore])

  ccProductsRaw
  levels(ccProductsRaw[, ProductType])

  # remove not rated
  sum(ccProductsRaw[, Likelihood] == "Not rated")
  sum(ccProductsRaw[, Likelihood] == "Low")
  sum(ccProductsRaw[, Likelihood] == "Hard fail")
  ccProductsRaw = ccProductsRaw[ccProductsRaw[, Likelihood] != "Not rated", ]
  
    
  #create agg by submissionid
  str(ccProductsRaw)
  ccProductsAgg = ccProductsRaw[, list(count = .N, 
                       med = median(LikelihoodScore), 
                       avg = mean(LikelihoodScore), 
                       min = min(LikelihoodScore), 
                       max = max(LikelihoodScore),
                       pct50 = length(LikelihoodScore[LikelihoodScore>=0.5])/.N,
                       pct80 = length(LikelihoodScore[LikelihoodScore>=0.8])/.N,
                       pct90 = length(LikelihoodScore[LikelihoodScore>=0.9])/.N,
                       leadProductCount = length(LikelihoodScore[ProductSet=="Lead Product"]),
                       leadProductAvg = sum(LikelihoodScore[ProductSet=="Lead Product"])/length(LikelihoodScore[ProductSet=="Lead Product"]),
                       zeroPctCount = length(LikelihoodScore[ProductType=="0% Spending"]),
                       zeroPctAvg = sum(LikelihoodScore[ProductType=="0% Spending"])/length(LikelihoodScore[ProductType=="0% Spending"]),
                       zeroPctMax = max(LikelihoodScore[ProductType=="0% Spending"]),
                       zeroPctMin = min(LikelihoodScore[ProductType=="0% Spending"]),
                       airlineCount = length(LikelihoodScore[ProductType=="Airline"]),
                       airlineAvg = sum(LikelihoodScore[ProductType=="Airline"])/length(LikelihoodScore[ProductType=="Airline"]),
                       airlineMax = max(LikelihoodScore[ProductType=="Airline"]),
                       airlineMin = min(LikelihoodScore[ProductType=="Airline"]),
                       allroundCount = length(LikelihoodScore[ProductType=="All-Rounder"]),
                       allroundAvg = sum(LikelihoodScore[ProductType=="All-Rounder"])/length(LikelihoodScore[ProductType=="All-Rounder"]),
                       allroundMax = max(LikelihoodScore[ProductType=="All-Rounder"]),
                       allroundMin = min(LikelihoodScore[ProductType=="All-Rounder"]),
                       badcreditCount = length(LikelihoodScore[ProductType=="Bad Credit"]),
                       badcreditAvg = sum(LikelihoodScore[ProductType=="Bad Credit"])/length(LikelihoodScore[ProductType=="Bad Credit"]),
                       badcreditMax = max(LikelihoodScore[ProductType=="Bad Credit"]),
                       badcreditMin = min(LikelihoodScore[ProductType=="Bad Credit"]),
                       balanceTransCount = length(LikelihoodScore[ProductType=="Balance Transfer"]),
                       balanceTransAvg = sum(LikelihoodScore[ProductType=="Balance Transfer"])/length(LikelihoodScore[ProductType=="Balance Transfer"]),
                       balanceTransMax = max(LikelihoodScore[ProductType=="Balance Transfer"]),
                       balanceTransMin = min(LikelihoodScore[ProductType=="Balance Transfer"]),
                       cashbackCount = length(LikelihoodScore[ProductType=="Cashback"]),
                       cashbackAvg = sum(LikelihoodScore[ProductType=="Cashback"])/length(LikelihoodScore[ProductType=="Cashback"]),
                       cashbackMax = max(LikelihoodScore[ProductType=="Cashback"]),
                       cashbackMin = min(LikelihoodScore[ProductType=="Cashback"]),
                       moneyTransCount = length(LikelihoodScore[ProductType=="Money Transfer"]),
                       moneyTransAvg = sum(LikelihoodScore[ProductType=="Money Transfer"])/length(LikelihoodScore[ProductType=="Money Transfer"]),
                       moneyTransMax = max(LikelihoodScore[ProductType=="Money Transfer"]),
                       moneyTransMin = min(LikelihoodScore[ProductType=="Money Transfer"]),
                       rewardsCount = length(LikelihoodScore[ProductType=="Rewards"]),
                       rewardsAvg = sum(LikelihoodScore[ProductType=="Rewards"])/length(LikelihoodScore[ProductType=="Rewards"]),
                       rewardsMax = max(LikelihoodScore[ProductType=="Rewards"]),
                       rewardsMin = min(LikelihoodScore[ProductType=="Rewards"]),
                       travelCount = length(LikelihoodScore[ProductType=="Travel Money"]),
                       travelMax = max(LikelihoodScore[ProductType=="Travel Money"]),
                       travelMin = min(LikelihoodScore[ProductType=="Travel Money"]),
                       travelAvg = sum(LikelihoodScore[ProductType=="Travel Money"])/length(LikelihoodScore[ProductType=="Travel Money"])
  ), by = list(SubmissionID)]
  
  
  
  
  #figure out the join - key both tables on SubmissionID
  nrow(ccProductsAgg)
  nrow(ccUsersClean)
  setkey(ccProductsAgg, SubmissionID)
  setkey(ccUsersClean, SubmissionID)

  ccJoin = ccProductsAgg[ccUsersClean, ]

  levels(ccJoin$ProductType)
  levels(ccJoin$LeadProduct)
  
  ccJoin[, requestedNum:=0]
  ccJoin[, requestedAvg:=0]
  tmpIdx = ccJoin[, ProductType] == "0% Spending"
  ccJoin[tmpIdx, requestedNum:=as.numeric(zeroPctCount)]
  ccJoin[tmpIdx, requestedAvg:=zeroPctAvg]
  ccJoin[tmpIdx, requestedMax:=zeroPctMax]
  ccJoin[tmpIdx, requestedMin:=zeroPctMin]
  tmpIdx = ccJoin[, ProductType] == "Airline"
  ccJoin[tmpIdx, requestedNum:=as.numeric(airlineCount)]
  ccJoin[tmpIdx, requestedAvg:=airlineAvg]
  ccJoin[tmpIdx, requestedMax:=airlineMax]
  ccJoin[tmpIdx, requestedMin:=airlineMin]
  tmpIdx = ccJoin[, ProductType] == "All-Rounder"
  ccJoin[tmpIdx, requestedNum:=as.numeric(allroundCount)]
  ccJoin[tmpIdx, requestedAvg:=allroundAvg]
  ccJoin[tmpIdx, requestedMax:=allroundMax]
  ccJoin[tmpIdx, requestedMin:=allroundMin]
  tmpIdx = ccJoin[, ProductType] == "Bad Credit"
  ccJoin[tmpIdx, requestedNum:=as.numeric(badcreditCount)]
  ccJoin[tmpIdx, requestedAvg:=badcreditAvg]
  ccJoin[tmpIdx, requestedMax:=badcreditMax]
  ccJoin[tmpIdx, requestedMin:=badcreditMin]
  tmpIdx = ccJoin[, ProductType] == "Balance Transfer"
  ccJoin[tmpIdx, requestedNum:=as.numeric(balanceTransCount)]
  ccJoin[tmpIdx, requestedAvg:=balanceTransAvg]
  ccJoin[tmpIdx, requestedMax:=balanceTransMax]
  ccJoin[tmpIdx, requestedMin:=balanceTransMin]
  tmpIdx = ccJoin[, ProductType] == "Cashback"
  ccJoin[tmpIdx, requestedNum:=as.numeric(cashbackCount)]
  ccJoin[tmpIdx, requestedAvg:=cashbackAvg]
  ccJoin[tmpIdx, requestedMax:=cashbackMax]
  ccJoin[tmpIdx, requestedMin:=cashbackMin]
  tmpIdx = ccJoin[, ProductType] == "Money Transfer"
  ccJoin[tmpIdx, requestedNum:=as.numeric(moneyTransCount)]
  ccJoin[tmpIdx, requestedAvg:=moneyTransAvg]
  ccJoin[tmpIdx, requestedMax:=moneyTransMax]
  ccJoin[tmpIdx, requestedMin:=moneyTransMin]
  tmpIdx = ccJoin[, ProductType] == "Rewards"
  ccJoin[tmpIdx, requestedNum:=as.numeric(rewardsCount)]
  ccJoin[tmpIdx, requestedAvg:=rewardsAvg]
  ccJoin[tmpIdx, requestedMax:=rewardsMax]
  ccJoin[tmpIdx, requestedMin:=rewardsMin]
  tmpIdx = ccJoin[, ProductType] == "Travel Money"
  ccJoin[tmpIdx, requestedNum:=as.numeric(travelCount)]
  ccJoin[tmpIdx, requestedAvg:=travelAvg]
  ccJoin[tmpIdx, requestedMax:=travelAvg]
  ccJoin[tmpIdx, requestedMin:=travelAvg]
  
  #add in an age range
  ccJoin[, ageRange]
  br = seq(0,100,10)
  ccJoin[, ageRange:=as.factor(cut(ccJoin[, Age], br))]
  
  
  
  ccJoin[ccJoin[, requestedMax]==0, ]
  
  str(ccJoin)
  
  ccProductsRaw[SubmissionID==5508331, ]
  hist(ccJoin[, requestedMax])  
  hist(ccJoin[, max])  
  
  
  save(ccJoin, file = "eligibility.Rdata")

  
  #agg by product type and age
  ccJoinAgg = ccJoin[, list(totCount = .N, 
                totRequestedNum = sum(requestedNum),
                avgRequestedNum = sum(requestedNum)/.N,
                avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                pct90Requested = sum(requestedMax>=0.9)/.N, 
                pct80Requested = sum(requestedMax>=0.8)/.N, 
                pct50Requested = sum(requestedMax>=0.5)/.N),
          by = list(ProductType, ageRange)]
  
  ccJoinAgg[, pct90Requested]
  
  ccJoinAgg[ ProductType == "Balance Transfer" & ageRange == "(10,20]", ]
  ccJoin[ ProductType == "Balance Transfer" & ageRange == "(10,20]", ]
  sum(ccJoin[ ProductType == "Balance Transfer" & ageRange == "(10,20]", requestedMax]>=0.9) / 
  nrow(ccJoin[ ProductType == "Balance Transfer" & ageRange == "(10,20]", ])  
  
  
  ggplot(ccJoinAgg, aes(x=ageRange, y=ProductType, size = log(totCount+1), colour=pct90Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  ggplot(ccJoinAgg, aes(x=ageRange, y=ProductType, size = log(totCount+1), colour=pct80Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  ggplot(ccJoinAgg, aes(x=ageRange, y=ProductType, size = log(totCount+1), colour=pct50Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  
  ggplot(ccJoinAgg, aes(x=ageRange, y=ProductType, size = log(totCount+1), colour=avgRequested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  
  #agg by product type and income
  ccJoinAgg2 = ccJoin[, list(totCount = .N, 
                            totRequestedNum = sum(requestedNum),
                            avgRequestedNum = sum(requestedNum)/.N,
                            avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                            pct90Requested = sum(requestedMax>=0.9)/.N, 
                            pct80Requested = sum(requestedMax>=0.8)/.N, 
                            pct50Requested = sum(requestedMax>=0.5)/.N),
                     by = list(ProductType, IncomeRange)]
  
  
  
  ggplot(ccJoinAgg2, aes(x=IncomeRange, y=ProductType, size = log(totCount+1), colour=pct90Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  ggplot(ccJoinAgg2, aes(x=IncomeRange, y=ProductType, size = log(totCount+1), colour=pct50Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  
  ggplot(ccJoin, aes(x=ageRange)) +
    geom_histogram() 
  
  
  ccJoin[ ageRange == "(10,20]", ]
  
  ggplot(ccJoin, aes(x=Income, y=requestedAvg)) +
    geom_point() 
  
  ggplot(ccJoin, aes(x=avg)) +
    geom_density() 
  
  ggplot(ccJoin, aes(x=max)) +
    geom_density() 
  
  ggplot(ccJoin, aes(x=max)) +
    geom_histogram(bin = 0.1) 
  
  ggplot(ccJoin, aes(x=requestedAvg)) +
    geom_density() 
  
  ## create plot dt
  library(reshape2)
  
  ccPlot = melt(ccJoin,
                 # ID variables - all the variables to keep but not split apart on
                 id.vars=c("SubmissionID", "ProductType", "IncomeRange"),
                 # The source columns
                 measure.vars=c("max", "avg", "requestedMax", "requestedAvg"),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 #variable.name="submitter",
                 value.name="score"
  )
  
  
  ggplot(ccPlot, aes(x=ProductType, y=IncomeRange)) +
    geom_point()
  
  
  ggplot(ccPlot, aes(x=score, fill=variable)) +
    geom_histogram(binwidth=.05, alpha=.5, position="dodge")
  
  ggplot(ccPlot, aes(x=score, fill=variable)) +
    geom_density(alpha=.5)
  
  
  ggplot(ccPlot[ variable %in% c("avg", "requestedAvg"), ], aes(x=score, fill=variable)) +
    geom_density(alpha=.5)
  
  ggplot(ccPlot[ variable %in% c("max", "requestedMax"), ], aes(x=score, fill=variable)) +
    geom_density(alpha=.5)
  
  ggplot(ccPlot[ variable %in% c("max", "requestedMax"), ], aes(x=score, fill=variable)) +
    geom_histogram(binwidth=.1, alpha=.5, position="identity")
  
  
  ggplot(ccPlot[ variable %in% c("max", "requestedMax"), ], aes(x=score, fill=variable)) +
    geom_density(alpha=.5) +
    facet_wrap(ProductType ~ .)
  
  ggplot(ccJoin, aes(x=requestedMax)) +
    geom_histogram(bin=0.1) 
  
  ggplot(ccJoin, aes(x=requestedAvg)) +
    geom_density() 
  
  
  
  df <- data.table( cbind(rnorm(200),rnorm(200, mean=.8),rnorm(200, mean=.9),rnorm(200, mean=1),rnorm(200, mean=.2),rnorm(200, mean=.3)),rnorm(200, mean=4),rnorm(200, mean=.5))
  setnames(df, c("w.cancer","w.normal","x.cancer","x.normal","y.cancer","y.normal","z.cancer","z.normal"))
  df_log<-log2(df) # ignore the warning with NA
  head(df_log)
  
  df$id <- 1:nrow(df)
  
  df.m <- melt(df, "id")
  df.m$grp1 <- factor(gsub("\\..*$", "", df.m$variable))
  df.m$grp2 <- factor(gsub(".*\\.", "", df.m$variable))
  
  p <- ggplot(data = df.m, aes(x=value)) + geom_density(aes(fill=grp2), alpha = 0.4)
  p <- p + facet_wrap( ~ grp1)
  p + scale_fill_brewer(palette = "Set1")
  
  
  
  
## end explore products
#################################################################################
    
  
#################################################################################
## model
    
## split into train and test
set.seed(1234)
n = nrow(homeDT)  
trainPct = 0.80 # but keep last 3 months in test as well

trainId = sort(sample(seq(1,n-3,1), floor(trainPct*n), replace=FALSE))
trainDiffId = (trainId-1)[(trainId-1)>0]

trainCar = carDT[trainId, ]
testCar = carDT[-trainId, ]
trainDiffCar = carDiffDT[trainIdDiff, ]
testDiffCar = carDiffDT[-trainIdDiff, ]
trainHome = homeDT[trainId, ]
testHome = homeDT[-trainId, ]
trainDiffHome = homeDiffDT[trainDiffId, ]
testDiffHome = homeDiffDT[-trainDiffId, ]



library(ggplot2)
library(GGally)
library(scales)

# lets explore the correlations and distributions of the variables
# matrix scatter plot

homeVars = setdiff(colnames(trainHome), "date")
carVars = setdiff(colnames(trainCar), "date")
homeVarsTgt = c("homeinsurance", "houseinsurance", "comparehomeinsurance", "sov", "share")
carVarsTgt = c("carinsurance", "cheapcarinsurance", "comparecarinsurance", "sov", "share")

ggpairs(trainHome[, homeVarsTgt, with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")

ggpairs(trainDiffHome[, homeVarsTgt, with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")


ggpairs(trainCar[, carVarsTgt, with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")

ggpairs(trainDiffCar[, carVarsTgt, with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")



ggplot(trainHome[date>'2014-01-01', ], aes(x=homeinsurance, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainHome[date>'2012-01-01', ], aes(x=sov, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainDiffHome[date>'2014-01-01', ], aes(x=homeinsurance, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainDiffHome[date>'2012-01-01', ], aes(x=sov, y=share, colour = as.factor(date))) +
  geom_point()


ggplot(trainCar[date>'2012-01-01', ], aes(x=carinsurance, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainCar[date>'2012-01-01', ], aes(x=sov, y=share, colour = as.factor(date))) +
  geom_point()


ggplot(trainDiffCar[date>'2012-01-01', ], aes(x=carinsurance, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainDiffCar[date>'2012-01-01', ], aes(x=sov, y=share, colour = as.factor(date))) +
  geom_point()

#ggpairs(trainHome[, c(colnames(rawData)[grep("bs", colnames(rawData))], "y"), with=FALSE], 
#        diag=list(continuous="density",   discrete="bar"), axisLabels="show")



#### simple modelling
library(caret)

## focus on insample to start with to save out of sample
predictionsCar = data.table(date = carDT[, date], actuals = carDT[, share],
                            mean = mean(carDT[, share]), last = c(0.2, carDT[1:(nrow(carDT)-1), share]))

predictionsCarDiff = data.table(date = carDiffDT[, date], actuals = carDiffDT[, share],
                            mean = mean(carDiffDT[, share]), last = c(0.0, carDiffDT[1:(nrow(carDiffDT)-1), share]))

predictionsHome = data.table(date = homeDT[, date], actuals = homeDT[, share],
                            mean = mean(homeDT[, share]), last = c(0.2, homeDT[1:(nrow(homeDT)-1), share]))

predictionsHomeDiff = data.table(date = homeDiffDT[, date], actuals = homeDiffDT[, share],
                                mean = mean(homeDiffDT[, share]), last = c(0.0, homeDiffDT[1:(nrow(homeDiffDT)-1), share]))


## start with a simple linear regression 
tmpModelglm = train(share ~ ., data = trainCar[, carVars, with=FALSE],
                    preProcess = c("scale", "center"),  
                    method = "glm")
tmp = predict(tmpModelglm, newdata = carDT[, carVars, with=FALSE])
predictionsCar[, glmbase := tmp]
rm(tmpModelglm)

tmpModelglm = train(share ~ ., data = trainDiffCar[, carVars, with=FALSE],
                    preProcess = c("scale", "center"),  
                    method = "glm")
tmp = predict(tmpModelglm , newdata = carDiffDT[, carVars, with=FALSE])
predictionsCarDiff[, glmbase := tmp]
rm(tmpModelglm)

testCar = genTest(predictionsCar[trainId, !"date", with=FALSE])
testCarDiff = genTest(predictionsCarDiff[trainId, !"date", with=FALSE])

# glmnet and random forest
fitControl <- trainControl(## should nortmally do 10-fold CV, will do 5 for speed
  method = "repeatedcv",
  number = 5,
  ## repeated 2 times
  repeats = 2)

tmpModelglmnet = train(share ~ ., data = trainCar[, carVars, with=FALSE],
                       preProcess = c("scale", "center"),  
                       method = 'glmnet',
                       trControl = fitControl)
tmp = predict(tmpModelglmnet, newdata = carDT[, carVars, with=FALSE])
predictionsCar[, glmnet := tmp]
rm(tmpModelglmnet)


tmpModelglmnet = train(share ~ ., data = trainDiffCar[, carVars, with=FALSE],
                       preProcess = c("scale", "center"),  
                       method = 'glmnet',
                       trControl = fitControl)
tmp = predict(tmpModelglmnet, newdata = carDiffDT[, carVars, with=FALSE])
predictionsCarDiff[, glmnet := tmp]
rm(tmpModelglmnet)


tmpModelrf = train(share ~ ., data = trainCar[, carVars, with=FALSE],
                       preProcess = c("scale", "center"),  
                       method = 'rf',
                       trControl = fitControl)
tmp = predict(tmpModelrf, newdata = carDT[, carVars, with=FALSE])
predictionsCar[, rf := tmp]
rm(tmpModelrf)


tmpModelrf = train(share ~ ., data = trainDiffCar[, carVars, with=FALSE],
                       preProcess = c("scale", "center"),  
                       method = 'rf',
                       trControl = fitControl)
tmp = predict(tmpModelrf, newdata = carDiffDT[, carVars, with=FALSE])
predictionsCarDiff[, rf := tmp]
rm(tmpModelrf)


testCar = genTest(predictionsCar[trainId, !"date", with=FALSE])
testCarDiff = genTest(predictionsCarDiff[trainDiffId, !"date", with=FALSE])



testCarOOS = genTest(predictionsCar[-trainId, !"date", with=FALSE])
testCarDiffOOS = genTest(predictionsCarDiff[-trainDiffId, !"date", with=FALSE])

library(reshape2)

tmpPlot = melt(predictionsCar[trainId, ],
               # ID variables - all the variables to keep but not split apart on
               id.vars=c("date"),
               # The source columns
               measure.vars=setdiff(colnames(predictionsCar), "date"),
               # Name of the destination column that will identify the original
               # column that the measurement came from
               variable.name="model",
               value.name="share"
)

ggplot(tmpPlot, aes(x=date, y = share, colour = model)) + geom_line()

## end model
#################################################################################
  
   

#################################################################################
## performance function

genTest = function(pred) {
  
  require(data.table)
  
  #assume input includes results from models including one column "actuals"
  models = setdiff(colnames(pred), c("actuals"))
  nModels = length(models)
  rawAD = abs(pred[, models, with=FALSE] -
                pred[, rep("actuals",nModels), with=FALSE])
  rawSD = as.data.table((pred[, models, with=FALSE] -
             pred[, rep("actuals",nModels), with=FALSE])^2)
  
  summaryDT = data.table(modelName = models)
  summaryDT[, mad:= colMeans(rawAD)]
  summaryDT[, madMin:= lapply(rawAD, min)]
  summaryDT[, madMax:= lapply(rawAD, max)]
  summaryDT[, madsd:= lapply(rawAD, sd)]
  
  summaryDT[, mse:= colMeans(rawSD)]
  summaryDT[, mseMin:= lapply(rawSD, min)]
  summaryDT[, mseMax:= lapply(rawSD, max)]
  summaryDT[, msesd:= lapply(rawSD, sd)]
  
  return(list(summaryDT,rawAD,rawSD))

}

## end performance function
#################################################################################

library(data.table)
tmp = data.table(id = seq(1,10000000,1))
tmp[, a:="char"]
tmp[, b:="char"]
tmp[, c:="char"]
tmp[, d:="char"]
tmp[, e:="char"]
tmp[, f:="char fill 2"]
tmp[, g:=0]
tmp[, h:=1]
tmp[, i:=0]
tmp[, j:=1]
tmp[, k:=0]
tmp[, l:=0]
tmp[, m:=0]
tmp[, n:=40241]
tmp[, o:=10000]

save(tmp, file="test10mrows.Rdata")
write.table(tmp, "test10mrows.txt", sep="\t", row.names=F, col.names = T)

load("test10mrows.Rdd")
tmp2 = fread("test10mrows.txt")

sort( sapply(ls(),function(x){object.size(get(x))}))

