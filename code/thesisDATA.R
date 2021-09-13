######### data collection for intervention

## data feature:
## collect: - machine index
##          - failure indicator
##          - define perfect remedy indicator
##          - define root cause remedied by perfect remedy
##          - pick the first time or early time data for each machine 
##          - create dataset with 20 machines, 500 data in total?
##          - check failure times to ensure the prediction given intervention make sense
## create test data, second or later with a failure!
## because routine intervention no need to predict
D <- read.csv("~/Documents/PhD Paper 2/data/fulldfsyn.csv")
D <- D[,-1]
tm = read.csv("~/Documents/PhD Paper 2/data/timemachine2.csv")
tm = tm[,-1]
dim(tm) ## 111*2
## restore the 111 documents
restore = rep(0,111)
for(i in 1:111){
  restore[i] = which(number == i)[1]
}
restore.doc = D[restore,]
restore.doc$machine = tm$machine ## machine index added
#write.csv(restore.doc,"~/Documents/PhD project/data/restoredata.csv")
restoredata <- read.csv("~/Documents/PhD project/data/restoredata.csv")
restoredata <- restoredata[,-1]
restoretrain <- restoredata[which(restoredata$pick=="train"),] 
ftimetrain <- tm$ftime[which(restoredata$pick=="train")]
restoretest <- restoredata[which(restoredata$pick=="test"),]
ftimetest <-tm$ftime[which(restoredata$pick=="test")]
#shuffle <- sample(1:20,500,replace = TRUE)
#for(i in 1:20){
#  print(length(which(shuffle == i)))
#}
length(which(restoretrain$revise.fail == 1))
## 16 fail/remedial intervention
## 4 no fail/routine intervention
length(which(restoretrain$remedy=="routine")) # 4
length(which(restoretrain$remedy=="perfect")) # 4 perfect remedy
length(which(restoretrain$remedy=="random")) # 12 random remedy
restoretrain$ID = 1:20
unique(restoretrain$action) ## 9 possible values of actions
unique(restoretrain$action[restoretrain$remedy=="random"]) ## 5
## 5 possible random remedy action,
randomremdyID = matrix(c("add oil","2","setting oil level","tighten valve","order",1:5),ncol=2)
otherremedyID = matrix(c("tighten sight glass screw","check oil level","reset","maintenance",6:9),ncol=2)
allremedyID = rbind(randomremdyID,otherremedyID)
indexhold = rep(0,20)
for(i in 1:20){
  indexhold[i]=which(allremedyID[,1] == restoretrain$action[i])
}
restoretrain$remedyID = indexhold
trainDF <- restoretrain[shuffle,]
length(which(trainDF$revise.fail == 1)) # 404
length(which(trainDF$remedy=="routine")) # 96
length(which(trainDF$remedy=="perfect")) # 91 perfect remedy
length(which(trainDF$remedy=="random"))  # 313
rownames(trainDF) = {}
# write.csv(trainDF,"Desktop/thesis pack/data/traindata500.csv")
trainD <- matrix(c(trainDF$system,trainDF$seal,trainDF$temperature,
                   trainDF$level,trainDF$leak,trainDF$alarm,trainDF$pipe,
                   trainDF$sight.glass,trainDF$buchholz,trainDF$transformer,
                   trainDF$ID,as.numeric(trainDF$remedy) - 1,trainDF$remedyID),ncol = 13)
# 2:random remedy,3:routine,1:perfect
## 500*13
trainCORE <- trainD[,1:10]
trainID <- trainD[,11]
trainINTVT <- trainD[,12]
trainREMEDY <- trainD[,13]
trainTIME <- ftimetrain[shuffle]/365
trainDF$failtime/365
mean(trainTIME) ## 0.81
sd(trainTIME) ## 0.88
zeta = rep(1.5,21)
g = c(1,3/5)


restoretest$ID = 1:20
testDF <- restoretest[shuffle,]
testD <- matrix(c(testDF$system,testDF$seal,testDF$temperature,
                  testDF$level,testDF$leak,testDF$alarm,testDF$pipe,
                  testDF$sight.glass,testDF$buchholz,testDF$transformer,
                  testDF$ID),ncol = 11)
#write.csv(testD,"~/Documents/PhD project/data/testdata500inference.csv")
#testD <- read.csv("~/Documents/PhD project/data/testdata500inference.csv")
testCORE <- testD[,1:10]
testID <- testD[,11]
testTIME <- ftimetest[shuffle]/365
timeout <- data.frame(train = trainTIME,test = testTIME)


