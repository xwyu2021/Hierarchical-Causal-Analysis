library(bnlearn)
library(Rgraphviz)
########################################################
############ construct the global net ##################
########################################################

sortdata <- read.csv("~/Documents/TEXTDATA/sortedtext2CEvalue.csv")
sortvar <- read.csv("~/Documents/TEXTDATA/sortedtext2CEvar.csv")
var.names <- unique(c(sortvar$cause,sortvar$effect))  ## 14 vars
# remove redundant lines
rmline <- c()
rmline2 <- c()
for(i in 1:dim(sortdata)[1]){
  if(sortdata$cause[i]==sortdata$effect[i]){
    rmline = c(rmline,i)
  }
  if(sortvar$cause[i] == sortvar$effect[i]){
    rmline2 = c(rmline2,i)
  }
}
rms <- unique(c(rmline,rmline2)) ## 38
sortdata1 <- sortdata[-rms,]
sortvar1 <- sortvar[-rms,]
datamt <- matrix("na",nrow=length(unique(sortvar1$doc_ind)),ncol=length(var.names))
for(i in 1:length(unique(sortvar1$doc_ind))){
  lines <- which(sortvar1$doc_ind==unique(sortvar1$doc_ind)[i])
  for(j in 1:length(lines)){
    var1<-sortvar1$cause[lines[j]]
    var2<- sortvar1$effect[lines[j]]
    level1 <- sortdata1$cause[lines[j]]
    level2 <- sortdata1$effect[lines[j]]
    datamt[i,which(var.names== var1)] <- level1
    datamt[i,which(var.names == var2)] <- level2
  }
}

datatb <- data.frame(temperature=datamt[,9],oilindicator=datamt[,7],contact=datamt[,4],float=datamt[,14],othercause = datamt[,1],
                     oillevel=datamt[,5],oilincor=datamt[,6],leak=datamt[,12],breather=datamt[,3],control=datamt[,2],
                     buchholz=datamt[,13],alarm=datamt[,10],transformer=datamt[,8])
unique(datatb$temperature)
unique(datatb$oilindicator)
unique(datatb$contact)
unique(datatb$float)
unique(datatb$othercause)
unique(datatb$oillevel)
unique(datatb$oilincor)
unique(datatb$leak)
unique(datatb$breather)
unique(datatb$control)
unique(datatb$buchholz)
unique(datatb$alarm)
unique(datatb$transformer)

datanum <- data.frame(temperature=as.numeric(as.factor(datamt[,9])),oil_indicator=as.numeric(as.factor(datamt[,7])),
                      contact=as.numeric(as.factor(datamt[,4])),float=as.numeric(as.factor(datamt[,14])),other_cause = as.numeric(as.factor(datamt[,1])),
                      low_level=as.numeric(as.factor(datamt[,5])),level_incorrect=as.numeric(as.factor(datamt[,6])),
                      leak=as.numeric(as.factor(datamt[,12])),
                      breather=as.numeric(as.factor(datamt[,3])),control=as.numeric(as.factor(datamt[,2])),
                      buchholz=as.numeric(as.factor(datamt[,13])),alarm=as.numeric(as.factor(datamt[,10])),
                      transformer=as.numeric(as.factor(datamt[,8])))
# temperature: 1:high humidity; 2: low temperatures; 3:na
# oilindicator: 1: gauge defect; 2: glass dirty 3: na 4: sight glass
# contact: 1:contact fault 2: crack 3: damaged component 4: ferrule 5: gasket 6: loose fixing 7: na
#          7: seal deterioration 8: seal integrity defect 9: terminal cover
#float: 1: float chamber mechanism defect 2: magnet or reed switch 3: mercury switch defective 4:na
# 
net <- hc(datanum)
plot(net)
uniqsort<-sortdata1[!duplicated(sortdata1[,3:4]),] 
freq <- rep(0,dim(uniqsort)[1])
for(i in 1:dim(uniqsort)[1]){
  for(j in 1:dim(sortdata1)[1]){
    if(all(sortdata1[j,3:4]==uniqsort[i,3:4])){
      freq[i] <- freq[i] + 1
    }
  }
}
freq.sort <- uniqsort[which(freq>10),] ## 158
freq.var <- sortvar1[!duplicated(sortdata1[,3:4]),][which(freq>20),3:4]
freq.var1 <- freq.var[-which(freq.var$effect=="Xmaint"),]
wl = matrix(c("other_cause","transformer",
              "control","transformer",
              "contact","transformer",
              "low_level","transformer",
              "breather","transformer",
              "low_level","level_incorrect"),ncol=2,byrow=TRUE,dimnames=list(NULL,c("from","to")))

bl = matrix(c("buchholz","low_level",
              "control","low_level"),ncol=2,byrow=TRUE,dimnames=list(NULL,c("from","to")))
net <- hc(datanum,whitelist = wl,blacklist = bl)
plot(net)
