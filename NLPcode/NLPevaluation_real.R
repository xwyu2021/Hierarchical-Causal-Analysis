library(prodlim)

###################################################
###### core event extraction evaluation ###########
###################################################


## 1. load hypothesized causal pairs
mypair <-read.csv("~/Documents/TEXTDATA/annotateCE.csv")
mypair <- mypair[,c(1,2)]
mypair <- mypair[!duplicated(mypair),] ## 296 pairs
## 2. load extracted causal pairs
extracted <- read.csv("~/Documents/TEXTDATA/sortedtext2.csv")
extracted <- extracted[,c(3,4)] 
origin <- extracted
extracted<-extracted[!duplicated(extracted),] ### 496 pairs
freq <- rep(0,dim(extracted)[1])
for(i in 1:dim(extracted)[1]){
  for(j in 1:dim(origin)[1]){
    if(all(origin[j,]==extracted[i,])){
      freq[i] <- freq[i] + 1
    }
  }
}
freq.extract <- extracted[which(freq>1),] ## 158

## 3. precision, recall and fscore

intersection <- mypair[!is.na(row.match(mypair,extracted)),] ## 195
precision <- dim(intersection)[1]/dim(extracted)[1] ## 0.3931452
recall <- dim(intersection)[1]/dim(mypair)[1] ## 0.6587838
fscore <- 2*precision*recall/(precision + recall) ## 0.4924242

## 4. include the indirect causal relation A->B->C=>(A,C) only one intermediate cause
indirect.c = c()
indirect.e = c()
for(i in 1:dim(mypair)[1]){
  cause <- mypair$cause[i]
  effect <- mypair$effect[i]
  inter <- which(mypair$cause[(i+1):dim(mypair)[1]]==effect)
  if(length(inter)>0){
    index <- c(i+1:dim(mypair)[1])[inter]
    indirect.e <- c(indirect.e,mypair$effect[index])
    indirect.c <- c(indirect.c,rep(cause,length(index)))
  }
}
indirect.pair <- data.frame(cause=indirect.c,effect=indirect.e)
indirect.pair <- indirect.pair[!duplicated(indirect.pair),]
all.pair <- rbind(mypair,indirect.pair)
all.pair <- all.pair[!duplicated(all.pair),]
new.intersection <- all.pair[!is.na(row.match(all.pair,extracted)),] ## 218
precision <- dim(new.intersection)[1]/dim(extracted)[1] ## 0.4395161
recall <- dim(new.intersection)[1]/dim(all.pair)[1] ## 0.4368737
fscore <- 2*precision*recall/(precision + recall) ##  0.438191





