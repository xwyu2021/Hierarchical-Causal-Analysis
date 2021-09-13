###################################################
######### To search for candidate topologies ######
############# use 100 pooled train data ###########


#                  - ahc algorithm: initialise a starated tree, score = logposterior prob of it
#                                   for every pair of stages, computer lpBF(uij,(ui,uj))
#                                   find the largest lpBF, if >0, merge and update
#                                   inputs: (1) alphaprior: prior; (2) alphapost: posterior;
#                                           (3) to.merge: situations that can be merged;
#                                           (4) localscores
ahc_search<-function(alphaprior,alphapost,to.merge,localscores){
  #each entry of liststages is a list stages (merged) for each variable, start from the non-root
  liststages = to.merge
  listscores = localscores
  listposteriors =vector("list",length(to.merge))
  
  for(i in length(to.merge):1){ ## check from the leaf to root
    nowstages = as.list(liststages[[i]]) ## list of current stages
    nowscore = listscores[[i]] ## vector of current scores for every current stage
    nowprior = alphaprior[liststages[[i]]] ## vector of current prior for every current stage
    nowpost = vector("list",length(nowstages)) ## list of current posterior for every current stage
    for(j in 1:length(nowstages)){
      nowpost[[j]] = alphapost[[nowstages[[j]]]]
    }
    stop = FALSE
    while(stop == FALSE){
      if(length(nowstages)>1){
        ## pair any two stages (index)
        pairs = as.matrix(combn(length(nowstages),2),nrow=2)
        lpbfs = rep(0,dim(pairs)[2])
        for(j in 1:dim(pairs)[2]){
          # pick a pair
          pairindex = pairs[,j]
          notmerge = sum(nowscore[pairindex])
          
          # find merged local score
          prior1 = nowprior[pairindex[1]]
          prior2 = nowprior[pairindex[2]]
          post1 = nowpost[[pairindex[1]]]
          post2 = nowpost[[pairindex[2]]]
          merge = llk_local_mer2(prior1,prior2,post1,post2)
          # log posterior bayes factor
          lpbfs[j] = merge-notmerge
        }
        if(max(lpbfs)>0){
          mergedpair = which.max(lpbfs)
          pairA = pairs[,mergedpair]
          # update stage
          stageA = c(nowstages[[pairA[1]]],nowstages[[pairA[2]]])
          nowstages[[pairA[1]]] = stageA
          nowstages = nowstages[-pairA[2]]
          # update score
          nowscore[pairA[1]] = sum(nowscore[pairA]) + lpbfs[mergedpair]
          nowscore = nowscore[-pairA[2]]
          # update prior
          nowprior[pairA[1]] = sum(nowprior[pairA])
          nowprior = nowprior[-pairA[2]]
          # update posterior
          nowpost[[pairA[1]]] = nowpost[[pairA[1]]] + nowpost[[pairA[2]]]
          nowpost = nowpost[-pairA[2]]
        }else{
          stop = TRUE
        }
      }else{
        stop = TRUE
      }
    }
    ## UPDATE OUTPUT FOR THE VARIABLE STAGE
    liststages[[i]] = nowstages
    listscores[[i]] = nowscore
    listposteriors[[i]] = nowpost
  }
  return(list(stages = liststages,scores=listscores,post = listposteriors))
}


#                  - log-likelihood functions
#                                 (1) llk_local: compute llk score for each stage
#                                 (2) llk_local_mer, llk_local_mer2: merged score
llk_local<-function(stage,alphaprior,alphapost){
  alpha_u_plus = sum(alphapost[[stage]])
  alpha_u = alphaprior[stage] * length(alphapost[[stage]])
  return(lgamma(alpha_u) - lgamma(alpha_u_plus) -
           sum(lgamma(alphaprior[stage]) - lgamma(alphapost[[stage]])))
}
llk_local_mer<-function(stagea,stageb,alphaprior,alphapost){
  newalpha = alphaprior[stagea] + alphaprior[stageb]
  newalphapost = alphapost[[stagea]] + alphapost[[stageb]]
  alpha_u_plus = sum(newalphapost)
  alpha_u = newalpha * length(newalphapost)
  return(lgamma(alpha_u) - lgamma(alpha_u_plus) - 
           sum(lgamma(newalpha) - lgamma(newalphapost)))
}
llk_local_mer2 <- function(prior1,prior2,post1,post2){
  newalpha = prior1+prior2
  newalphapost = post1+post2
  alpha_u_plus = sum(newalphapost)
  alpha_u = newalpha * length(newalphapost)
  return(lgamma(alpha_u) - lgamma(alpha_u_plus) - 
           sum(lgamma(newalpha) - lgamma(newalphapost)))
}
## note: transition + holding time

## event tree
evT.path.num <- 42
evT.path.node <- list()
evTpathtable <- read.csv("~/Documents/thesisTOYPATH.csv",header = FALSE)
colnames(evTpathtable)={}
evTpathtable=as.matrix(evTpathtable)
for(i in 1:evT.path.num){
  evT.path.node[[i]] = evTpathtable[i,is.na(evTpathtable[i,])==FALSE]
}

evTpathedgetable <- read.csv("~/Documents/thesisTOYPATHEDGE.csv",header = FALSE)
colnames(evTpathedgetable)={}
evTpathedgetable <- as.matrix(evTpathedgetable)
evT.path.edge <- list()
for(i in 1:evT.path.num){
  evT.path.edge[[i]] <- evTpathedgetable[i,is.na(evTpathedgetable[i,])==FALSE]
}
evT.node <- 0:39
evT.node.edge <- rep(2,40); evT.node.edge[7]=3
evT.node.child <- vector()
evT.nodeedge.count <- vector("list",40)
for(i in 1:40){
  evT.nodeedge.count[[i]] <- rep(0,evT.node.edge[i]) 
}

to.merge <- vector("list",4)
to.merge[[1]] <- c(1,5) + 1 # symptoms
to.merge[[2]] <- c(3,4,11,12,13,14,15) +1 # missing s/b
to.merge[[3]] <- c(8,10,21,23,25,27,29) + 1## s/b
to.merge[[4]] <- c(7,9,16,17,18,19,20,22,24,26,28,30,31,32,33,34,35,36,37,38,39) +1 # fail

ess <- 3
alphaprior <- c(ess/2,rep(ess/(2*2),2),rep(ess/8,3),ess/12, rep(ess/16,6),rep(ess/24,3),
                rep(ess/32,8),rep(ess/48,6),
                rep(ess/64,4),rep(ess/96,6))


## train data
restoredata <- read.csv("~/Documents/PhD project/data/restoredata.csv")
restoredata <- restoredata[,-1]
restoretrain <- restoredata[which(restoredata$pick=="train"),] 
tm = read.csv("~/Documents/PhD Paper 2/data/timemachine2.csv")
tm = tm[,-1]
ftimetrain <- tm$ftime[which(restoredata$pick=="train")]/365
pathart <- c(2,31,3,3,31, 
             33,13,5,31,35,
             25,8,8,39,31,
             34,37,29,25,35)
# toy <- sample(1:20,100,replace=TRUE)
# toyDF <- restoretrain[toy,]
# toyDF$pathevt <- pathart[toy]
# write.csv(toyDF,"~/Documents/PhD project/data/thesistoydata.csv")
toyDF <- read.csv("~/Documents/PhD project/data/thesistoydata.csv")[,-1]
## update counts
for(i in 1:100){
  path_i <- toyDF$pathevt[i]
  nodes_of_path_i <- evT.path.node[[path_i]] + 1 # because start from root 0
  edges_of_path_i <- evT.path.edge[[path_i]]
  for(j in 1:length(nodes_of_path_i)){
    node <- nodes_of_path_i[j]
    edge <- edges_of_path_i[j]
    evT.nodeedge.count[[node]][edge] <- evT.nodeedge.count[[node]][edge] + 1
  }
}
## find posterior
alphapost = vector("list",40)
for(j in 1:40){
  alphapost[[j]] <- alphaprior[j] + evT.nodeedge.count[[j]]
}
## find llk before merging
localscores = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscores[[i]][j] = llk_local(stage,alphaprior,alphapost)
  }
}
model= ahc_search(alphaprior,alphapost,to.merge,localscores)
he = lapply(model$post[[4]],sum)
for(j in 1:length(model$post[[4]])){
  print(model$post[[4]][[j]]/he[[j]])
}


##### consider time
mean(toyDF$failtime)/365;sd(toyDF$failtime/365)
mean(toyDF$failtime[toyDF$revise.fail==1])/365 # fail
mean(toyDF$failtime[toyDF$revise.fail==0])/365 # no
## set weibull mean = 0.7, va= 0.6
## weibull: beta=0.5 for all path
# let
muprior <- 0.5
xiprior <- 1
betafix <- 0.5
## for a single path:
posthyper <- function(betapath,priorxi, priormu,pathcounts,failuretimepath){
  postxi <- priorxi + sum(pathcounts) 
  postmu <- priormu + sum(pathcounts * (failuretimepath^betapath))
  return(list(postxi=postxi,postmu=postmu))
}
llk_weibull <- function(priormupath,priorxipath,postmupath,postxipath){
  return(priorxipath * log(priormupath) - postxipath * log(postmupath) +
    lgamma(postxipath) - lgamma(priorxipath))
}
path.to.merge <- list()
path.to.merge[[1]] <- c(3,5)
path.to.merge[[2]] <- c(4,6)
path.to.merge[[3]] <- c(9,11)
path.to.merge[[4]] <- c(10,12)
path.to.merge[[5]] <- c(15,17)
path.to.merge[[6]] <- c(16,18)
path.to.merge[[7]] <- c(25,31,37)
path.to.merge[[8]] <- c(26,32,38)

pathcounts <- rep(0,42)
repdocs <- rep(0,20)
for(i in 1:20){
  id = restoretrain$doc.id[i]
  repdocs[i] = length(which(toyDF$doc.id == id))
  j = toyDF$pathevt[which(toyDF$doc.id == id)[1]]
  pathcounts[j] = pathcounts[j] + length(which(toyDF$doc.id==id))
}

llk.to.merge <- path.to.merge
for(i in 1:8){
  for(j in 1:length(path.to.merge[[i]])){
    i=7;j=3
    id <- path.to.merge[[i]][j]
    pool <- which(pathart == id)
    if(length(pool) == 0){ 
      ct <- 0
      ft <- 0
      llk.to.merge[[i]][j] <- 0
    }else{
      ct <- repdocs[pool]
      ft <- ftimetrain[pool]
      posts <- posthyper(betafix,xiprior,muprior,ct,ft)
      llk.to.merge[[i]][j] <- llk_weibull(muprior,xiprior,posts$postmu,posts$postxi)
    }
  }
}
#11,19,
#2  5  9 15
#17
## [[1]], [[7]] nonzero
pathmergepair<- function(llks,cts,fts,betafix,xiprior,muprior){
  nomerge_score <- sum(llks)
  posts <- posthyper(betafix,xiprior,muprior,cts,fts)
  newscore <- llk_weibull(muprior,xiprior,posts$postmu,posts$postxi)
  if(newscore > nomerge_score){
    return(1)
  }else{
    return(0)
  }
}

entry1 = pathmergepair(llk.to.merge[[1]],repdocs[c(3,4,8)],ftimetrain[c(3,4,8)],betafix,xiprior,muprior)
entry7 = pathmergepair(llk.to.merge[[7]][c(1,2)],repdocs[c(11,19,2,5,9,15)],ftimetrain[c(11,19,2,5,9,15)],betafix,xiprior,muprior)
entry7 = pathmergepair(c(sum(llk.to.merge[[7]][c(1,2)]),llk.to.merge[[7]][3]),repdocs[c(11,19,2,5,9,15)],ftimetrain[c(11,19,2,5,9,15)],betafix,xiprior,muprior)
## all yes
