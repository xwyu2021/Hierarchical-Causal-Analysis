##### REMEDIAL INTERVENTION LEARNING EXPERIMENT ###############
###############################################################

## ground truth CAUSAL tree
npath=20
tpos <- read.csv("Documents/DATA/treeposREM.csv",header=FALSE)
tpos <- as.matrix(tpos)
ted <-read.csv("Documents/DATA/treeedgeREM.csv",header=FALSE)
ted <- as.matrix(ted)
tp <- read.csv("Documents/DATA/treeprobREM.csv",header=FALSE)
tp <- as.matrix(tp)
pi.t <- list()
for(i in 1:npath){
  pi.t[[i]] <- tp[i,is.na(tp[i,])==FALSE]
}
## ground truth LEARNING tree
tlpos <- read.csv("Documents/DATA/treeposREML.csv",header=FALSE)
tlpos <- as.matrix(tlpos)
tled <-read.csv("Documents/DATA/treeedgeREML.csv",header=FALSE)
tled <- as.matrix(tled)
tlp <- read.csv("Documents/DATA/treeprobREML.csv",header=FALSE)
tlp <- as.matrix(tlp)
## path transfer from t to tl
t.tl <- c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10,20)
## simulate from ground truth causal tree
   # samplepath <- sample(1:npath,5000,sapply(pi.t,prod),replace = TRUE)
   # write.csv(samplepath,"Documents/DATA/pathREM.csv")
## read synthetic causal data and convert it for learning
samplepath <- read.csv("Documents/DATA/pathREM.csv")
samplepath <- samplepath[,-1]
convpath <- t.tl[samplepath]
Df <- convpath[which(convpath < 11)] ## could be read and summarised from failure reports
Dn <- convpath[which(convpath > 10)] ## could be read somewhere else known it's not a fail

## reconstruct the tree prob for intervened system
 # library(DirichletReg)
tIp <- tp
tIp[1:16,2] <- rep(c(0.2,16/30,4/30,4/30),each=4)
pi.tI <- list()
for(i in 1:npath){
  pi.tI[[i]] <- tIp[i,is.na(tIp[i,])==FALSE]
}
   # intpath <- sample(1:npath,5000,sapply(pi.tI,prod),replace = TRUE)
   # write.csv(intpath,"Documents/DATA/intpathREM.csv")
intpath <- read.csv("Documents/DATA/intpathREM.csv")
intpath <- intpath[,-1]
convpathI <- t.tl[intpath]
DIf <- convpathI[which(convpathI < 11)] ## could be read and summarised from failure reports
DIn <- convpathI[which(convpathI > 10)] ## could be read somewhere else known it's not a fail



## nodes to merge
to.merge <- list()
to.merge[[1]] <- c(1,2) + 1
to.merge[[2]] <- c(3,5) + 1
to.merge[[3]] <- c(4,6) + 1
to.merge[[4]] <- c(7,8,11,12) + 1
to.merge[[5]] <- c(9,13) + 1
to.merge[[6]] <- c(10,14) + 1

## simulate time
library(invgamma)
 #idletime <- rweibull(5000,shape=2,scale=5) ## mean = 5 months
 #intvtime <- idletime
 #for(i in which(convpathI > 10)){
   #h <- idletime[i]
   #t <- 0
   #while(t<=h){
    # t <- rweibull(1,shape=2,scale=5)
   #}
   #intvtime[i] <- t-h
 #}
 # timeset <- data.frame(idle =idletime,intv = intvtime)
 # write.csv(timeset,"Documents/DATA/timeREM.csv")
timeset <- read.csv("Documents/DATA/timeREM.csv")
timeset<- timeset[,-1]
idletime <- timeset$idle
idleftime <- idletime[which(convpath < 11)]
idlentime <- idletime[which(convpath > 10)]
intvtime <- timeset$intv
intvftime <- intvtime[which(convpath < 11)]
## prior specification
beta <- rep(2,npath)
zeta <- rep(2,npath)
mu <- rep(5,npath)
## posteriors:
pathct <- rep(0,npath)
post_mu <-rep(0,npath)
#for(i in 1:npath){
#  uu = which(convpath==i)
#  pathct[i]=length(uu)
#  for(j in uu){
#  }
#}
post_zeta <- zeta + pathct


#mean(rinvgamma(5000,shape=2,rate=5))
ess=1
a <- ess * c(1/2,1/4,1/4,1/16,1/8,1/16,1/8,rep(1/32,8))
alphaprior <- list()
edgenum <- c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2)
for(i in 1:15){
  alphaprior[[i]] = rep(a[i],edgenum[i])
}
alphapriorI <- alphaprior
alphapriorI[[4]]=alphapriorI[[6]] <- c(3,8,2,2)/10

## counts and posterior parameters
source("Documents/CODE/MAPscore.R")
ct.Df <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(Df),paths=Df,vertex=tlpos,edge=tled)
ct.D <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(convpath),paths=convpath,vertex=tlpos,edge=tled)
ct.DIf<-counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(DIf),paths=DIf,vertex=tlpos,edge=tled)
ct.DI <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(convpathI),paths=convpathI,vertex=tlpos,edge=tled)

alphapost <- alphaprior
alphapostf <- alphaprior
alphapost_id <- alphaprior
alphapostf_id <- alphaprior
alphapostI <- alphapriorI
alphapostIf <- alphapriorI
for(i in 1:15){
  alphapostf[[i]] <- alphaprior[[i]] + ct.Df[[i]]
  alphapost[[i]] <- alphaprior[[i]] + ct.D[[i]]
  alphapostIf[[i]] <- alphapriorI[[i]] + ct.DIf[[i]]
  alphapostI[[i]] <- alphapriorI[[i]] + ct.DI[[i]]
  alphapostf_id[[i]] <- alphaprior[[i]] + ct.DIf[[i]]
  alphapost_id[[i]] <- alphaprior[[i]] + ct.DI[[i]]
}

## model search

## (1) idle data learning
localscoresf = to.merge
localscores = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresf[[i]][j] = llk_local(stage,alphaprior,alphapostf)
    localscores[[i]][j] = llk_local(stage,alphaprior,alphapost)
  }
}

modelf= ahc_search(alphaprior,alphapostf,to.merge,localscoresf) 
model= ahc_search(alphaprior,alphapost,to.merge,localscores) 


## (2) intvt data idle prior learning
localscoresf_id = to.merge
localscores_id = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresf_id[[i]][j] = llk_local(stage,alphaprior,alphapostf_id)
    localscores_id[[i]][j] = llk_local(stage,alphaprior,alphapost_id)
  }
}

modelf_id= ahc_search(alphaprior,alphapostf_id,to.merge,localscoresf_id) 
model_id= ahc_search(alphaprior,alphapost_id,to.merge,localscores_id) 

## (3) intvt data intvt prior learning

localscoresIf = to.merge
localscoresI = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresIf[[i]][j] = llk_local(stage,alphapriorI,alphapostIf)
    localscoresI[[i]][j] = llk_local(stage,alphapriorI,alphapostI)
  }
}

modelIf= ahc_search(alphapriorI,alphapostIf,to.merge,localscoresIf) 
modelI= ahc_search(alphapriorI,alphapostI,to.merge,localscoresI) 

## report scores
# (1) idle system
score = sum(unlist(model$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapost[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapost[[1]]))  # -13156.62
scoref = sum(unlist(modelf$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapostf[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapostf[[1]]))  # -7147.511
# (2) intervened system idle prior
score_id = sum(unlist(model_id$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapost_id[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapost_id[[1]])) # -13201.92
scoref_id = sum(unlist(modelf_id$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapostf_id[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapostf_id[[1]])) # -7119.447
# (3) intervened system intervened prior
scoreI = sum(unlist(modelI$scores)) + lgamma(sum(alphapriorI[[1]]))-lgamma(sum(alphapostI[[1]])) - 
  sum(lgamma(alphapriorI[[1]]) - lgamma(alphapostI[[1]])) # -13198.56
scoreIf = sum(unlist(modelIf$scores)) + lgamma(sum(alphapriorI[[1]]))-lgamma(sum(alphapostIf[[1]])) - 
  sum(lgamma(alphapriorI[[1]]) - lgamma(alphapostIf[[1]])) # -7116.403

## report situational errors (convert back)

est = list();estf=est;est_id=est;estf_id=est;estI=est;estIf=est;
est[[1]] = alphapost[[1]]/sum(alphapost[[1]])
estf[[1]] = alphapostf[[1]]/sum(alphapostf[[1]])
est_id[[1]] = alphapost_id[[1]]/sum(alphapost_id[[1]])
estf_id[[1]] = alphapostf_id[[1]]/sum(alphapostf_id[[1]])
estI[[1]] = alphapostI[[1]]/sum(alphapostI[[1]])
estIf[[1]] = alphapostIf[[1]]/sum(alphapostIf[[1]])
for(i in 1:length(to.merge)){
  est[[i+1]] = list();estf[[i+1]] = list();
  est_id[[i+1]] = list();estf_id[[i+1]] = list();
  estI[[i+1]] = list();estIf[[i+1]] = list()
  for(j in 1:length(model$post[[i]])){
    est[[i+1]][[j]] = model$post[[i]][[j]]/sum(model$post[[i]][[j]])
  }
  for(j in 1:length(modelf$post[[i]])){
    estf[[i+1]][[j]] = modelf$post[[i]][[j]]/sum(modelf$post[[i]][[j]])
  }
  for(j in 1:length(model_id$post[[i]])){
    est_id[[i+1]][[j]] = model_id$post[[i]][[j]]/sum(model_id$post[[i]][[j]])
  }
  for(j in 1:length(modelf_id$post[[i]])){
    estf_id[[i+1]][[j]] = modelf_id$post[[i]][[j]]/sum(modelf_id$post[[i]][[j]])
  }
  for(j in 1:length(modelI$post[[i]])){
    estI[[i+1]][[j]] = modelI$post[[i]][[j]]/sum(modelI$post[[i]][[j]])
  }
  for(j in 1:length(modelIf$post[[i]])){
    estIf[[i+1]][[j]] = modelIf$post[[i]][[j]]/sum(modelIf$post[[i]][[j]])
  }
}

# reverse for our tree (bespoke)
estcausal <-function(est){
  estcausal <- vector("list",17)
  ## class:
  if(length(est[[2]]) == 1){
    estcausal[[1]]=est[[2]][[1]]
  }else{
    estcausal[[1]] = c(sum(sapply(est[[2]],'[[',1)*est[[1]]),sum(sapply(est[[2]],'[[',2)*est[[1]]))
  }
  ## endo cause:
  estcausal[[2]] = c(sum(sapply(est[[3]],'[[',1)*sapply(est[[2]],'[[',1)*est[[1]]),
               sum(sapply(est[[3]],'[[',2)*sapply(est[[2]],'[[',1)*est[[1]]),
               sum(sapply(est[[3]],'[[',3)*sapply(est[[2]],'[[',1)*est[[1]]),
               sum(sapply(est[[3]],'[[',4)*sapply(est[[2]],'[[',1)*est[[1]]))/estcausal[[1]][1]
  ## exo cause:
  estcausal[[3]] = c(sum(sapply(est[[4]],'[[',1)*sapply(est[[2]],'[[',2)*est[[1]]),
                     sum(sapply(est[[4]],'[[',2)*sapply(est[[2]],'[[',2)*est[[1]]))/estcausal[[1]][2]
  ## leak|gas,endo:
  con = estcausal[[1]][1] * estcausal[[2]][1]
  estcausal[[4]] = c(sum(sapply(est[[5]],'[[',1)*sapply(est[[3]],'[[',1)*sapply(est[[2]],'[[',1)*est[[1]]),
                                      sum(sapply(est[[5]],'[[',2)*sapply(est[[3]],'[[',1)*sapply(est[[2]],'[[',1)*est[[1]]))/con
  ## leak|seal,endo:
  con = estcausal[[1]][1] * estcausal[[2]][2]
  estcausal[[5]] =c(sum(sapply(est[[5]],'[[',1)*sapply(est[[3]],'[[',2)*sapply(est[[2]],'[[',1)*est[[1]]),
                    sum(sapply(est[[5]],'[[',2)*sapply(est[[3]],'[[',2)*sapply(est[[2]],'[[',1)*est[[1]]))/con
  ## loss|insu,endo:
  con = estcausal[[1]][1] * estcausal[[2]][3]
  estcausal[[6]] =c(sum(sapply(est[[6]],'[[',1)*sapply(est[[3]],'[[',3)*sapply(est[[2]],'[[',1)*est[[1]]),
                    sum(sapply(est[[6]],'[[',2)*sapply(est[[3]],'[[',3)*sapply(est[[2]],'[[',1)*est[[1]]))/con
  ## ther|other,endo:
  con = estcausal[[1]][1] * estcausal[[2]][4]
  estcausal[[7]] =c(sum(sapply(est[[7]],'[[',1)*sapply(est[[3]],'[[',4)*sapply(est[[2]],'[[',1)*est[[1]]),
                    sum(sapply(est[[7]],'[[',2)*sapply(est[[3]],'[[',4)*sapply(est[[2]],'[[',1)*est[[1]]))/con
  ## fail|leak,gas,endo:
  con=estcausal[[1]][1] * estcausal[[2]][1]*estcausal[[4]][1]
  if(length(sapply(est[[2]],'[[',1)) >1){
    l = sapply(est[[2]],'[[',1)[2]
  }else{
    l= sapply(est[[2]],'[[',1)[1]
  }
  if(length(sapply(est[[3]],'[[',1)) >1){
    k = sapply(est[[3]],'[[',1)[2]
  }else{
    k= sapply(est[[3]],'[[',1)[1]
  }
  
  estcausal[[8]] =c(sapply(est[[5]],'[[',1)[1]*sapply(est[[3]],'[[',1)[1]*sapply(est[[2]],'[[',1)[1]*est[[1]][1], 
                    sapply(est[[5]],'[[',1)[1]*k*l*est[[1]][2])/con
  
  ## fail|noleak,gas,endo:
  con=estcausal[[1]][1] * estcausal[[2]][1]*estcausal[[4]][2]
  estcausal[[9]] = c(sapply(est[[5]],'[[',2)[1]*sapply(est[[3]],'[[',1)[1]*sapply(est[[2]],'[[',1)[1]*est[[1]][1], 
                     sapply(est[[5]],'[[',2)[1]*k*l*est[[1]][2])/con
  ## fail|leak,seal,endo:
  if(length(sapply(est[[3]],'[[',2)) >1){
    k = sapply(est[[3]],'[[',2)[2]
  }else{
    k= sapply(est[[3]],'[[',2)[1]
  }
  con=estcausal[[1]][1] * estcausal[[2]][2]*estcausal[[5]][1]
  estcausal[[10]] = c(sapply(est[[5]],'[[',1)[1]*sapply(est[[3]],'[[',2)[1]*sapply(est[[2]],'[[',1)[1]*est[[1]][1], 
                      sapply(est[[5]],'[[',1)[1]*k*l*est[[1]][2])/con
  ## fail|noleak,seal,endo:
  con=estcausal[[1]][1] * estcausal[[2]][2]*estcausal[[5]][2]
  estcausal[[11]] = c(sapply(est[[5]],'[[',2)[1]*sapply(est[[3]],'[[',2)[1]*sapply(est[[2]],'[[',1)[1]*est[[1]][1], 
                      sapply(est[[5]],'[[',2)[1]*k*l*est[[1]][2])/con
  if(length(sapply(est[[5]],'[[',1))>1){
    estcausal[[8]][2]=estcausal[[8]][2]*sapply(est[[5]],'[[',1)[2]/sapply(est[[5]],'[[',1)[1]
    estcausal[[9]][2]=estcausal[[9]][2]*sapply(est[[5]],'[[',2)[2]/sapply(est[[5]],'[[',2)[1]
    estcausal[[10]][2]=estcausal[[10]][2]*sapply(est[[5]],'[[',1)[2]/sapply(est[[5]],'[[',1)[1]
    estcausal[[11]][2]=estcausal[[11]][2]*sapply(est[[5]],'[[',2)[2]/sapply(est[[5]],'[[',2)[1]
  }
  
  ## fail|loss,insu,endo:
  if(length(sapply(est[[3]],'[[',1)) >1){
    k = sapply(est[[3]],'[[',3)[2]
  }else{
    k= sapply(est[[3]],'[[',3)[1]
  }
  con=estcausal[[1]][1] * estcausal[[2]][3]*estcausal[[6]][1]
  estcausal[[12]] = c(sapply(est[[6]],'[[',1)*sapply(est[[3]],'[[',3)[1]*sapply(est[[2]],'[[',1)[1]*est[[1]][1], 
                      sapply(est[[6]],'[[',1)*k*l*est[[1]][2])/con
  ## fail|mix,insu,endo:
 
  con=estcausal[[1]][1] * estcausal[[2]][3]*estcausal[[6]][2]
  estcausal[[13]] = c(sapply(est[[6]],'[[',2)[1]*sapply(est[[3]],'[[',3)[1]*sapply(est[[2]],'[[',1)[1]*est[[1]][1], 
                      sapply(est[[6]],'[[',2)[1]*k*l*est[[1]][2])/con
  ## fail|ther,other,endo:
  if(length(sapply(est[[3]],'[[',1)) >1){
    k = sapply(est[[3]],'[[',4)[2]
  }else{
    k= sapply(est[[3]],'[[',4)[1]
  }
  con=estcausal[[1]][1] * estcausal[[2]][4]*estcausal[[7]][1]
  estcausal[[14]] = c(sapply(est[[7]],'[[',1)[1]*sapply(est[[3]],'[[',4)[1]*sapply(est[[2]],'[[',1)[1]*est[[1]][1], 
                      sapply(est[[7]],'[[',1)[1]*k*l*est[[1]][2])/con
  ## fail|elec,other,endo:
  con=estcausal[[1]][1] * estcausal[[2]][4]*estcausal[[7]][2]
  estcausal[[15]] = c(sapply(est[[7]],'[[',2)[1]*sapply(est[[3]],'[[',4)[1]*sapply(est[[2]],'[[',1)[1]*est[[1]][1], 
                      sapply(est[[7]],'[[',2)[1]*k*l*est[[1]][2])/con
  
  ## fail|exo,corr:
  if(length(sapply(est[[2]],'[[',2)) >1){
    ll = sapply(est[[2]],'[[',2)[2]
  }else{
    ll= sapply(est[[2]],'[[',2)[1]
  }
  con=estcausal[[1]][2] * estcausal[[3]][1]
  estcausal[[16]] = c(sapply(est[[4]],'[[',1)[1]*sapply(est[[2]],'[[',2)[1]*est[[1]][1], 
                      sapply(est[[4]],'[[',1)[1]*ll*est[[1]][2])/con
  ## fail|exo,other:
  con=estcausal[[1]][2] * estcausal[[3]][2]
  estcausal[[17]] = c(sapply(est[[4]],'[[',2)[1]*sapply(est[[2]],'[[',2)[1]*est[[1]][1], 
                      sapply(est[[4]],'[[',2)[1]*ll*est[[1]][2])/con
  
   return(estcausal)
}

causal = estcausal(est)
causalf=estcausal(estf)
causal_id = estcausal(est_id)
causalf_id = estcausal(estf_id)
causalI = estcausal(estI)
causalIf = estcausal(estIf)

## error on causal tree and learning tree
o <- read.csv("Documents/DATA/intprobpos.csv",header=F)
sitp <-list()
for(i in 1:17){
  sitp[[i]]=o[i,is.na(o[i,])==F]
}
o <- read.csv("Documents/DATA/intprobposL.csv",header=F)
sitpL <- list()
for(i in 1:15){
  sitpL[[i]]=o[i,is.na(o[i,])==F]
}
rm(o)
er_id = 0
erf_id = 0
erI = 0
erIf = 0

for(i in 1:17){
 er_id = er_id +  sqrt(sum((sitp[[i]]-causal_id[[i]])^2))
 erf_id = erf_id +  sqrt(sum((sitp[[i]]-causalf_id[[i]])^2))
 erI = erI +  sqrt(sum((sitp[[i]]-causalI[[i]])^2))
 erIf = erIf +  sqrt(sum((sitp[[i]]-causalIf[[i]])^2))
}
er_id;erf_id;erI;erIf
# 0.5142932;4.725707;0.2453148;4.725722

Ler_id=0
Lerf_id=0
LerI = 0
LerIf = 0
for(i in 2:7){
  for(j in 1:length(est_id[[i]])){
    nodes=model_id$stages[[i-1]][[j]]
    for(k in 1:length(nodes)){
      ind = nodes[k]
      Ler_id = Ler_id + sqrt(sum((sitpL[[ind]]-est_id[[i]][[j]])^2))
    }
  }
  for(j in 1:length(estf_id[[i]])){
    nodes=modelf_id$stages[[i-1]][[j]]
    for(k in 1:length(nodes)){
      ind = nodes[k]
      Lerf_id = Lerf_id + sqrt(sum((sitpL[[ind]]-estf_id[[i]][[j]])^2))
    }
  }
  for(j in 1:length(estI[[i]])){
    nodes=modelI$stages[[i-1]][[j]]
    for(k in 1:length(nodes)){
      ind = nodes[k]
      LerI = LerI + sqrt(sum((sitpL[[ind]]-estI[[i]][[j]])^2))
    }
  }
  for(j in 1:length(estIf[[i]])){
    nodes=modelIf$stages[[i-1]][[j]]
    for(k in 1:length(nodes)){
      ind = nodes[k]
      LerIf = LerIf + sqrt(sum((sitpL[[ind]]-estIf[[i]][[j]])^2))
    }
  }
}
Ler_id = Ler_id + sqrt(sum((sitpL[[1]]-est_id[[1]])^2)) ## 0.2436946
Lerf_id = Lerf_id + sqrt(sum((sitpL[[1]]-estf_id[[1]])^2)) ##  1.19869
LerI = LerI + sqrt(sum((sitpL[[1]]-estI[[1]])^2)) ## 0.1754162
LerIf = LerIf + sqrt(sum((sitpL[[1]]-estIf[[1]])^2)) ## 1.198708


pstm=modelf_id$stages
pstsd=pstm
lvm = pstm
lvin=lvm
for(i in 1:length(modelf_id$stages)){
  for(j in 1:length(pstm[[i]])){
    for(k in 1:length(pstm[[i]][[j]])){
      v = modelf_id$stages[[i]][[j]][k]
      leave = ct.DIf[[v]]
      if(leave ==0){
        lvm[[i]][[j]][k]=0
      }else{
        lvm[[i]][[j]][k]=c(leave/sum(leave))[1]
      }
      all = modelf_id$post[[i]][[j]]
      out = all-leave
      prop=out/sum(out)
      pstm[[i]][[j]][k]=(all/sum(all))[1]
      pstsd[[i]][[j]][k] = sqrt((prop[1])*(1-(prop[1]))/(sum(out)+1))
      up = pstm[[i]][[j]][k]+2*pstsd[[i]][[j]][k]
      low = pstm[[i]][[j]][k]-2*pstsd[[i]][[j]][k]
      if((lvm[[i]][[j]][k] <= up) & (lvm[[i]][[j]][k] >= low)){
        lvin[[i]][[j]][k]=1
      }else{
        lvin[[i]][[j]][k]=0
      }
    }
  }
}


plot(1:14,c(pstm[[1]][[1]],pstm[[2]][[1]],pstm[[3]][[1]],pstm[[4]][[1]],pstm[[5]][[1]],pstm[[6]][[1]]),pch=19,
     xlab="Situations",ylab="E[X]",xaxt="n",xlim=c(0.5,14.5),
     ylim=c(min(c(pstm[[1]][[1]]-2*pstsd[[1]][[1]],lvm[[1]][[1]],
                  pstm[[2]][[1]]-2*pstsd[[2]][[1]],lvm[[2]][[1]],
                  pstm[[3]][[1]]-2*pstsd[[3]][[1]],lvm[[3]][[1]],
                  pstm[[4]][[1]]-2*pstsd[[4]][[1]],lvm[[4]][[1]],
                  pstm[[5]][[1]]-2*pstsd[[5]][[1]],lvm[[5]][[1]],
                  pstm[[6]][[1]]-2*pstsd[[6]][[1]],lvm[[6]][[1]])),
            max(c(pstm[[1]][[1]]+2*pstsd[[1]][[1]],lvm[[1]][[1]],
                  pstm[[2]][[1]]+2*pstsd[[2]][[1]],lvm[[2]][[1]],
                  pstm[[3]][[1]]+2*pstsd[[3]][[1]],lvm[[3]][[1]],
                  pstm[[4]][[1]]+2*pstsd[[4]][[1]],lvm[[4]][[1]],
                  pstm[[5]][[1]]+2*pstsd[[5]][[1]],lvm[[5]][[1]],
                  pstm[[6]][[1]]+2*pstsd[[6]][[1]],lvm[[6]][[1]]))))
lines(rbind(1:14,1:14,NA),rbind(c(pstm[[1]][[1]]-2*pstsd[[1]][[1]],pstm[[2]][[1]]-2*pstsd[[2]][[1]],
                                  pstm[[3]][[1]]-2*pstsd[[3]][[1]],pstm[[4]][[1]]-2*pstsd[[4]][[1]],
                                  pstm[[5]][[1]]-2*pstsd[[5]][[1]],pstm[[6]][[1]]-2*pstsd[[6]][[1]]),
                              c(pstm[[1]][[1]]+2*pstsd[[1]][[1]],pstm[[2]][[1]]+2*pstsd[[2]][[1]],
                                pstm[[3]][[1]]+2*pstsd[[3]][[1]],pstm[[4]][[1]]+2*pstsd[[4]][[1]],
                                pstm[[5]][[1]]+2*pstsd[[5]][[1]],pstm[[6]][[1]]+2*pstsd[[6]][[1]]),NA))
points(1:14,c(unlist(lvm)),pch=17,col="blue")
axis(side=1,at=1:14,labels=unlist(modelf_id$stages)-1)
abline(v = c(2.5,4.5,6.5,10.5,12.5), col="red", lwd=1, lty=2)



pstm=modelIf$stages
pstsd=pstm
lvm = pstm
lvin=lvm
for(i in 1:length(modelIf$stages)){
  for(j in 1:length(pstm[[i]])){
    for(k in 1:length(pstm[[i]][[j]])){
      v = modelIf$stages[[i]][[j]][k]
      leave = ct.DIf[[v]]
      if(leave ==0){
        lvm[[i]][[j]][k]=0
      }else{
        lvm[[i]][[j]][k]=c(leave/sum(leave))[1]
      }
      all = modelIf$post[[i]][[j]]
      out = all-leave
      prop=out/sum(out)
      pstm[[i]][[j]][k]=(all/sum(all))[1]
      pstsd[[i]][[j]][k] = sqrt((prop[1])*(1-(prop[1]))/(sum(out)+1))
      up = pstm[[i]][[j]][k]+2*pstsd[[i]][[j]][k]
      low = pstm[[i]][[j]][k]-2*pstsd[[i]][[j]][k]
      if((lvm[[i]][[j]][k] <= up) & (lvm[[i]][[j]][k] >= low)){
        lvin[[i]][[j]][k]=1
      }else{
        lvin[[i]][[j]][k]=0
      }
    }
  }
}


plot(1:14,c(pstm[[1]][[1]],pstm[[2]][[1]],pstm[[3]][[1]],pstm[[4]][[1]],pstm[[5]][[1]],pstm[[6]][[1]]),pch=19,
     xlab="Situations",ylab="E[X]",xaxt="n",xlim=c(0.5,14.5),
     ylim=c(min(c(pstm[[1]][[1]]-2*pstsd[[1]][[1]],lvm[[1]][[1]],
                  pstm[[2]][[1]]-2*pstsd[[2]][[1]],lvm[[2]][[1]],
                  pstm[[3]][[1]]-2*pstsd[[3]][[1]],lvm[[3]][[1]],
                  pstm[[4]][[1]]-2*pstsd[[4]][[1]],lvm[[4]][[1]],
                  pstm[[5]][[1]]-2*pstsd[[5]][[1]],lvm[[5]][[1]],
                  pstm[[6]][[1]]-2*pstsd[[6]][[1]],lvm[[6]][[1]])),
            max(c(pstm[[1]][[1]]+2*pstsd[[1]][[1]],lvm[[1]][[1]],
                  pstm[[2]][[1]]+2*pstsd[[2]][[1]],lvm[[2]][[1]],
                  pstm[[3]][[1]]+2*pstsd[[3]][[1]],lvm[[3]][[1]],
                  pstm[[4]][[1]]+2*pstsd[[4]][[1]],lvm[[4]][[1]],
                  pstm[[5]][[1]]+2*pstsd[[5]][[1]],lvm[[5]][[1]],
                  pstm[[6]][[1]]+2*pstsd[[6]][[1]],lvm[[6]][[1]]))))
lines(rbind(1:14,1:14,NA),rbind(c(pstm[[1]][[1]]-2*pstsd[[1]][[1]],pstm[[2]][[1]]-2*pstsd[[2]][[1]],
                                  pstm[[3]][[1]]-2*pstsd[[3]][[1]],pstm[[4]][[1]]-2*pstsd[[4]][[1]],
                                  pstm[[5]][[1]]-2*pstsd[[5]][[1]],pstm[[6]][[1]]-2*pstsd[[6]][[1]]),
                                c(pstm[[1]][[1]]+2*pstsd[[1]][[1]],pstm[[2]][[1]]+2*pstsd[[2]][[1]],
                                  pstm[[3]][[1]]+2*pstsd[[3]][[1]],pstm[[4]][[1]]+2*pstsd[[4]][[1]],
                                  pstm[[5]][[1]]+2*pstsd[[5]][[1]],pstm[[6]][[1]]+2*pstsd[[6]][[1]]),NA))
points(1:14,c(unlist(lvm)),pch=17,col="blue")
axis(side=1,at=1:14,labels=unlist(modelIf$stages)-1,cex.axis=0.8)
abline(v = c(2.5,4.5,6.5,10.5,12.5), col="red", lwd=1, lty=2)

### leave one out stage monitor
loo = list()
for(i in 1:6){
  situation = modelf_id$stages[[i]][[1]]
  prior_u = alphaprior[[situation[1]]]
  for(j in 2:length(situation)){
    prior_u = prior_u + alphaprior[[situation[j]]] 
  }
  prior_u_s=sum(prior_u)
  post_u= modelf_id$post[[i]][[1]]
  post_u_s= sum(post_u)
  loo[[i]] = lgamma(prior_u_s)-lgamma(post_u_s)-(sum(lgamma(prior_u))-sum(lgamma(post_u)))
  for(j in 1:length(situation)){
    post_u = modelf_id$post[[i]]-ct.DIf[[situation[j]]]
    post_u_s=sum(post_u)
    ho = lgamma(prior_u_s)-lgamma(post_u_s)-(sum(lgamma(prior_u))-sum(lgamma(post_u)))
    loo[[i]] = c(loo[[i]],ho)
  }
}



############################# full data #######################




pstm=modelI$stages
pstsd=pstm
lvm = pstm
lvin=lvm
for(i in 1:length(modelIf$stages)){
  for(j in 1:length(pstm[[i]])){
    for(k in 1:length(pstm[[i]][[j]])){
      v = modelI$stages[[i]][[j]][k]
      leave = ct.DI[[v]]
      if(leave ==0){
        lvm[[i]][[j]][k]=0
      }else{
        lvm[[i]][[j]][k]=c(leave/sum(leave))[1]
      }
      all = modelI$post[[i]][[j]]
      out = all-leave
      prop=out/sum(out)
      pstm[[i]][[j]][k]=(all/sum(all))[1]
      pstsd[[i]][[j]][k] = sqrt((prop[1])*(1-(prop[1]))/(sum(out)+1))
      up = pstm[[i]][[j]][k]+2*pstsd[[i]][[j]][k]
      low = pstm[[i]][[j]][k]-2*pstsd[[i]][[j]][k]
      if((lvm[[i]][[j]][k] <= up) & (lvm[[i]][[j]][k] >= low)){
        lvin[[i]][[j]][k]=1
      }else{
        lvin[[i]][[j]][k]=0
      }
    }
  }
}


plot(1:14,c(unlist(pstm)),pch=19,
     xlab="Situations",ylab="E[X]",xaxt="n",xlim=c(0.5,14.5),
     ylim=c(min(c(pstm[[1]][[1]]-2*pstsd[[1]][[1]],lvm[[1]][[1]],
                  pstm[[1]][[2]]-2*pstsd[[1]][[2]],lvm[[1]][[2]],
                  pstm[[2]][[1]]-2*pstsd[[2]][[1]],lvm[[2]][[1]],
                  pstm[[2]][[2]]-2*pstsd[[2]][[2]],lvm[[2]][[2]],
                  pstm[[3]][[1]]-2*pstsd[[3]][[1]],lvm[[3]][[1]],
                  pstm[[4]][[1]]-2*pstsd[[4]][[1]],lvm[[4]][[1]],
                  pstm[[4]][[2]]-2*pstsd[[4]][[2]],lvm[[4]][[2]],
                  pstm[[5]][[1]]-2*pstsd[[5]][[1]],lvm[[5]][[1]],
                  pstm[[6]][[1]]-2*pstsd[[6]][[1]],lvm[[6]][[1]])),
            max(c(pstm[[1]][[1]]+2*pstsd[[1]][[1]],lvm[[1]][[1]],
                  pstm[[1]][[2]]+2*pstsd[[1]][[2]],lvm[[1]][[2]],
                  pstm[[2]][[1]]+2*pstsd[[2]][[1]],lvm[[2]][[1]],
                  pstm[[2]][[2]]+2*pstsd[[2]][[2]],lvm[[2]][[2]],
                  pstm[[3]][[1]]+2*pstsd[[3]][[1]],lvm[[3]][[1]],
                  pstm[[4]][[1]]+2*pstsd[[4]][[1]],lvm[[4]][[1]],
                  pstm[[4]][[2]]+2*pstsd[[4]][[2]],lvm[[4]][[2]],
                  pstm[[5]][[1]]+2*pstsd[[5]][[1]],lvm[[5]][[1]],
                  pstm[[6]][[1]]+2*pstsd[[6]][[1]],lvm[[6]][[1]]))))
lines(rbind(1:14,1:14,NA),rbind(c(pstm[[1]][[1]]-2*pstsd[[1]][[1]],pstm[[1]][[2]]-2*pstsd[[1]][[2]],
                                  pstm[[2]][[1]]-2*pstsd[[2]][[1]],pstm[[2]][[2]]-2*pstsd[[2]][[2]],
                                  pstm[[3]][[1]]-2*pstsd[[3]][[1]],pstm[[4]][[1]]-2*pstsd[[4]][[1]],
                                  pstm[[4]][[2]]-2*pstsd[[4]][[2]],
                                  pstm[[5]][[1]]-2*pstsd[[5]][[1]],pstm[[6]][[1]]-2*pstsd[[6]][[1]]),
                                c(pstm[[1]][[1]]+2*pstsd[[1]][[1]],pstm[[1]][[2]]+2*pstsd[[1]][[2]],
                                  pstm[[2]][[1]]+2*pstsd[[2]][[1]],pstm[[2]][[2]]+2*pstsd[[2]][[2]],
                                  pstm[[3]][[1]]+2*pstsd[[3]][[1]],pstm[[4]][[1]]+2*pstsd[[4]][[1]],
                                  pstm[[4]][[2]]+2*pstsd[[4]][[2]],
                                  pstm[[5]][[1]]+2*pstsd[[5]][[1]],pstm[[6]][[1]]+2*pstsd[[6]][[1]]),NA))
points(1:14,c(unlist(lvm)),pch=17,col="blue")
axis(side=1,at=1:14,labels=unlist(modelI$stages)-1,cex.axis=0.8)
abline(v = c(1.5,2.5,3.5,4.5,6.5,8.5,10.5,12.5), col="red", lwd=1, lty=2)

