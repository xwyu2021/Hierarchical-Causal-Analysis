####################################
############# model 2 ##############
#####################################

## ••• on CEG topology
num_pos <- 18
num_pos_edge <- rep(2,18);num_pos_edge[3] <- 3
path.pos <- vector("list",42)
path.edge <- vector("list",21)

temp <- read.csv("~/Desktop/thesis pack/data/THESISpathposMOD2.csv",header = FALSE)
colnames(temp) <- {}
temp <- as.matrix(temp)
for(i in 1:42){
  path.pos[[i]] <- temp[i,!is.na(temp[i,])] + 1
}

temp <- read.csv("~/Desktop/thesis pack/data/THESISpathedgesMOD2.csv",header = FALSE)
colnames(temp) <- {}
temp <- as.matrix(temp)
for(i in 1:42){
  path.edge[[i]] <- temp[i,!is.na(temp[i,])]
}
rm(temp)

num_path <- 42
mcause <- matrix(c(rep(state.parents[[1]][2,],12),rep(state.parents[[1]][3,],18),rep(state.parents[[1]][1,],12)),byrow=T,ncol=2)
symps <- rep(c(1,2,2,1,3,2,1),each=6)
msb <- rep(c(1,0,0,1,1,0,1,0,1,0,0,1,0,1),c(2,4,4,2,2,4,2,4,2,4,4,2,4,2))
sb <- msb
sb[which(msb == 1)] = 2
sb[which(msb == 0)] = rep(c(1,0),each = 2)
fs <- rep(c(1,0),21)
path.y <- cbind(mcause,symps,msb,sb,fs)
rm(mcause);rm(symps);rm(msb);rm(sb);rm(fs)

path.miss.f <- list(which((path.y[,1] == 1) & (path.y[,6] == 1)),which((path.y[,4]==1)& (path.y[,6] == 1)))
path.nm.f <-list(which((path.y[,1] == 0) & (path.y[,6] == 1)),which((path.y[,4]==0)& (path.y[,6] == 1)))
path.miss.ok <- list(which((path.y[,1] == 1) & (path.y[,6] == 0)),which((path.y[,4]==1)& (path.y[,6] == 0)))
path.nm.ok <-list(which((path.y[,1] == 0) & (path.y[,6] == 0)),which((path.y[,4]==0)& (path.y[,6] == 0)))
path.pos <- vector("list",42)
path.edge <- vector("list",21)


## ••• Stages
stages <- vector("list",12)
stages[[1]] <- 0; stages[[2]] <- 1;stages[[3]] <- 2;stages[[4]] <- 3; stages[[5]] <- 4
stages[[6]] <- c(5,6); stages[[7]] <- c(7,8,9,10); stages[[8]] <- 11;
stages[[9]] <- c(12,13,14);stages[[10]] <- 15; stages[[11]] <- 16; stages[[12]] <- 17;
num.stages <- 12

## ••• Prior Elicitation

ess <- 3
alpha <- vector("list",18)
alpha[[1]] <- rep(ess/2,2)
alpha[[2]] <- alpha[[5]] <- rep(ess/4,2)
alpha[[3]] <- rep(ess/8,2)
alpha[[4]] <- rep(ess/12,3)
alpha[[6]] <- rep(ess/16,2)
alpha[[7]] <- rep(ess/8,2)
alpha[[8]] <- rep(ess/12,3)
alpha[[9]] <- rep(ess/24,2)
alpha[[10]] <- rep(ess/16,2)
alpha[[11]] <- rep(ess/8,2)
alpha[[12]] <- rep(ess/48,2)
alpha[[13]] <- rep(ess/16,2)
alpha[[14]] <- rep(ess/8,2)
alpha[[15]] <- rep(ess/24,2)
alpha[[16]] <- alpha[[17]] <- rep(ess*3/16,2)
alpha[[18]] <- rep(ess/8,2)

alpha.stage <- vector("list",12)
alpha.stage[1:5] <- alpha[1:5]
alpha.stage[[6]] <- alpha[[6]]+alpha[[7]]
alpha.stage[[7]] <- alpha[[8]] + alpha[[9]] + alpha[[10]] + alpha[[11]] 
alpha.stage[[8]] <- alpha[[12]]
alpha.stage[[9]] <- alpha[[13]] + alpha[[14]] + alpha[[15]]
alpha.stage[[10]] <- alpha[[16]]
alpha.stage[[11]] <- alpha[[17]]
alpha.stage[[12]] <- alpha[[18]]




