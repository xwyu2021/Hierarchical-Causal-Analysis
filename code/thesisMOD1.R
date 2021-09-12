####################################
############# model 1 ##############
#####################################
library(prodlim)

## ••• Note: here we fetch the staged tree obtained from thesisTOPOLOGY.r
## •••       derive a ceg from it

## ••• Define Core event variables
##       (1) states: state spaces of core events variables
states <- list()
states$system <- c(2,0,1); states$seal <- c(2,1,0);
states$temperature <- c(2,0,1);states$level <- c(1,0);
states$leak <- c(0,1); states$alarm <- c(1,0);states$pipe <- c(2,0,1);
states$sight.glass <- c(2,0,1);states$buchholz <- c(2,0,1);
states$transformer <- c(2,1,0)

## ••• Define briges between core event variables and floret variables
##       (1) num_o: number of core event variables, excluding failure indicator,failure time, action
##       (2) child.ind: inferior indices, repeated ind means has two potential direct superiors
##       (3) dsup.ind: direct superior index for each core event variable
##       (4) pa.gn.ind: parents of core event variables within the GN
##       (5) pa.child.ind: child var of each core event variable 
##       (6) num_ch_va: number of states of core event variables corresponding to child.ind
##       (7) num_paH: number of parents set in the flattening
##       (8) state.parents: state space of parents set in the flattening for each core event variable
##       (9) y.edge: set of edges corresonding to each parent variable
num_o <- 10
child.ind <- c(1,2,3,4,4,5,6,7,7,8,9,10)
dsup.ind <- c(1,1,1,1,2,2,2,2,3,3,3,3)
pa.gn.ind <- vector("list",12)
pa.gn.ind[1:5] <- 0
pa.gn.ind[[6]] <- c(3,4)
pa.gn.ind[[7]] <- 5
pa.gn.ind[[8]] <- c(4,6)
pa.gn.ind[[9]] <- c(4,6)
pa.gn.ind[[10]] <- c(1,4,6)
pa.gn.ind[[11]] <- 8
pa.gn.ind[[12]] <- 8
pa.child.ind = pa.gn.ind[c(1,2,3,4,6,7,8,10,11,12)]
num_ch_va <- rep(0,12)
for(i in 1:12){
  num_ch_va[i] <- length(states[[child.ind[i]]])
}
num_paH <- length(child.ind)
state.parents <- vector("list",num_paH)
state.parents[[1]] <- matrix(c(1,2,0,0,0,1),byrow = T,ncol=2)
state.parents[[2]] <- state.parents[[1]]
state.parents[[3]] <- state.parents[[1]]
state.parents[[4]] <- state.parents[[1]]
state.parents[[5]] <- matrix(c(1,2,3),nrow=3,ncol=1)
state.parents[[6]] <- matrix(c(rep(c(1,2,3),each=6),
                              rep(c(2,0,1),6),rep(rep(c(1,0),each=3),3)),ncol=3)
state.parents[[7]] <- matrix(c(rep(c(1,2,3),each=2),rep(c(1,0),3)),nrow=6)
state.parents[[8]] <- matrix(c(rep(c(1,2,3),each=4),rep(c(1,1,0,0),3),rep(c(1,0),6)),ncol=3)
state.parents[[9]] <- matrix(c(rep(c(1,0,0),each=4),rep(c(2,0,1),each=4),
                              rep(c(1,1,0,0),3),rep(c(1,0),6)),ncol=4)
state.parents[[10]] <- matrix(c(rep(c(1,0,0),each=12),rep(c(2,0,1),each=12),
                               rep(rep(c(0,1,2),each=4),3),rep(c(1,1,0,0),9),rep(c(1,0),18)),ncol = 5)
state.parents[[11]] <- matrix(c(rep(c(1,0,0),each=3),rep(c(2,0,1),each=3),
                               rep(c(0,1,2),3)),ncol=3)
state.parents[[12]] <- state.parents[[11]]
y.edge = vector("list",3)
y.edge[[1]] = c(1,2)
y.edge[[2]] = 3
y.edge[[3]] = c(4,5)

## ••• Define multiple d-sups
##         (1) A: number of core event variables with more than one possible direct superiors
##         (2) rangeA: the location of these variables in child.ind

A <- 2
rangeA <- vector("list",A)
rangeA[[1]] <- c(4,5)
rangeA[[2]] <- c(7,8)


## ••• Define paths on the CEG
##        (1) num_path: number of paths on the CEG
##        (2) path.y: y values associated with each path 
##        (3) path.miss.f: candidate failure paths ind of missing either cause or sb
##        (4) path.nm.f: candidate failure paths ind of not missing either cause or sb
##        (5) path.miss.ok: candidate deteriorating paths ind of missing either cause or sb
##        (6) path.nm.ok: candidate deteriorating paths ind of not missing either cause or sb
##        (7) path.pos: positions indices on paths
##        (8) path.edge: edges indices on paths from positions
##        (9) num_pos: number of positions
##        (10) num_pos_edge: number of edges emanating from each position
##        (11) miss.A: which floret variable corresponding to missingness 
       
num_path <- 42
mcause <- matrix(c(rep(state.parents[[1]][2,],12),rep(state.parents[[1]][3,],18),rep(state.parents[[1]][1,],12)),byrow=T,ncol=2)
symps <- rep(c(1,2,2,3,1,2,1),each=6)
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

temp <- read.csv("~/Desktop/thesis pack/data/THESISpathposMOD1.csv",header = FALSE)
colnames(temp) <- {}
temp <- as.matrix(temp)
for(i in 1:42){
  path.pos[[i]] <- temp[i,!is.na(temp[i,])] + 1
}

temp <- read.csv("~/Desktop/thesis pack/data/THESISpathedgesMOD1.csv",header = FALSE)
colnames(temp) <- {}
temp <- as.matrix(temp)
for(i in 1:42){
  path.edge[[i]] <- temp[i,!is.na(temp[i,])]
}
rm(temp)

num_pos <- 20
num_pos_edge <- rep(2,20);num_pos_edge[4] <- 3
miss.A <- c(1,3)


## ••• Define marginal variables corresponds to the CEG
##           (1) Y: the marginal variables (missing, var)
##           (2) Y.num.states: the number of states for each var
##           (3) Y.A: split location
##           (4) Y.states: state space of each variable

Y <- c(1,2,3)
Y.num.states <- c(3,3,3)
Y.A <- c(1,3)
Y.states <- list()
Y.states[[1]] <- matrix(c(1,0,0,2,0,1),nrow=3,ncol=2)
Y.states[[2]] <- matrix(c(1,2,3),nrow=3,ncol=1)
Y.states[[3]] <- Y.states[[1]]



## ••• Stages
stages <- vector("list",13)
stages[[1]] <- 0; stages[[2]] <- 1;stages[[3]] <- 2;stages[[4]] <- 3; stages[[5]] <- 4
stages[[6]] <- c(5,6); stages[[7]] <- c(7,8,9,10,11); stages[[8]] <- 12;
stages[[9]] <- c(13,14,15);stages[[10]] <- 16; stages[[11]] <- 17; stages[[12]] <- 18; stages[[13]] <- 19
num.stages <- 13

## ••• Prior Elicitation
##         (1) ess: effective sample size
##         (2) alpha: concentration parameters of Dirichlet for transition
##         (3) beta: concentration parameters of Dirichlet for emission
##         (4) r: cut probability prior hyperparameters
##         (5) zeta: Gamma time scale parameter
##         (6) g: Gamma conjugate prior hyperparameters
ess <- 3
alpha <- vector("list",20)
alpha[[1]] <- rep(ess/2,2)
alpha[[2]] <- alpha[[5]] <- rep(ess/4,2)
alpha[[3]] <- rep(ess/8,2)
alpha[[4]] <- rep(ess/12,3)
alpha[[6]] <- rep(ess/16,2)
alpha[[7]] <- rep(ess/8,2)
alpha[[8]] <- alpha[[9]] <- alpha[[10]] <- rep(ess/24,2)
alpha[[11]] <- rep(ess/16,2)
alpha[[12]] <- rep(ess/8,2)
alpha[[13]] <- rep(ess/48,2)
alpha[[14]] <- rep(ess/16,2)
alpha[[15]] <- rep(ess/8,2)
alpha[[16]] <- alpha[[17]] <- rep(ess/48,2)
alpha[[18]] <- alpha[[19]] <- rep(ess*3/16,2)
alpha[[20]] <- rep(ess/8,2)

alpha.stage <- vector("list",13)
alpha.stage[1:5] <- alpha[1:5]
alpha.stage[[6]] <- alpha[[6]]+alpha[[7]]
alpha.stage[[7]] <- alpha[[8]] + alpha[[9]] + alpha[[10]] + alpha[[11]] + alpha[[12]]
alpha.stage[[8]] <- alpha[[13]]
alpha.stage[[9]] <- alpha[[14]] + alpha[[15]] + alpha[[16]]
alpha.stage[[10]] <- alpha[[17]]
alpha.stage[[11]] <- alpha[[18]]
alpha.stage[[12]] <- alpha[[19]]
alpha.stage[[13]] <- alpha[[20]]

beta = vector("list",num_paH)
for(i in 1:num_paH){
  beta[[i]] = matrix(0,nrow = num_paH_va[i],ncol = num_ch_va[i])
  for(j in 1:nrow(beta[[i]])){
    beta[[i]][j,] = ess/(num_paH_va[i]*num_ch_va[i])
  }
}
r = list()
r[[1]] = c(0.5,0.5)
r[[2]] = c(0.5,0.5)
zeta = rep(1.5,42)
g = c(1,3/5)


## ••• on predicting intervention indicator
## rho_lambda is over intervention indicator
##            and drawn from psi
##            assume all rho_lambda are over the same space of I
##            so simply assume drawn from a unique psi
length_I <- 2
psi <- rep(0.5,length_I)
## s_{lambda,I} is drawn from dirichlet(sigma)
##  due to randomness of intervention
##  we do not assume which root cause is more likely to
num_xr <- 5 ## number of random remedies is 5
sigma <- rep(0.5,num_xr)
## number of (lambda,I) combination
## num_lambdaI <- num_path * length_I



 










