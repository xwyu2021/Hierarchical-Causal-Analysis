#############################################################
################ Experiments ################################
#############################################################

## ••• collect training data

trainDF<- read.csv("Desktop/thesis pack/data/traindata500.csv")
trainD <- matrix(c(trainDF$system,trainDF$seal,trainDF$temperature,
                   trainDF$level,trainDF$leak,trainDF$alarm,trainDF$pipe,
                   trainDF$sight.glass,trainDF$buchholz,trainDF$transformer,
                   trainDF$ID,as.numeric(trainDF$remedy) - 1,trainDF$remedyID),ncol = 13)
trainCORE <- trainD[,1:10]
trainID <- trainD[,11]
trainINTVT <- trainD[,12]
trainREMEDY <- trainD[,13]
trainTIME <- trainDF$failtime/365
trainFAIL <- trainDF$revise.fail
## ••• source rcpp metropolis hastings algorithms

Rcpp::sourceCpp('Desktop/thesis pack/code/Initial_intervention_re.cpp')
Rcpp::sourceCpp('Desktop/thesis pack/code/alg_intervention_re.cpp')
## ••• source and run model 1

source("Desktop/thesis pack/code/thesisMOD1.R")

zeroint = initial_intvt(M = 500, num_o, D=trainCORE, A, rangeA, num_path, 
                        num_pos, num_pos_edge, path.pos,
                        path.edge, path.y, miss.A, 
                        y.edge,states, state.parents,path.miss.f,path.nm.f,path.miss.ok,path.nm.ok,
                        trainFAIL,Y.num.states,
                        Y.A,Y.states, pa.gn.ind, dsup.ind, child.ind,pa.child.ind,3,num_paH, 
                        num_paH_va, num_ch_va,ID=trainID,intervention=trainINTVT,
                        remedy=trainREMEDY,length_I, num_xr,num_ID=20)
neID = zeroint$n_eID
nlambda = zeroint$n_lambda
lambdaM = zeroint$path
Yind0 = zeroint$Yind
nA = zeroint$n_A
AM = zeroint$cuts
PAind0 = zeroint$PAind
npa = zeroint$n_pa
nI = zeroint$n_I
Iind0 = zeroint$Iind
nxr = zeroint$n_xr
mod = HCAGIBBS_intvt(neID,lambdaM,Yind0,nA,AM,PAind0,npa,
                     nI,nxr,Iind0,
                     M=500,D=trainCORE,num_o,num_ch_va,states,trainTIME,
                     num_path,num_pos,num_pos_edge,path.pos,
                     path.edge,path.y,y.edge,num_paH,num_paH_va,
                     pa.gn.ind,dsup.ind,child.ind,pa.child.ind,
                     state.parents,path.miss.f,path.nm.f,path.miss.ok,path.nm.ok,
                     trainFAIL,3,A,
                     rangeA,miss.A,Y.A,Y.num.states,Y.states,
                     stages,num.stages,alpha.stage,beta,r,zeta,g,iteration=3,ID=trainID,intervention=trainINTVT,
                     remedy=trainREMEDY,length_I, num_xr,num_ID=20,sigma,psi) 

