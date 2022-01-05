data.exp <- data.frame(temperature=as.numeric(as.factor(datamt[,9])),oil_indicator=as.numeric(as.factor(datamt[,7])),
                       contact=as.numeric(as.factor(datamt[,4])),float=as.numeric(as.factor(datamt[,14])),other_cause = as.numeric(as.factor(datamt[,1])))
oil <- cbind(as.numeric(as.factor(datamt[,12])),as.numeric(as.factor(datamt[,5])),as.numeric(as.factor(datamt[,6])))
newoil <- rep(0,dim(oil)[1])
for(i in 1:dim(oil)[1]){
  if((oil[i,1]==2)&(oil[i,2]==2)&(oil[i,3]==2)){
    newoil[i]=1
  }else{
    if((oil[i,1]==1)&(oil[i,2]==2)&(oil[i,3]==2)){
      newoil[i]=2
    }else{
      if((oil[i,1]==2)&(oil[i,2]==1)&(oil[i,3]==2)){
        newoil[i]=3
      }else{
        if((oil[i,1]==2)&(oil[i,2]==2)&(oil[i,3]==1)){
          newoil[i]=4
        }else{
          if((oil[i,1]==1)&(oil[i,2]==1)&(oil[i,3]==2)){
            newoil[i]=5
          }else{
            if((oil[i,1]==1)&(oil[i,2]==2)&(oil[i,3]==1)){
              newoil[i]=6
            }else{
              if((oil[i,1]==2)&(oil[i,2]==1)&(oil[i,3]==1)){
                newoil[i]=7
              }else{
                if((oil[i,1]==1)&(oil[i,2]==1)&(oil[i,3]==1)){
                  newoil[i]=8
                }
              }
            }
          }
        }
      }
    }
  }
}

symp <-cbind(as.numeric(as.factor(datamt[,13])),as.numeric(as.factor(datamt[,2])),as.numeric(as.factor(datamt[,3])))
newsymp <- rep(0,dim(symp)[1])
for(i in 1:dim(symp)[1]){
  if((symp[i,1]==3)&(symp[i,2]==3)&(symp[i,3]==4)){
    newsymp[i]=1
  }else{
    if((symp[i,1]==1)&(symp[i,2]==3)&(symp[i,3]==4)){
      newsymp[i]=2
    }else{
      if((symp[i,1]==2)&(symp[i,2]==3)&(symp[i,3]==4)){
        newsymp[i]=3
      }else{
        if((symp[i,1]==4)&(symp[i,2]==3)&(symp[i,3]==4)){
          newsymp[i]=4
        }else{
          if((symp[i,1]==3)&(symp[i,2]==1)&(symp[i,3]==4)){
            newsymp[i]=5
          }else{
            if((symp[i,1]==3)&(symp[i,2]==2)&(symp[i,3]==4)){
              newsymp[i]=6
            }else{
              if((symp[i,1]==3)&(symp[i,2]==3)&(symp[i,3]==1)){
                newsymp[i]=7
              }else{
                if((symp[i,1]==3)&(symp[i,2]==3)&(symp[i,3]==2)){
                  newsymp[i]=8
                }else{
                  if((symp[i,1]==3)&(symp[i,2]==3)&(symp[i,3]==3)){
                    newsymp[i]=9
                  }else{
                    if((symp[i,1]==3)&(symp[i,2]==2)&(symp[i,3]==3)){
                      newsymp[i]=10
                    }else{
                      if((symp[i,1]==3)&(symp[i,2]==1)&(symp[i,3]==3)){
                        newsymp[i]=11
                      }else{
                        if((symp[i,1]==4)&(symp[i,2]==3)&(symp[i,3]==3)){
                          newsymp[i]=12
                        }else{
                          if((symp[i,1]==4)&(symp[i,2]==2)&(symp[i,3]==4)){
                            newsymp[i]=13
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

trala<-cbind(as.numeric(as.factor(datamt[,10])),as.numeric(as.factor(datamt[,8])))
newtrala <- rep(0,dim(trala)[1])
for(i in 1:dim(trala)[1]){
  if((trala[i,1]==1)&(trala[i,2]==2)){
    newtrala[i]=1
  }else{
    if((trala[i,1]==2)&(trala[i,2]==2)){
      newtrala[i]=2
    }else{
      if((trala[i,1]==4)&(trala[i,2]==2)){
        newtrala[i]=3
      }else{
        if((trala[i,1]==4)&(trala[i,2]==1)){
          newtrala[i]=4
        }else{
          if((trala[i,1]==3)&(trala[i,2]==1)){
            newtrala[i]=5
          }else{
            if((trala[i,1]==2)&(trala[i,2]==1)){
              newtrala[i]=6
            }else{
              if((trala[i,1]==1)&(trala[i,2]==1)){
                newtrala[i]=7
              }else{
                if((trala[i,1]==4)&(trala[i,2]==3)){
                  newtrala[i]=8
                }
              }
            }
          }
        }
      }
    }
  }
}

data.exp$oil=newoil
data.exp$symo=newsymp
data.exp$trala=newtrala

pathpool <- list()
for(i in 1:dim(data.exp)[1]){
  #######################################################################
  if(data.exp$temperature[i]!=3){
    if((data.exp$oil[i]==1)||(data.exp$oil[i]==3)||(data.exp$oil[i]==4)){
      if(data.exp$symo[i]==1){
        pathpool[[i]]=c(1,2)
      }else{
        if(data.exp$symo[i]%in%c(2,3,4)){
          if(data.exp$trala[i]%in%c(1,2,5,6,7)){
            pathpool[[i]]=c(1,3)
          }else{
            pathpool[[i]]=c(2,4)
          }
        }else{
          pathpool[[i]]=c(3,4)
        }
      }
    }else{
      if(data.exp$symo[i]==1){
        pathpool[[i]]=c(5,6)
      }else{
        if(data.exp$symo[i]%in%c(2,3,4)){
          if(data.exp$trala[i]%in%c(1,2,5,6,7)){
            pathpool[[i]]=c(5,7)
          }else{
            pathpool[[i]]=c(6,8)
          }
        }else{
          pathpool[[i]]=c(7,8)
        }
      }
    }
  }else{ ## temp is na and
    if(data.exp$oil_indicator[i]!=3){
      if((data.exp$oil[i]==1)||(data.exp$oil[i]==3)||(data.exp$oil[i]==4)){
        if(data.exp$symo[i]==1){
          pathpool[[i]]=c(9,10)
        }else{
          if(data.exp$symo[i]%in%c(2,3,4)){
            if(data.exp$trala[i]%in%c(1,2,5,6,7)){
              pathpool[[i]]=c(9,11)
            }else{
              pathpool[[i]]=c(10,12)
            }
          }else{
            pathpool[[i]]=c(11,12)
          }
        }
      }else{
        if(data.exp$symo[i]==1){
          pathpool[[i]]=c(13,14)
        }else{
          if(data.exp$symo[i]%in%c(2,3,4)){
            if(data.exp$trala[i]%in%c(1,2,5,6,7)){
              pathpool[[i]]=c(13,15)
            }else{
              pathpool[[i]]=c(14,16)
            }
          }else{
            pathpool[[i]]=c(15,16)
          }
        }
      }
    }else{## temp is na and oil is na
      if(data.exp$contact[i]!=7){
        if((data.exp$oil[i]==1)||(data.exp$oil[i]==3)||(data.exp$oil[i]==4)){
          if(data.exp$symo[i]==1){
            pathpool[[i]]=c(17,18)
          }else{
            if(data.exp$symo[i]%in%c(2,3,4)){
              if(data.exp$trala[i]%in%c(1,2,5,6,7)){
                pathpool[[i]]=c(17,19)
              }else{
                pathpool[[i]]=c(18,20)
              }
            }else{
              pathpool[[i]]=c(19,20)
            }
          }
        }else{
          if(data.exp$symo[i]==1){
            pathpool[[i]]=c(21,22)
          }else{
            if(data.exp$symo[i]%in%c(2,3,4)){
              if(data.exp$trala[i]%in%c(1,2,5,6,7)){
                pathpool[[i]]=c(21,23)
              }else{
                pathpool[[i]]=c(22,24)
              }
            }else{
              pathpool[[i]]=c(23,24)
            }
          }
        }
      }else{## temp is na, oil is na, contact is na,
        if(data.exp$float[i]!=4){
          if((data.exp$oil[i]==1)||(data.exp$oil[i]==3)||(data.exp$oil[i]==4)){
            if(data.exp$symo[i]==1){
              pathpool[[i]]=c(25,26)
            }else{
              if(data.exp$symo[i]%in%c(2,3,4)){
                if(data.exp$trala[i]%in%c(1,2,5,6,7)){
                  pathpool[[i]]=c(25,27)
                }else{
                  pathpool[[i]]=c(26,28)
                }
              }else{
                pathpool[[i]]=c(27,28)
              }
            }
          }else{
            if(data.exp$symo[i]==1){
              pathpool[[i]]=c(29,30)
            }else{
              if(data.exp$symo[i]%in%c(2,3,4)){
                if(data.exp$trala[i]%in%c(1,2,5,6,7)){
                  pathpool[[i]]=c(29,31)
                }else{
                  pathpool[[i]]=c(30,32)
                }
              }else{
                pathpool[[i]]=c(31,32)
              }
            }
          }
        }else{ ### else is other cause
          if((data.exp$oil[i]==1)||(data.exp$oil[i]==3)||(data.exp$oil[i]==4)){
            if(data.exp$symo[i]==1){
              pathpool[[i]]=c(33,34)
            }else{
              if(data.exp$symo[i]%in%c(2,3,4)){
                if(data.exp$trala[i]%in%c(1,2,5,6,7)){
                  pathpool[[i]]=c(33,35)
                }else{
                  pathpool[[i]]=c(34,36)
                }
              }else{
                pathpool[[i]]=c(35,36)
              }
            }
          }else{
            if(data.exp$symo[i]==1){
              pathpool[[i]]=c(37,38)
            }else{
              if(data.exp$symo[i]%in%c(2,3,4)){
                if(data.exp$trala[i]%in%c(1,2,5,6,7)){
                  pathpool[[i]]=c(37,39)
                }else{
                  pathpool[[i]]=c(38,40)
                }
              }else{
                pathpool[[i]]=c(39,40)
              }
            }
          }
        }
      }
    }
  }
}
pathp=vector("list",length(pathpool))
for(i in 1:length(pathpool)){
  pathp[[i]]=rep(1/length(pathpool[[i]]),length(pathpool[[i]]))
}
childemiss <- c(1,2,3,4,5,6,6,7,7,8,8)
parh<-list()
paths.par=cbind(rep(1:5,each=8),rep(rep(c(6,7),each=4),5),rep(rep(c(8,9),each=2),10),rep(c(10,11),20))
for(i in 1:40){
  parh[[i]]=paths.par[i,]
}
npah <- list()
npah[[1]]=c(0,0,0)
npah[[2]]=c(0,0,0,0)
npah[[3]]=rep(0,10)
npah[[4]]=rep(0,4)
npah[[5]]=rep(0,19)
npah[[6]]=npah[[7]]=rep(0,8)
npah[[8]]=npah[[9]]=rep(0,13)
npah[[10]]=npah[[11]]=rep(0,8)

f.v.e=list()
f.v.e[[1]]=c(0,0,0,0,0)
for(i in 2:36){
  f.v.e[[i]]=c(0,0)
}
path.f.v.e <- list()
path.f.v.e[[1]] <- matrix(c(1,2,7,17,1,1,1,1),ncol=2)
path.f.v.e[[2]] <- matrix(c(1,2,7,17,1,1,1,2),ncol=2)
path.f.v.e[[3]] <- matrix(c(1,2,7,18,1,1,2,1),ncol=2)
path.f.v.e[[4]] <- matrix(c(1,2,7,18,1,1,2,2),ncol=2)
path.f.v.e[[5]] <- matrix(c(1,2,8,19,1,2,1,1),ncol=2)
path.f.v.e[[6]] <- matrix(c(1,2,8,19,1,2,1,2),ncol=2)
path.f.v.e[[7]] <- matrix(c(1,2,8,20,1,2,2,1),ncol=2)
path.f.v.e[[8]] <- matrix(c(1,2,8,20,1,2,2,2),ncol=2)
path.f.v.e[[9]] <- matrix(c(1,3,9,21,2,1,1,1),ncol=2)
path.f.v.e[[10]] <- matrix(c(1,3,9,21,2,1,1,2),ncol=2)
path.f.v.e[[11]] <- matrix(c(1,3,9,22,2,1,2,1),ncol=2)
path.f.v.e[[12]] <- matrix(c(1,3,9,22,2,1,2,2),ncol=2)
path.f.v.e[[13]] <- matrix(c(1,3,10,23,2,2,1,1),ncol=2)
path.f.v.e[[14]] <- matrix(c(1,3,10,23,2,2,1,2),ncol=2)
path.f.v.e[[15]] <- matrix(c(1,3,10,24,2,2,2,1),ncol=2)
path.f.v.e[[16]] <- matrix(c(1,3,10,24,2,2,2,2),ncol=2)

path.f.v.e[[17]] <- matrix(c(1,4,11,25,3,1,1,1),ncol=2)
path.f.v.e[[18]] <- matrix(c(1,4,11,25,3,1,1,2),ncol=2)
path.f.v.e[[19]] <- matrix(c(1,4,11,26,3,1,2,1),ncol=2)
path.f.v.e[[20]] <- matrix(c(1,4,11,26,3,1,2,2),ncol=2)
path.f.v.e[[21]] <- matrix(c(1,4,12,27,3,2,1,1),ncol=2)
path.f.v.e[[22]] <- matrix(c(1,4,12,27,3,2,1,2),ncol=2)
path.f.v.e[[23]] <- matrix(c(1,4,12,28,3,2,2,1),ncol=2)
path.f.v.e[[24]] <- matrix(c(1,4,12,28,3,2,2,2),ncol=2)

path.f.v.e[[25]] <- matrix(c(1,5,13,29,4,1,1,1),ncol=2)
path.f.v.e[[26]] <- matrix(c(1,5,13,29,4,1,1,2),ncol=2)
path.f.v.e[[27]] <- matrix(c(1,5,13,30,4,1,2,1),ncol=2)
path.f.v.e[[28]] <- matrix(c(1,5,13,30,4,1,2,2),ncol=2)
path.f.v.e[[29]] <- matrix(c(1,5,14,31,4,2,1,1),ncol=2)
path.f.v.e[[30]] <- matrix(c(1,5,14,31,4,2,1,2),ncol=2)
path.f.v.e[[31]] <- matrix(c(1,5,14,32,4,2,2,1),ncol=2)
path.f.v.e[[32]] <- matrix(c(1,5,14,32,4,2,2,2),ncol=2)

path.f.v.e[[33]] <- matrix(c(1,6,15,33,5,1,1,1),ncol=2)
path.f.v.e[[34]] <- matrix(c(1,6,15,33,5,1,1,2),ncol=2)
path.f.v.e[[35]] <- matrix(c(1,6,15,34,5,1,2,1),ncol=2)
path.f.v.e[[36]] <- matrix(c(1,6,15,34,5,1,2,2),ncol=2)
path.f.v.e[[37]] <- matrix(c(1,6,16,35,5,2,1,1),ncol=2)
path.f.v.e[[38]] <- matrix(c(1,6,16,35,5,2,1,2),ncol=2)
path.f.v.e[[39]] <- matrix(c(1,6,16,36,5,2,2,1),ncol=2)
path.f.v.e[[40]] <- matrix(c(1,6,16,36,5,2,2,2),ncol=2)

ess <- 10

alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/10)* c(1,1,1,1,1)
alpha_prior[[2]] <- alpha_prior[[3]]<-alpha_prior[[4]]<-
  alpha_prior[[5]]<-alpha_prior[[6]]<-ess*(1/20)*c(1,1)
alpha_prior[[7]]<-alpha_prior[[8]]<-alpha_prior[[9]]<-
  alpha_prior[[10]]<-alpha_prior[[11]]<-alpha_prior[[12]]<-
  alpha_prior[[13]]<-alpha_prior[[14]]<-alpha_prior[[15]]<-alpha_prior[[16]]<-ess*(1/40)*c(1,1)  
alpha_prior[[17]] <- alpha_prior[[18]] <-alpha_prior[[19]] <-alpha_prior[[20]] <-
  alpha_prior[[21]] <-alpha_prior[[22]] <-alpha_prior[[23]] <-alpha_prior[[24]] <-
  alpha_prior[[25]] <-alpha_prior[[26]] <-alpha_prior[[27]] <-alpha_prior[[28]] <-
  alpha_prior[[29]] <-alpha_prior[[30]] <-alpha_prior[[31]] <-alpha_prior[[32]] <-
  alpha_prior[[33]] <-alpha_prior[[34]] <-alpha_prior[[35]] <-alpha_prior[[36]] <-ess*(1/80)*c(1,1)

beta_prior <-list()
pn <- 1
#pn=5
beta_prior[[1]] <- pn*c(1,1,0.1)
beta_prior[[2]] <- pn*c(1,1,0.1,1)
beta_prior[[3]] <- pn*c(1,1,1,1,1,1,0.1,1,1,1)
beta_prior[[4]] <- pn*c(1,1,1,0.1)
beta_prior[[5]] <- pn*c(rep(1,14),0.1,1,1,1,1)
beta_prior[[6]] <- pn*c(1,0.1,1,1,0.1,0.1,0.1,0.1)
beta_prior[[7]] <- pn*c(0.1,1,0.1,0.1,1,1,1,1)
beta_prior[[8]] <- pn*c(1,1,1,1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
beta_prior[[9]] <- pn*c(0.1,0.5,0.5,0.5,1,1,1,1,1,1,1,1,1)
beta_prior[[10]] <- pn*c(1,1,0.1,0.1,1,1,1,0.1)
beta_prior[[11]] <- pn*c(0.1,0.1,1,1,0.1,0.1,0.1,1)
source("~/Documents/CODE/exp_hier_init.R")
E0<-ini_p(M=314,n.path=40,pathpool=pathpool,pathp=pathp,
          Ny=npah,pa=parh,ch=childemiss,O=as.matrix(data.exp),Ne=f.v.e,path.Ne=path.f.v.e)
E1 <- HCAgibbs(M=314,n.path=40,pathpool=pathpool,pathp=pathp,
               Ny=npah,pa=parh,ch=childemiss,O=as.matrix(data.exp),Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
               alpha_prior,beta_prior)
require(rlist)
list.save(E1,"E1.rds")
E1=list.load("E1.rds")

# mixture:
plot(1:10000,E1$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,E1$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)
# model selection
ess=10
a <- ess * c(1/2,1/10,1/10,rep(1/20,10),rep(1/40,20),rep(1/80,40))
alphaprior <- list()
edgenum <- c(2,5,5,rep(2,70))
for(i in 1:73){
  alphaprior[[i]] = rep(a[i],edgenum[i])
}
est.path <- c()
for(i in 1:314){
  cts = rep(0,length(pathpool[[i]]))
  for(j in 1:length(pathpool[[i]])){
    cts[j]=sum(E1$Paths[i,1:10001]==pathpool[[i]][j])
  }
  est.path[i]=pathpool[[i]][which.max(cts)]
}
path.sit <- matrix(0,nrow=80,ncol=5)
path.sit[,2]<-c(rep(1,40),rep(2,40))
path.sit[,3]<-rep(3:12,each=8)
path.sit[,4]<-rep(13:32,each=4)
path.sit[,5]<-rep(33:72,each=2)
path.ed <-matrix(0,nrow=80,ncol=5)
path.ed[,1]<-rep(1:2,each=40)
path.ed[,2]<-rep(rep(1:5,each=8),2)
path.ed[,3]<-rep(rep(1:2,each=4),10)
path.ed[,4]<-rep(rep(1:2,each=2),20)
path.ed[,5]<-rep(1:2,40)
ct.Df <- counts.collector(nsit=73,nedge=edgenum,n=314,paths=est.path,vertex=path.sit,edge=path.ed)
alphapostf <- alphaprior 
for(i in 1:73){
  alphapostf[[i]] <- alphaprior[[i]] + ct.Df[[i]]}
to.merge=list()
to.merge[[1]]<-c(1,2) + 1
to.merge[[2]]<-3:12 + 1
to.merge[[3]]<-13:32 + 1
to.merge[[4]]<-33:72 + 1

localscoresf = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresf[[i]][j] = llk_local(stage,alphaprior,alphapostf)
  }
}
modelf= ahc_search(alphaprior,alphapostf,to.merge,localscoresf) 
scoref = sum(unlist(modelf$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapostf[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapostf[[1]])) # -6214.01
estf=list()
estf[[1]] = alphapostf[[1]]/sum(alphapostf[[1]])
for(i in 1:length(to.merge)){
  estf[[i+1]] = list();
  for(j in 1:length(modelf$post[[i]])){
    estf[[i+1]][[j]] = modelf$post[[i]][[j]]/sum(modelf$post[[i]][[j]])
  }
}

ess <- 10

alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/10)* c(1,1,1,1,1)
alpha_prior[[2]] <- alpha_prior[[5]]<-ess*(1/10)*c(1,1)
alpha_prior[[3]]<-alpha_prior[[4]]<-alpha_prior[[6]]<-ess*(3/20)*c(1,1)
alpha_prior[[7]]<-alpha_prior[[9]]<-alpha_prior[[12]]<-ess*(1/5)*c(1,1)  
alpha_prior[[8]]<-alpha_prior[[11]]<-alpha_prior[[13]]<-alpha_prior[[16]]<-ess*(9/40)*c(1,1) 
alpha_prior[[10]]<-alpha_prior[[14]]<-alpha_prior[[15]]<-ess*(3/40)*c(1,1)  
alpha_prior[[23]] <-alpha_prior[[25]] <-alpha_prior[[28]] <-alpha_prior[[31]]  <-
  alpha_prior[[33]] <-alpha_prior[[35]] <-ess*(3/40)*c(1,1)
alpha_prior[[27]] <-alpha_prior[[36]] <-ess*(1/40)*c(1,1)
alpha_prior[[17]] <- alpha_prior[[18]] <-alpha_prior[[19]] <-alpha_prior[[20]] <-
  alpha_prior[[21]] <-alpha_prior[[22]] <-alpha_prior[[24]] <- alpha_prior[[26]] <-
  alpha_prior[[29]] <-alpha_prior[[30]] <-alpha_prior[[32]]<-alpha_prior[[34]] <- ess*(3/8)*c(1,1)


E2 <- HCAgibbs(M=314,n.path=40,pathpool=pathpool,pathp=pathp,
               Ny=npah,pa=parh,ch=childemiss,O=as.matrix(data.exp),Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
               alpha_prior,beta_prior)
list.save(E2,"E2.rds")
plot(1:10000,E2$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,E2$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)
edge0<-f.v.e
#for(i in 1:10001){
for(j in 1:314){
  edge0 <- mapply("+",edge0,cted(E2$Paths[j,10001],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
}
#}
stage <- list()
stage[[1]]<-1
stage[[2]]<-c(2,5)
stage[[3]]<-c(3,4,6)
stage[[4]]<-c(7,9,12)
stage[[5]]<-c(8,11,13,16)
stage[[6]]<-c(10,14,15)
stage[[7]]<-c(17:22,24,26,29,30,32,34)
stage[[8]]<-c(23,25,28,31,33,35)
stage[[9]]<-c(27,36)
stage.pr<-list()
stage.pr[[1]]=alpha_prior[[1]]
stage.pr[[2]]=alpha_prior[[2]]
stage.pr[[3]]=alpha_prior[[3]]
stage.pr[[4]]=alpha_prior[[7]]
stage.pr[[5]]=alpha_prior[[8]]
stage.pr[[6]]=alpha_prior[[10]]
stage.pr[[7]]=alpha_prior[[17]]
stage.pr[[8]]=alpha_prior[[23]]
stage.pr[[9]]=alpha_prior[[27]]
stage.ed<-list()
stage.ed[[1]]=c(0,0,0,0,0)
stage.ed[[2]]<-stage.ed[[3]]<-stage.ed[[4]]<-stage.ed[[5]]<-
  stage.ed[[6]]<-stage.ed[[7]]<-stage.ed[[8]]<-stage.ed[[9]]<-c(0,0)
for(i in 1:9){
  for(j in 1:length(stage[[i]])){
    stage.ed[[i]]<-stage.ed[[i]]+edge0[[stage[[i]][j]]]   
  }
}
stage.pt<-mapply("+",stage.pr,stage.ed)
stage.pr.e<-lapply(stage.pt,function(x){x/sum(x)})


ynum1<-npah
for(j in 1:314){
  ys1 <- parh[[E2$Paths[j,10001]]]
  print(ys1)
  ind1 <- childemiss[ys1]
  va1<-as.matrix(data.exp)[j,ind1]
  for(k in 1:length(ys1)){
    ynum1[[ys1[k]]][va1[k]] <- ynum1[[ys1[k]]][va1[k]]+1
  }
}

emiss.ct<-mapply("+",beta_prior,ynum1)
emiss.est<-lapply(emiss.ct,function(x){x/sum(x)})
