########################################################
####################   simulation  #####################
########################################################
emis.true <- list()
emis.true[[1]] <- c(0.6,0.4)
emis.true[[2]] <- c(0.6,0.4)
emis.true[[3]] <- c(0.5,0.5)
emis.true[[4]] <- c(0.5,0.5)
emis.true[[5]] <- c(0.4,0.6)
emis.true[[6]] <- c(0.4,0.6)
emis.true[[7]] <- c(0.4,0.4,0.1,0.1)
emis.true[[8]] <- c(0.4,0.4,0.1,0.1)
emis.true[[9]] <- c(0.7,0.3)
emis.true[[10]] <- c(0.2,0.2,0.1,0.5)
emis.true[[11]] <- c(0.5,0.5)
emis.true[[12]] <- c(1)
emis.true[[13]] <- c(0.5,0.5)
emis.true[[14]] <- c(1)
emis.true[[15]] <- c(0.1,0.9)
emis.true[[16]] <- c(1)
emis.true[[17]] <- c(1)
emis.true[[18]] <- c(1)
emis.ind <- list()
emis.ind[[1]] <- 1
emis.ind[[2]] <- 1
emis.ind[[3]] <- 2
emis.ind[[4]] <- 2
emis.ind[[5]] <- 3
emis.ind[[6]] <- 3
emis.ind[[7]] <- c(5,6)
emis.ind[[8]] <- c(5,6)
emis.ind[[9]] <- 8
emis.ind[[10]] <- c(9,10)
emis.ind[[11]] <- 4
emis.ind[[12]] <- 4
emis.ind[[13]] <- 4
emis.ind[[14]] <- 4
emis.ind[[15]] <- 4
emis.ind[[16]] <- 4
emis.ind[[17]] <- 7
emis.ind[[18]] <- 7
emis.va <- list()
emis.va[[1]]<-matrix(c(1,2),ncol=2,nrow=1)
emis.va[[6]]<- emis.va[[5]]<-emis.va[[4]] <- emis.va[[3]]<-emis.va[[2]] <- emis.va[[1]]
emis.va[[8]]<- emis.va[[7]]<-matrix(c(1,1,1,2,2,1,2,2),nrow=2,ncol=4)
emis.va[[9]]<- matrix(c(1,2),ncol=2,nrow=1)
emis.va[[10]]<-emis.va[[8]]
emis.va[[11]]<- matrix(c(1,2),ncol=2,nrow=1)
emis.va[[12]]<- matrix(c(3),ncol=1,nrow=1)
emis.va[[13]] <- emis.va[[11]]
emis.va[[14]] <- emis.va[[12]]
emis.va[[15]] <- matrix(c(1,4),ncol=2,nrow=1)
emis.va[[16]]<- matrix(c(5),ncol=1,nrow=1)
emis.va[[17]]<- matrix(1,ncol=1,nrow=1)
emis.va[[18]]<- matrix(2,ncol=1,nrow=1)

find.p <- data.frame(v=c(3,3,3,3,4,4,5,5,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14),
                     e=c(1,2,3,4,1,2,1,2,3,4,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2),
                     r=c(1,3,5,7,9,10,2,4,6,8,9,10,11,12,11,12,15,16,17,18,13,14,13,14,15,16,17,18))


## sample paths
samplepath <- read.csv("Documents/DATA/pathREM.csv")
samplepath <- samplepath[,-1]
t.tl <- c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10,20)
convpath <- t.tl[samplepath]
Df <- convpath[which(convpath < 11)]

find.p <- data.frame(v=c(3,3,3,3,4,4,7,7,8,8,9,9,10,10),
                     e=c(1,2,3,4,1,2,1,2,1,2,1,2,1,2),
                     r=c(1,2,3,4,5,6,7,8,7,8,9,10,11,12))

emis.true <- list()
emis.true[[1]] <- c(0.6,0.4)
emis.true[[2]] <- c(0.5,0.5)
emis.true[[3]] <- c(0.4,0.6)
emis.true[[4]] <- c(0.4,0.4,0.1,0.1)
emis.true[[5]] <- c(0.7,0.3,0,0,0,0)
emis.true[[6]] <- c(0,0,0.2,0.2,0.1,0.5)
emis.true[[7]] <- c(0.5,0.5,0,0,0)
emis.true[[8]] <- c(0,0,1,0,0)
emis.true[[9]] <- c(0.1,0,0,0.9,0)
emis.true[[10]] <- c(0,0,0,0,1)
emis.true[[11]] <- c(1,0)
emis.true[[12]] <- c(0,1)

emis.var <- c(1,2,3,5,7,7,4,4,4,4,6,6)

obs2 <- read.csv("Documents/DATA/obs2REM.csv")
obs2 <- obs2[,-1]
obs2<-as.matrix(obs2)
obs.pos = data.frame(o=c(1,2,3,4,4,4,5,6,7),
                     p=c(7,8,9,7,8,9,10,10,4))

tlpos <- read.csv("Documents/DATA/treeposREML.csv",header=FALSE)
tlpos <- as.matrix(tlpos)
pathpool=list()
for(i in 1:length(Df)){
  events <- which(obs2[i,]!=0)
  vs <- obs.pos$p[which(obs.pos$o == events[1])]
  lam<-c()
  for(l in 1:10){
    if(any(tlpos[l,]%in% vs)){
      lam<-c(lam,l)
    }
  }
  if(length(events)>1){
    for(j in 2:length(events)){
      vs <- obs.pos$p[which(obs.pos$o == events[j])]
      ps<-c()
      for(l in 1:10){
        if(any(tlpos[l,]%in% vs)){
          ps<-c(ps,l)
        }
      }
      lam <- intersect(lam,ps) 
    }
  }
  pathpool[[i]]=lam[lam< 11]
}

pipath <-c(0.2505943,0.04698644,0.2505943,0.04698644,
           0.04111313,0.02740875,0.03181373,0.03181373,
           0.1636135,0.1090757)
pathp <- list()
for(i in 1:length(Df)){
  pathp[[i]] <- pipath[pathpool[[i]]]/sum(pipath[pathpool[[i]]])
}

pa <- list()
pa[[1]] <- c(1,11)
pa[[2]] <- c(1,12)
pa[[3]] <- c(3,11)
pa[[4]] <- c(3,12)
pa[[5]] <- c(5,15)
pa[[6]] <- c(5,16)
pa[[7]] <- c(7,17)
pa[[8]] <- c(7,18)
pa[[9]] <- c(9)
pa[[10]] <- c(10)


ch=c(1,1,2,2,3,3,5,5,7,7,4,4,4,4,4,4,6,6)
Ny0 <- list()
Ny0[[1]] <-Ny0[[2]] <- Ny0[[3]] <- Ny0[[4]] <- Ny0[[5]]<-Ny0[[6]] <- c(0,0)
Ny0[[7]]<-Ny0[[8]] <- c(0,0,0,0)
Ny0[[10]]<-Ny0[[9]]<-c(0,0,0,0,0,0)
Ny0[[11]]<-Ny0[[12]]<-Ny0[[13]]<-Ny0[[14]]<-Ny0[[15]]<-Ny0[[16]]<-c(0,0,0,0,0)
Ny0[[17]]<-Ny0[[18]]<-c(0,0)

Ny.rm0 <- Ny0[sort(unique(unlist(pa)))]
ch.rm0 <- ch[sort(unique(unlist(pa)))]
pa.rm0 <- list()
pa.rm0[[1]] <- c(1,7)
pa.rm0[[2]] <- c(1,8)
pa.rm0[[3]] <- c(2,7)
pa.rm0[[4]] <- c(2,8)
pa.rm0[[5]] <- c(3,9)
pa.rm0[[6]] <- c(3,10)
pa.rm0[[7]] <- c(4,11)
pa.rm0[[8]] <- c(4,12)
pa.rm0[[9]] <- c(5)
pa.rm0[[10]] <- c(6)
ch.rm0 <-c(1,2,3,5,7,7,4,4,4,4,6,6)

##################
## plain tree: choose a tree
#################
f.v.e <- list()
f.v.e[[1]] <-c(0,0)
f.v.e[[2]] <-c(0,0,0,0)
f.v.e[[3]] <-c(0,0)
f.v.e[[4]] <-c(0,0)
f.v.e[[5]] <-c(0,0)
f.v.e[[6]] <-c(0,0)
f.v.e[[7]] <-c(0,0)

path.f.v.e <- list()
path.f.v.e[[1]] <- matrix(c(1,2,4,1,1,1),ncol=2)
path.f.v.e[[2]] <- matrix(c(1,2,4,1,1,2),ncol=2)
path.f.v.e[[3]] <- matrix(c(1,2,5,1,2,1),ncol=2)
path.f.v.e[[4]] <- matrix(c(1,2,5,1,2,2),ncol=2)
path.f.v.e[[5]] <- matrix(c(1,2,6,1,3,1),ncol=2)
path.f.v.e[[6]] <- matrix(c(1,2,6,1,3,2),ncol=2)
path.f.v.e[[7]] <- matrix(c(1,2,7,1,4,1),ncol=2)
path.f.v.e[[8]] <- matrix(c(1,2,7,1,4,2),ncol=2)
path.f.v.e[[9]] <- matrix(c(1,3,2,1),ncol=2)
path.f.v.e[[10]] <- matrix(c(1,3,2,2),ncol=2)

#################
## staged tree:
##################
f.v.e <- list()
f.v.e[[1]] <-c(0,0)
f.v.e[[2]] <-c(0,0,0,0)
f.v.e[[3]] <-c(0,0)
f.v.e[[4]] <-c(0,0)
f.v.e[[5]] <-c(0,0)
f.v.e[[6]] <-c(0,0)

path.f.v.e <- list()
path.f.v.e[[1]] <- matrix(c(1,2,4,1,1,1),ncol=2)

path.f.v.e[[2]] <- matrix(c(1,2,4,1,1,2),ncol=2)
path.f.v.e[[3]] <- matrix(c(1,2,4,1,2,1),ncol=2)
path.f.v.e[[4]] <- matrix(c(1,2,4,1,2,2),ncol=2)
path.f.v.e[[5]] <- matrix(c(1,2,5,1,3,1),ncol=2)
path.f.v.e[[6]] <- matrix(c(1,2,5,1,3,2),ncol=2)
path.f.v.e[[7]] <- matrix(c(1,2,6,1,4,1),ncol=2)
path.f.v.e[[8]] <- matrix(c(1,2,6,1,4,2),ncol=2)
path.f.v.e[[9]] <- matrix(c(1,3,2,1),ncol=2)
path.f.v.e[[10]] <- matrix(c(1,3,2,2),ncol=2)

prov.t <- list()
prov.t[[1]]<-c(0.5201/0.7151,0.195/0.7151)
prov.t[[2]] <- c(0.2128,0.2128,0.049,0.0455)/0.5201
prov.t[[3]] <- c(0.6,0.4)
prov.t[[4]] <- c(0.64,0.12)/0.76
prov.t[[5]] <- c(0.64,0.12)/0.76
prov.t[[6]] <- c(0.6,0.4)
prov.t[[7]] <- c(0.5,0.5)

pathprob_est <- function(theta,path.ct=path.f.v.e,npath){
  pi <- rep(1,npath)
  for(i in 1:npath){
    t <- path.ct[[i]]
    for(j in 1:nrow(t)){
      pi[i] <- pi[i] * theta[[t[j,1]]][t[j,2]]
    }
  }
  return(pi)
}


pathprob_est(prov.t,path.f.v.e,10)

### prior elicitation
ess <- 1
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
#alpha_prior[[7]] <- ess*(1/16)*c(1,1)
beta_prior <-list()
pn <- 1
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
#beta_prior[[5]] <- pn*c(1,1)
#beta_prior[[6]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)


################################
#### experiment staged tree ####
################################
ess=0.01
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
A0<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
#require(rlist)
#list.save(A0,"A0.rds")


ess=0.1
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
A1<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(A1,"A1.rds")

ess=1

alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
A2<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(A2,"A2.rds")

ess=5

alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
A3<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(A3,"A3.rds")

ess=10
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
A4<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(A4,"A4.rds")
beta_prior <-list()
pn=5
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)
A4.1<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
               Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
               alpha_prior,beta_prior)
list.save(A4.1,"A4b5.rds")

beta_prior <-list()
pn=10
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)
A4.2<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
               Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
               alpha_prior,beta_prior)
list.save(A4.2,"A4b10.rds")

beta_prior <-list()
pn=0.1
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)
A4.0<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
               Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
               alpha_prior,beta_prior)
list.save(A4.0,"A4b01.rds")


###########################################################
#################### diagnostics ##########################
###########################################################
## (1) convergence through paths
# 1.1 difference
lam0 <- apply(A0$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(2200:3900,lam0[2200:3900],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A0$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A0$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)




lam1 <- apply(A1$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lam1[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A1$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A1$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)


lam2 <- apply(A2$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lam2[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A2$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A2$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)

lam3 <- apply(A3$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lam3[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A3$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A3$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)

lam4 <- apply(A4$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lam4[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A4$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A4$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)

# 1.2 running averages
ynum1=Ny.rm0
b1 <- matrix(0,nrow=510001,ncol=2)
for(i in 1:10001){
  for(j in 1:3587){
    ys1 <- pa.rm0[[A1$Paths[j,i]]]
    ind1 <- ch.rm0[ys1]
    va1<-obs2[j,ind1]
    for(k in 1:length(ys1)){
      ynum1[[ys1[k]]][va1[k]] <- ynum1[[ys1[k]]][va1[k]]+1
    }
  }
  l2=ynum1[[11]]
  if(i==1){
    b1[i,]=l2/sum(l2)
    l1=l2
  }else{
    l3=l2-l1
    b1[i,]=l3/sum(l3)
    l1=l2
  }
}
plot(1:10001,b1[1:10001,1],pch=19,cex=0.4,xlab = "iteration",ylab=expression(beta))
ynum0<-ynum2<-ynum3<-ynum4<-Ny.rm0
b0<-b2<- b3<- b4<- matrix(0,nrow=10001,ncol=2)
for(i in 1:10001){
  for(j in 1:3587){
    ys0 <- pa.rm0[[A0$Paths[j,i]]]
    ind0 <- ch.rm0[ys0]
    va0<-obs2[j,ind0]
    for(k in 1:length(ys0)){
      ynum0[[ys0[k]]][va0[k]] <- ynum0[[ys0[k]]][va0[k]]+1
    }
    ys2 <- pa.rm0[[A2$Paths[j,i]]]
    ind2 <- ch.rm0[ys2]
    va2<-obs2[j,ind2]
    for(k in 1:length(ys2)){
      ynum2[[ys2[k]]][va2[k]] <- ynum2[[ys2[k]]][va2[k]]+1
    }
    ys3 <- pa.rm0[[A3$Paths[j,i]]]
    ind3 <- ch.rm0[ys3]
    va3<-obs2[j,ind3]
    for(k in 1:length(ys3)){
      ynum3[[ys3[k]]][va3[k]] <- ynum3[[ys3[k]]][va3[k]]+1
    }
    ys4 <- pa.rm0[[A4$Paths[j,i]]]
    ind4 <- ch.rm0[ys4]
    va4<-obs2[j,ind4]
    for(k in 1:length(ys4)){
      ynum4[[ys4[k]]][va4[k]] <- ynum4[[ys4[k]]][va4[k]]+1
    }
  }
  l2=ynum0[[11]]
  if(i==1){
    b0[i,]=l2/sum(l2)
    l10=l2
  }else{
    l3=l2-l10
    b0[i,]=l3/sum(l3)
    l10=l2
  }
  l2=ynum2[[11]]
  if(i==1){
    b2[i,]=l2/sum(l2)
    l12=l2
  }else{
    l3=l2-l12
    b2[i,]=l3/sum(l3)
    l12=l2
  }
  l2=ynum3[[11]]
  if(i==1){
    b3[i,]=l2/sum(l2)
    l13=l2
  }else{
    l3=l2-l13
    b3[i,]=l3/sum(l3)
    l13=l2
  }
  l2=ynum4[[11]]
  if(i==1){
    b4[i,]=l2/sum(l2)
    l14=l2
  }else{
    l3=l2-l14
    b4[i,]=l3/sum(l3)
    l14=l2
  }
}


#############################
### (2) precision check
# 2.0 average precision of paths
rightpath0=apply(A0$Paths,2,function(x){sum(x==Df)/3587})
rightpath1=apply(A1$Paths,2,function(x){sum(x==Df)/3587})
rightpath2=apply(A2$Paths,2,function(x){sum(x==Df)/3587})
rightpath3=apply(A3$Paths,2,function(x){sum(x==Df)/3587})
rightpath4=apply(A4$Paths,2,function(x){sum(x==Df)/3587})
mean(rightpath0) ## 0.7808749
mean(rightpath0[-(2000:4000)]) ## 0.7896309
mean(rightpath1) ## 0.7616149/0.7965919
mean(rightpath2) ## 0.8006856
mean(rightpath3) ## 0.8608665
mean(rightpath4) ## 0.8668317

# 2.1 precision of paths
n.r.lambda <- sapply(1:10,function(x){length(which(Df==x))})
lam0[length(lam0)] ## 0.7953722
n.0.lambda <- sapply(1:10,function(x){length(which(A0$Paths[,10001]==x))})
m.0.lambda <- sapply(1:10,function(x){mean(apply(A0$Paths[,-(2000:4000)],2,function(y){length(which(y==x))}))})
sd.0.lambda<- sapply(1:10,function(x){sd(apply(A0$Paths[,-(2000:4000)],2,function(y){length(which(y==x))}))})
m.1.lambda <- sapply(1:10,function(x){mean(apply(A1$Paths[,1:10001],2,function(y){length(which(y==x))}))})
sd.1.lambda<- sapply(1:10,function(x){sd(apply(A1$Paths[,1:10001],2,function(y){length(which(y==x))}))})
m.2.lambda <- sapply(1:10,function(x){mean(apply(A2$Paths[,1:10001],2,function(y){length(which(y==x))}))})
sd.2.lambda<- sapply(1:10,function(x){sd(apply(A2$Paths[,1:10001],2,function(y){length(which(y==x))}))})
m.3.lambda <- sapply(1:10,function(x){mean(apply(A3$Paths[,1:10001],2,function(y){length(which(y==x))}))})
sd.3.lambda<- sapply(1:10,function(x){sd(apply(A3$Paths[,1:10001],2,function(y){length(which(y==x))}))})
m.4.lambda <- sapply(1:10,function(x){mean(apply(A4$Paths[,1:10001],2,function(y){length(which(y==x))}))})
sd.4.lambda<- sapply(1:10,function(x){sd(apply(A4$Paths[,1:10001],2,function(y){length(which(y==x))}))})

linedf <- data.frame(alpha=rep(c("true","0.01","0.1","1","5","10"),each=10),path=c(1:10,1:10+0.1,1:10+0.2,1:10+0.3,1:10+0.4,1:10+0.5),
                     count=c(n.r.lambda,m.0.lambda,m.1.lambda,m.2.lambda,m.3.lambda,m.4.lambda),
                     sd=c(rep(0,10),sd.0.lambda,sd.1.lambda,sd.2.lambda,sd.3.lambda,sd.4.lambda))

ggplot(linedf,aes(x=path,y=count,group=alpha,color=alpha))+
  geom_errorbar(aes(ymin=count-sd, ymax=count+sd), width=.2, 
                position=position_dodge(0.05)) +
  geom_point(aes(shape=alpha))+
  theme_classic()+ scale_x_continuous(breaks = 1:10,labels = c(1:10))

# 2.2 precision of transition probabilities/emission probabilities
dim(A4$Paths)
edge4<-edge3<-edge2<-edge1 <- edge0<-f.v.e
for(i in 1:10001){
  for(j in 1:3587){
    edge0 <- mapply("+",edge0,cted(A0$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
    edge2 <- mapply("+",edge2,cted(A2$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
    edge3 <- mapply("+",edge3,cted(A3$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
    edge4 <- mapply("+",edge4,cted(A4$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
  }
}
edge1<-f.v.e
for(i in 1:10001){
  for(j in 1:3587){
    edge1 <- mapply("+",edge1,cted(A1$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
  }
}
edge01 <- f.v.e
for(i in c(1:1999,4001:10001)){
  for(j in 1:3587){
    edge01 <- mapply("+",edge01,cted(A0$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
  }
}
causal.theta <- prov.t[-4]
add <-function(ess){
  fest0=ess*c(1/2,1/2)+c(3587,0);fest0=fest0/sum(fest0)
  fest2=ess*c(1/4,1/4)+c(0,0);fest2=fest2/sum(fest2)
  fest4=ess*c(1/16,1/16,1/16,1/16);fest4=fest4/sum(fest4)
  fest7=ess*c(1/16,1/16);fest7=fest7/sum(fest7)
  return(sqrt(sum((gtp[[1]]-fest0)^2))+sqrt(sum((gtp[[3]]-fest2)^2))+
           sqrt(sum((gtp[[5]]-fest4)^2))+sqrt(sum((gtp[[8]]-fest7)^2)))
}
ess=0.01
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha0=mapply("+",edge0,alpha_prior)
sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha0,function(x) x/sum(x))),
                         function(y){y^2}),sum),sqrt))
add(0.01)+1.652008
# 1.652008, same ## 2.72398
## remove stuck sample
alpha01=mapply("+",edge01,alpha_prior)
sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha01,function(x) x/sum(x))),
                         function(y){y^2}),sum),sqrt))
# 1.544496 ## same
ess=0.1
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha1=mapply("+",edge1,alpha_prior)
sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha1,function(x) x/sum(x))),
                         function(y){y^2}),sum),sqrt))
add(0.1)+1.669829
# 1.672986 same 1.669829##### 2.741783
ess=1
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha2=mapply("+",edge2,alpha_prior)
sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha2,function(x) x/sum(x))),
                         function(y){y^2}),sum),sqrt))
add(1)+0.7961033
# 0.7961033 ### 1.86788
ess=5
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha3=mapply("+",edge3,alpha_prior)
sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha3,function(x) x/sum(x))),
                         function(y){y^2}),sum),sqrt))
add(5)+0.4636632
# 0.4636632  #### 1.534653
ess=10
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha4=mapply("+",edge4,alpha_prior)
sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha4,function(x) x/sum(x))),
                         function(y){y^2}),sum),sqrt))
add(10)+0.1050709
# 0.1050709 ## 1.175079

ynum0<-ynum1<-ynum2<-ynum3<-ynum4<-Ny.rm0
ynum1<-Ny.rm0
for(i in 1:10001){
  for(j in 1:3587){
    ys1 <- pa.rm0[[A1$Paths[j,i]]]
    ind1 <- ch.rm0[ys1]
    va1<-obs2[j,ind1]
    for(k in 1:length(ys1)){
      ynum1[[ys1[k]]][va1[k]] <- ynum1[[ys1[k]]][va1[k]]+1
    }
  }
}
for(i in 1:10001){
  for(j in 1:3587){
    ys0 <- pa.rm0[[A0$Paths[j,i]]]
    ind0 <- ch.rm0[ys0]
    va0<-obs2[j,ind0]
    for(k in 1:length(ys0)){
      ynum0[[ys0[k]]][va0[k]] <- ynum0[[ys0[k]]][va0[k]]+1
    }
    ####
    ys2 <- pa.rm0[[A2$Paths[j,i]]]
    ind2 <- ch.rm0[ys2]
    va2<-obs2[j,ind2]
    for(k in 1:length(ys2)){
      ynum2[[ys2[k]]][va2[k]] <- ynum2[[ys2[k]]][va2[k]]+1
    }
    ####
    ys3 <- pa.rm0[[A3$Paths[j,i]]]
    ind3 <- ch.rm0[ys3]
    va3<-obs2[j,ind3]
    for(k in 1:length(ys3)){
      ynum3[[ys3[k]]][va3[k]] <- ynum3[[ys3[k]]][va3[k]]+1
    }
    ####
    ys4 <- pa.rm0[[A4$Paths[j,i]]]
    ind4 <- ch.rm0[ys4]
    va4<-obs2[j,ind4]
    for(k in 1:length(ys4)){
      ynum4[[ys4[k]]][va4[k]] <- ynum4[[ys4[k]]][va4[k]]+1
    }
  }
}

ynum01=Ny.rm0

for(i in c(1:1999,4001:10001)){
  for(j in 1:3587){
    ys0 <- pa.rm0[[A0$Paths[j,i]]]
    ind0 <- ch.rm0[ys0]
    va0<-obs2[j,ind0]
    for(k in 1:length(ys0)){
      ynum01[[ys0[k]]][va0[k]] <- ynum01[[ys0[k]]][va0[k]]+1
    }
  }
}

beta0=mapply("+",beta_prior,ynum0)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta0,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))
## 3.304136----3.30395
## remove burn-in
beta01=mapply("+",beta_prior,ynum01)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta01,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))
# 3.255418===3.255228
beta1=mapply("+",beta_prior,ynum1)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta1,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))
## 3.401665==3.401631 4.200243
beta2=mapply("+",beta_prior,ynum2)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta2,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))
## 3.057062==3.057058
beta3=mapply("+",beta_prior,ynum3)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta3,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))
# 1.727138
beta4=mapply("+",beta_prior,ynum4)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta4,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))
# 1.966955


#############################
### (3) sensitivity check
## ------------ pn=5
ess=10
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
beta_prior <-list()
pn=5
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)
A4b5=list.load("A4b5.rds")
lam5 <- apply(A4b5$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lam5[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A4b5$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,A4b5$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)
# mean paths error
rightpath5=apply(A4b5$Paths,2,function(x){sum(x==Df)/3587})
mean(rightpath5) ## 0.8370716
# situational error
edge5<-f.v.e
for(i in 1:10001){
  for(j in 1:3587){
    edge5 <- mapply("+",edge5,cted(A4b5$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
  }
}
alpha5=mapply("+",edge5,alpha_prior)
sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha5,function(x) x/sum(x))),
                         function(y){y^2}),sum),sqrt)) ## 0.6696485
add(10)+0.6696485 ### 1.739657
# emission error
ynum5<-Ny.rm0
for(i in 1:10001){
  for(j in 1:3587){
    ys5 <- pa.rm0[[A4b5$Paths[j,i]]]
    ind5 <- ch.rm0[ys5]
    va5<-obs2[j,ind5]
    for(k in 1:length(ys5)){
      ynum5[[ys5[k]]][va5[k]] <- ynum5[[ys5[k]]][va5[k]]+1
    }
  }
}
beta5=mapply("+",beta_prior,ynum5)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta5,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))

## 2.171854
m.5.lambda <- sapply(1:10,function(x){mean(apply(A4b5$Paths[,1:10001],2,function(y){length(which(y==x))}))})
sd.5.lambda<- sapply(1:10,function(x){sd(apply(A4b5$Paths[,1:10001],2,function(y){length(which(y==x))}))})


############################################################
### use plain tree,do a model selection in the end #########
############################################################
f.v.e <- list()
f.v.e[[1]] <-c(0,0)
f.v.e[[2]] <-c(0,0,0,0)
f.v.e[[3]] <-c(0,0)
f.v.e[[4]] <-c(0,0)
f.v.e[[5]] <-c(0,0)
f.v.e[[6]] <-c(0,0)
f.v.e[[7]] <-c(0,0)

path.f.v.e <- list()
path.f.v.e[[1]] <- matrix(c(1,2,4,1,1,1),ncol=2)
path.f.v.e[[2]] <- matrix(c(1,2,4,1,1,2),ncol=2)
path.f.v.e[[3]] <- matrix(c(1,2,5,1,2,1),ncol=2)
path.f.v.e[[4]] <- matrix(c(1,2,5,1,2,2),ncol=2)
path.f.v.e[[5]] <- matrix(c(1,2,6,1,3,1),ncol=2)
path.f.v.e[[6]] <- matrix(c(1,2,6,1,3,2),ncol=2)
path.f.v.e[[7]] <- matrix(c(1,2,7,1,4,1),ncol=2)
path.f.v.e[[8]] <- matrix(c(1,2,7,1,4,2),ncol=2)
path.f.v.e[[9]] <- matrix(c(1,3,2,1),ncol=2)
path.f.v.e[[10]] <- matrix(c(1,3,2,2),ncol=2)

ess <- 10
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha_prior[[7]] <- ess*(1/16)*c(1,1)

# ------- pn=1
beta_prior <-list()
pn=1
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)

B0<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(B0,"B0.rds")

lamb0 <- apply(B0$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:8000,lamb0[1:8000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)

est.path <- c()
for(i in 1:3587){
  cts = rep(0,length(pathpool[[i]]))
  for(j in 1:length(pathpool[[i]])){
    cts[j]=sum(B0$Paths[i,1:8000]==pathpool[[i]][j])
  }
  est.path[i]=pathpool[[i]][which.max(cts)]
}
ess=10
a <- ess * c(1/2,1/4,1/4,1/16,1/8,1/16,1/8,rep(1/32,8))
alphaprior <- list()
edgenum <- c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2)
for(i in 1:15){
  alphaprior[[i]] = rep(a[i],edgenum[i])
}
ct.Df <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(Df),paths=est.path,vertex=tlpos,edge=tled)
alphapostf <- alphaprior 
for(i in 1:15){
  alphapostf[[i]] <- alphaprior[[i]] + ct.Df[[i]]}
## nodes to merge
to.merge <- list()
to.merge[[1]] <- c(1,2) + 1
to.merge[[2]] <- c(3,5) + 1
to.merge[[3]] <- c(4,6) + 1
to.merge[[4]] <- c(7,8,11,12) + 1
to.merge[[5]] <- c(9,13) + 1
to.merge[[6]] <- c(10,14) + 1
localscoresf = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresf[[i]][j] = llk_local(stage,alphaprior,alphapostf)
  }
}
modelf= ahc_search(alphaprior,alphapostf,to.merge,localscoresf) 
scoref = sum(unlist(modelf$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapostf[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapostf[[1]])) # -7163.873
estf=list()
estf[[1]] = alphapostf[[1]]/sum(alphapostf[[1]])
for(i in 1:length(to.merge)){
  estf[[i+1]] = list();
  for(j in 1:length(modelf$post[[i]])){
    estf[[i+1]][[j]] = modelf$post[[i]][[j]]/sum(modelf$post[[i]][[j]])
  }
}
gtp<-list()
gtp[[1]]<-c(0.7151,0.2849)
gtp[[2]]<-c(0.5201,0.195)/0.7151
gtp[[3]]<-c(0.1799,0.105)/0.2849
gtp[[4]]<-c(0.2128,0.2128,0.049,0.0455)/0.5201
gtp[[5]]<-c(0.0672,0.0672,0.021,0.0245)/0.1799
gtp[[6]]<-c(0.6,0.4)
gtp[[7]]<-c(0.64,0.12)/0.76
gtp[[8]]<-c(2/3,1/3)
gtp[[9]]<-c(0.6,0.4)
gtp[[10]]<-c(0.5,0.5)





err1<-sqrt(sum((estf[[1]]-gtp[[1]])^2))+
  sqrt(sum((estf[[2]][[1]]-gtp[[2]])^2))+
  sqrt(sum((estf[[2]][[2]]-gtp[[3]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[4]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[5]])^2))+
  sqrt(sum((estf[[4]][[1]]-gtp[[6]])^2))+
  sqrt(sum((estf[[5]][[1]]-gtp[[7]])^2))+
  sqrt(sum((estf[[5]][[2]]-gtp[[8]])^2))+
  sqrt(sum((estf[[6]][[1]]-gtp[[9]])^2))+
  sqrt(sum((estf[[7]][[1]]-gtp[[10]])^2)) # 1.110365



##################################################  
ess <- 5
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha_prior[[7]] <- ess*(1/16)*c(1,1)

B1<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(B1,"B1.rds")
lamb0 <- apply(B1$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lamb0[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
est.path <- c()
for(i in 1:3587){
  cts = rep(0,length(pathpool[[i]]))
  for(j in 1:length(pathpool[[i]])){
    cts[j]=sum(B1$Paths[i,]==pathpool[[i]][j])
  }
  est.path[i]=pathpool[[i]][which.max(cts)]
}
ess=5
a <- ess * c(1/2,1/4,1/4,1/16,1/8,1/16,1/8,rep(1/32,8))
alphaprior <- list()
edgenum <- c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2)
for(i in 1:15){
  alphaprior[[i]] = rep(a[i],edgenum[i])
}
ct.Df <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(Df),paths=est.path,vertex=tlpos,edge=tled)
alphapostf <- alphaprior 
for(i in 1:15){
  alphapostf[[i]] <- alphaprior[[i]] + ct.Df[[i]]}
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
err1<-sqrt(sum((estf[[1]]-gtp[[1]])^2))+
  sqrt(sum((estf[[2]][[1]]-gtp[[2]])^2))+
  sqrt(sum((estf[[2]][[1]]-gtp[[3]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[4]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[5]])^2))+
  sqrt(sum((estf[[4]][[1]]-gtp[[6]])^2))+
  sqrt(sum((estf[[5]][[1]]-gtp[[7]])^2))+
  sqrt(sum((estf[[5]][[2]]-gtp[[8]])^2))+
  sqrt(sum((estf[[5]][[3]]-gtp[[8]])^2))+
  sqrt(sum((estf[[6]][[1]]-gtp[[9]])^2))+
  sqrt(sum((estf[[7]][[1]]-gtp[[10]])^2)) # 1.547522

##############################################

ess <- 1
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha_prior[[7]] <- ess*(1/16)*c(1,1)

B2<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(B2,"B2.rds")

lamb0 <- apply(B2$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lamb0[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
est.path <- c()
for(i in 1:3587){
  cts = rep(0,length(pathpool[[i]]))
  for(j in 1:length(pathpool[[i]])){
    cts[j]=sum(B2$Paths[i,]==pathpool[[i]][j])
  }
  est.path[i]=pathpool[[i]][which.max(cts)]
}
ess=1
a <- ess * c(1/2,1/4,1/4,1/16,1/8,1/16,1/8,rep(1/32,8))
alphaprior <- list()
edgenum <- c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2)
for(i in 1:15){
  alphaprior[[i]] = rep(a[i],edgenum[i])
}
ct.Df <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(Df),paths=est.path,vertex=tlpos,edge=tled)
alphapostf <- alphaprior 
for(i in 1:15){
  alphapostf[[i]] <- alphaprior[[i]] + ct.Df[[i]]}
localscoresf = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresf[[i]][j] = llk_local(stage,alphaprior,alphapostf)
  }
}
modelf= ahc_search(alphaprior,alphapostf,to.merge,localscoresf) 
scoref = sum(unlist(modelf$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapostf[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapostf[[1]])) # -6206.506
estf=list()
estf[[1]] = alphapostf[[1]]/sum(alphapostf[[1]])
for(i in 1:length(to.merge)){
  estf[[i+1]] = list();
  for(j in 1:length(modelf$post[[i]])){
    estf[[i+1]][[j]] = modelf$post[[i]][[j]]/sum(modelf$post[[i]][[j]])
  }
}
err1<-sqrt(sum((estf[[1]]-gtp[[1]])^2))+
  sqrt(sum((estf[[2]][[1]]-gtp[[2]])^2))+
  sqrt(sum((estf[[2]][[1]]-gtp[[3]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[4]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[5]])^2))+
  sqrt(sum((estf[[4]][[1]]-gtp[[6]])^2))+
  sqrt(sum((estf[[5]][[1]]-gtp[[7]])^2))+
  sqrt(sum((estf[[5]][[2]]-gtp[[8]])^2))+
  sqrt(sum((estf[[5]][[3]]-gtp[[8]])^2))+
  sqrt(sum((estf[[6]][[1]]-gtp[[9]])^2))+
  sqrt(sum((estf[[7]][[1]]-gtp[[10]])^2)) # 1.550226

########################################
ess <- 10
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
alpha_prior[[7]] <- ess*(1/16)*c(1,1)

# ------- pn=1
beta_prior <-list()
pn=5
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)
B3<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(B3,"B3.rds")
lamb0 <- apply(B3$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lamb0[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
est.path <- c()
for(i in 1:3587){
  cts = rep(0,length(pathpool[[i]]))
  for(j in 1:length(pathpool[[i]])){
    cts[j]=sum(B3$Paths[i,]==pathpool[[i]][j])
  }
  est.path[i]=pathpool[[i]][which.max(cts)]
}
ess=10
a <- ess * c(1/2,1/4,1/4,1/16,1/8,1/16,1/8,rep(1/32,8))
alphaprior <- list()
edgenum <- c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2)
for(i in 1:15){
  alphaprior[[i]] = rep(a[i],edgenum[i])
}
ct.Df <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(Df),paths=est.path,vertex=tlpos,edge=tled)
alphapostf <- alphaprior 
for(i in 1:15){
  alphapostf[[i]] <- alphaprior[[i]] + ct.Df[[i]]}
localscoresf = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresf[[i]][j] = llk_local(stage,alphaprior,alphapostf)
  }
}
modelf= ahc_search(alphaprior,alphapostf,to.merge,localscoresf) 
scoref = sum(unlist(modelf$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapostf[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapostf[[1]])) # -6122.471
estf=list()
estf[[1]] = alphapostf[[1]]/sum(alphapostf[[1]])
for(i in 1:length(to.merge)){
  estf[[i+1]] = list();
  for(j in 1:length(modelf$post[[i]])){
    estf[[i+1]][[j]] = modelf$post[[i]][[j]]/sum(modelf$post[[i]][[j]])
  }
}
err1<-sqrt(sum((estf[[1]]-gtp[[1]])^2))+
  sqrt(sum((estf[[2]][[1]]-gtp[[2]])^2))+
  sqrt(sum((estf[[2]][[2]]-gtp[[3]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[4]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[5]])^2))+
  sqrt(sum((estf[[4]][[1]]-gtp[[6]])^2))+
  sqrt(sum((estf[[5]][[1]]-gtp[[7]])^2))+
  sqrt(sum((estf[[5]][[2]]-gtp[[8]])^2))+
  sqrt(sum((estf[[5]][[3]]-gtp[[8]])^2))+
  sqrt(sum((estf[[6]][[1]]-gtp[[9]])^2))+
  sqrt(sum((estf[[7]][[1]]-gtp[[10]])^2)) # 2.210657


# ------- pn=1
beta_prior <-list()
pn=10
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)
B4<-HCAgibbs(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
             Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
             alpha_prior,beta_prior)
list.save(B4,"B4.rds")

lamb0 <- apply(B4$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lamb0[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
est.path <- c()
for(i in 1:3587){
  cts = rep(0,length(pathpool[[i]]))
  for(j in 1:length(pathpool[[i]])){
    cts[j]=sum(B4$Paths[i,]==pathpool[[i]][j])
  }
  est.path[i]=pathpool[[i]][which.max(cts)]
}
ess=10
a <- ess * c(1/2,1/4,1/4,1/16,1/8,1/16,1/8,rep(1/32,8))
alphaprior <- list()
edgenum <- c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2)
for(i in 1:15){
  alphaprior[[i]] = rep(a[i],edgenum[i])
}
ct.Df <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(Df),paths=est.path,vertex=tlpos,edge=tled)
alphapostf <- alphaprior 
for(i in 1:15){
  alphapostf[[i]] <- alphaprior[[i]] + ct.Df[[i]]}
localscoresf = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresf[[i]][j] = llk_local(stage,alphaprior,alphapostf)
  }
}
modelf= ahc_search(alphaprior,alphapostf,to.merge,localscoresf) 
scoref = sum(unlist(modelf$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapostf[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapostf[[1]])) # -6122.471,-6046.736
estf=list()
estf[[1]] = alphapostf[[1]]/sum(alphapostf[[1]])
for(i in 1:length(to.merge)){
  estf[[i+1]] = list();
  for(j in 1:length(modelf$post[[i]])){
    estf[[i+1]][[j]] = modelf$post[[i]][[j]]/sum(modelf$post[[i]][[j]])
  }
}
err1<-sqrt(sum((estf[[1]]-gtp[[1]])^2))+
  sqrt(sum((estf[[2]][[1]]-gtp[[2]])^2))+
  sqrt(sum((estf[[2]][[2]]-gtp[[3]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[4]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[5]])^2))+
  sqrt(sum((estf[[4]][[1]]-gtp[[6]])^2))+
  sqrt(sum((estf[[5]][[1]]-gtp[[7]])^2))+
  sqrt(sum((estf[[5]][[2]]-gtp[[8]])^2))+
  sqrt(sum((estf[[5]][[3]]-gtp[[8]])^2))+
  sqrt(sum((estf[[6]][[1]]-gtp[[9]])^2))+
  sqrt(sum((estf[[7]][[1]]-gtp[[10]])^2)) # 2.210657,2.315217



#########################################################
######### time +  ground truth staged tree ##############
#########################################################
ess=10
alpha_prior <- list()
alpha_prior[[1]] <- ess*(1/4)* c(1,1)
alpha_prior[[2]] <- ess*(1/16)*c(1,1,1,1)
alpha_prior[[3]] <- ess*(1/4)*c(1,1)
alpha_prior[[4]] <- ess*(1/16)*c(1,1)
alpha_prior[[5]] <- ess*(1/16)*c(1,1)
alpha_prior[[6]] <- ess*(1/16)*c(1,1)
beta_prior <-list()
pn=10
beta_prior[[1]] <- pn*c(1,1)
beta_prior[[2]] <- pn*c(1,1)
beta_prior[[3]] <- pn*c(1,1)
beta_prior[[4]] <- pn*c(1,1,1,1)
beta_prior[[5]] <- pn*c(1,1,1/5,1/5,1/5,1/5)
beta_prior[[6]] <- pn*c(0.1,0.1,0.4,0.4,0.2,1)
beta_prior[[7]] <- pn*c(1,1,0.1,0.1,0.1)
beta_prior[[8]] <- pn*c(0.1,0.1,1,0.1,0.1)
beta_prior[[9]] <- pn*c(1,0.1,0.1,1,0.1)
beta_prior[[10]] <- pn*c(0.1,0.1,0.1,0.1,1)
beta_prior[[11]] <- pn*c(1,0.1)
beta_prior[[12]] <- pn*c(0.1,1)
weibull.shape <- rep(2,10)
zeta <- rep(2,10)
mu <- rep(5,10)
C0=HCAgibbs_time(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
                 Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
                 alpha_prior,beta_prior,
                 ingashape=zeta, ingascale=mu, weishape=weibull.shape,ft=idleftime/10)
#list.save(C0,"C0.rds")
C0=list.load("C0.rds")
lamb0 <- apply(C0$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lamb0[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,C0$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,C0$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)


edge1<-f.v.e
for(i in 1:10001){
  for(j in 1:3587){
    edge1 <- mapply("+",edge1,cted(C0$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
  }
}
alpha1=mapply("+",edge1,alpha_prior)
add(10)+sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha1,function(x) x/sum(x))),
                                 function(y){y^2}),sum),sqrt)) ## 4.214374
ynum1<-Ny.rm0
for(i in 1:10001){
  for(j in 1:3587){
    ys1 <- pa.rm0[[C0$Paths[j,i]]]
    ind1 <- ch.rm0[ys1]
    va1<-obs2[j,ind1]
    for(k in 1:length(ys1)){
      ynum1[[ys1[k]]][va1[k]] <- ynum1[[ys1[k]]][va1[k]]+1
    }
  }
}
beta1=mapply("+",beta_prior,ynum1)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta1,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))

## 3.346917
postmeantime <- function(ingashape,ingascale,weishape,ft){
  meanpostH = matrix(0,10,10001)
  for(i in 1:10){
    for(j in 1:10001){
      ingashape[i] = ingashape[i] + sum(C0$Paths[,j]==i)
      ingascale[i] = ingascale[i] + sum(ft[which(C0$Paths[,j]==i)]^{weishape[i]})
      meanweiscale = ingascale[i]/(ingashape[i]-1)
      meanweiscale= (meanweiscale)^{1/weishape[i]}
      print(meanweiscale)
      meanpostH[i,j]=weishape[i]*gamma(1+(1/meanweiscale))
    }
  }
  return(meanpostH)
}
empricalmeanH <- rep(0,10)
for(i in 1:10){
  empricalmeanH[i]=mean(idleftime[which(Df==i)])
}
meanpostH <- postmeantime(ingashape=zeta,ingascale=mu,weishape=weibull.shape,ft=idleftime/10)
sqrt(sum(apply(meanpostH,1,mean)-empricalmeanH)^2) ## 5.440352


zeta <- rep(3,10)
mu <- rep(10,10)
C1=HCAgibbs_time(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
                 Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
                 alpha_prior,beta_prior,
                 ingashape=zeta, ingascale=mu, weishape=weibull.shape,ft=idleftime/10)

lamb0 <- apply(C1$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lamb0[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,C1$eps.theta[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(delta,"(T)")),cex.axis=1,cex.lab=1.2)
plot(1:10000,C1$eps.phi[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(epsilon,"(G*,T)")),cex.axis=1,cex.lab=1.2)


edge1<-f.v.e
for(i in 1:10001){
  for(j in 1:3587){
    edge1 <- mapply("+",edge1,cted(C1$Paths[j,i],ct=f.v.e,path.ct=path.f.v.e),SIMPLIFY = FALSE)
  }
}
alpha1=mapply("+",edge1,alpha_prior)
add(10)+sum(sapply(lapply(lapply(mapply("-",causal.theta,lapply(alpha1,function(x) x/sum(x))),
                                 function(y){y^2}),sum),sqrt)) ## 4.060503
ynum1<-Ny.rm0
for(i in 1:10001){
  for(j in 1:3587){
    ys1 <- pa.rm0[[C1$Paths[j,i]]]
    ind1 <- ch.rm0[ys1]
    va1<-obs2[j,ind1]
    for(k in 1:length(ys1)){
      ynum1[[ys1[k]]][va1[k]] <- ynum1[[ys1[k]]][va1[k]]+1
    }
  }
}
beta1=mapply("+",beta_prior,ynum1)
sum(sapply(lapply(lapply(mapply("-",emis.true,
                                lapply(beta1,function(x){x/sum(x)})),function(y){y^2}),sum),sqrt))
## 3.31664
postmeantime <- function(ingashape,ingascale,weishape,ft){
  meanpostH = matrix(0,10,10001)
  for(i in 1:10){
    for(j in 1:10001){
      ingashape[i] = ingashape[i] + sum(C1$Paths[,j]==i)
      ingascale[i] = ingascale[i] + sum(ft[which(C1$Paths[,j]==i)]^{weishape[i]})
      meanweiscale = ingascale[i]/(ingashape[i]-1)
      meanweiscale= (meanweiscale)^{1/weishape[i]}
      print(meanweiscale)
      meanpostH[i,j]=weishape[i]*gamma(1+(1/meanweiscale))
    }
  }
  return(meanpostH)
}
empricalmeanH <- rep(0,10)
for(i in 1:10){
  empricalmeanH[i]=mean(idleftime[which(Df==i)])
}
meanpostH <- postmeantime(ingashape=zeta,ingascale=mu,weishape=weibull.shape,ft=idleftime/10)
sqrt(sum(apply(meanpostH,1,mean)-empricalmeanH)^2) ## 5.491043

## plain tree
weibull.shape <- rep(2,10)
zeta <- rep(2,10)
mu <- rep(5,10)
C2=HCAgibbs_time(M=length(Df),n.path=10,pathpool=pathpool,pathp=pathp,
                 Ny=Ny.rm0,pa=pa.rm0,ch=ch.rm0,O=obs2,Ne=f.v.e,path.Ne=path.f.v.e,iter=10000,
                 alpha_prior,beta_prior,
                 ingashape=zeta, ingascale=mu, weishape=weibull.shape,ft=idleftime/10)


lamb0 <- apply(C2$Paths,2,function(x){sum(x==Df)/length(Df)})
plot(1:10000,lamb0[1:10000],pch=19,cex=0.2,xlab="iterations",ylab=expression(paste(kappa,"(T)")),cex.axis=1,cex.lab=1.2)
est.path <- c()
for(i in 1:3587){
  cts = rep(0,length(pathpool[[i]]))
  for(j in 1:length(pathpool[[i]])){
    cts[j]=sum(C2$Paths[i,]==pathpool[[i]][j])
  }
  est.path[i]=pathpool[[i]][which.max(cts)]
}
ess=10
a <- ess * c(1/2,1/4,1/4,1/16,1/8,1/16,1/8,rep(1/32,8))
alphaprior <- list()
edgenum <- c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2)
for(i in 1:15){
  alphaprior[[i]] = rep(a[i],edgenum[i])
}
ct.Df <- counts.collector(nsit=15,nedge=c(2,2,2,4,2,4,2,2,2,2,2,2,2,2,2),n=length(Df),paths=est.path,vertex=tlpos,edge=tled)
alphapostf <- alphaprior 
for(i in 1:15){
  alphapostf[[i]] <- alphaprior[[i]] + ct.Df[[i]]}
localscoresf = to.merge
for(i in 1:length(to.merge)){
  for(j in 1:length(to.merge[[i]])){
    stage  = to.merge[[i]][j]
    localscoresf[[i]][j] = llk_local(stage,alphaprior,alphapostf)
  }
}
modelf= ahc_search(alphaprior,alphapostf,to.merge,localscoresf) 
scoref = sum(unlist(modelf$scores)) + lgamma(sum(alphaprior[[1]]))-lgamma(sum(alphapostf[[1]])) - 
  sum(lgamma(alphaprior[[1]]) - lgamma(alphapostf[[1]])) # -5274.743
estf=list()
estf[[1]] = alphapostf[[1]]/sum(alphapostf[[1]])
for(i in 1:length(to.merge)){
  estf[[i+1]] = list();
  for(j in 1:length(modelf$post[[i]])){
    estf[[i+1]][[j]] = modelf$post[[i]][[j]]/sum(modelf$post[[i]][[j]])
  }
}
err1<-sqrt(sum((estf[[1]]-gtp[[1]])^2))+
  sqrt(sum((estf[[2]][[1]]-gtp[[2]])^2))+
  sqrt(sum((estf[[2]][[2]]-gtp[[3]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[4]])^2))+
  sqrt(sum((estf[[3]][[1]]-gtp[[5]])^2))+
  sqrt(sum((estf[[4]][[1]]-gtp[[6]])^2))+
  sqrt(sum((estf[[4]][[2]]-gtp[[6]])^2))+
  sqrt(sum((estf[[5]][[1]]-gtp[[7]])^2))+
  sqrt(sum((estf[[5]][[2]]-gtp[[8]])^2))+
  sqrt(sum((estf[[5]][[3]]-gtp[[8]])^2))+
  sqrt(sum((estf[[6]][[1]]-gtp[[9]])^2))+
  sqrt(sum((estf[[6]][[2]]-gtp[[9]])^2))+
  sqrt(sum((estf[[7]][[1]]-gtp[[10]])^2))+
  sqrt(sum((estf[[7]][[2]]-gtp[[10]])^2))# 4.768701



