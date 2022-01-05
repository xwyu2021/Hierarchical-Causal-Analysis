########### initialisatioin ####################

## transform counts of edges
# f.v.e: counts table (all 0s)
# path.v.e: find v,e for each path

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
  
cted <- function(path.id,ct=f.v.e,path.ct=path.f.v.e){
  t <- path.ct[[path.id]]
  for(j in 1:nrow(t)){
    ct[[t[j,1]]][t[j,2]] <- ct[[t[j,1]]][t[j,2]] + 1
  }
  return(ct)
}
rm.cted <- function(path.id,ct=f.v.e,path.ct=path.f.v.e){
  t <- path.ct[[path.id]]
  for(j in 1:nrow(t)){
    ct[[t[j,1]]][t[j,2]] <- ct[[t[j,1]]][t[j,2]] - 1
  }
  return(ct)
}

# M: num of data cases
# n.path: number of path (for failures)
# W: number of positions
# E: number of maximum edges for paths
# n.y: number of y values
# f.path: failure paths
# pathpool: pathpool for each datacase
# pathp: pathpool probabilities // normalised
# Ny: counts of y with respect to core event value
# pa: y ind for each path
# ch: ch ind for each y
# O: simplified obs matrix



ini_p <- function(M,n.path,pathpool,pathp,Ny,pa,ch,O,Ne,path.Ne){ 
  # Ne: edge counts
  # Paths: path index
  Paths <- rep(0,M)
  for(i in 1:M){
    # (1) sample a path
    pool <- pathpool[[i]]
    pool.p <- pathp[[i]]
    Paths[i] <-sample(pool,1,prob = pool.p)
    # (2) add counts to Ne
    Ne <- cted(Paths[i],ct=Ne,path.ct=path.Ne)
    # (3) add counts to Ny
    ys <- pa[[Paths[i]]]
    ind <- ch[ys]
    va<-O[i,ind]
    for(j in 1:length(ys)){
      Ny[[ys[j]]][va[j]] <- Ny[[ys[j]]][va[j]]+1
    }
  }
  return(list(Ne=Ne,Ny=Ny,Paths=Paths))
} 

#### theta estimation
theta_est<-function(alpha_prior,counts){
  return(lapply(mapply("+",alpha_prior,counts),function(x){x/sum(x)}))
} 

#### theta difference
theta_diff<-function(theta1,theta2){ 
  return(sum(sapply(mapply("-",theta1,theta2),function(x){sqrt(sum(x^2))}))) 
} 

### phi est/ phi diff same as theta


#####################################
###### total learning model #########
#####################################
# npath: number of failure path
# iter: number of iterations
# alpha_prior: prior of transition probs
# beta_prior: prior of emiss probs
HCAgibbs <- function(M,n.path,pathpool,pathp,Ny,pa,ch,O,Ne,path.Ne,iter,
                     alpha_prior,beta_prior){
  # set convergence check
  eps.theta <-c()
  eps.phi <- c()
  # initialisation
  Q0<-ini_p(M=M,n.path=n.path,pathpool=pathpool,pathp=pathp,Ny=Ny,pa=pa,ch=ch,O=O,Ne=Ne,path.Ne=path.Ne)
  Ne <- Q0$Ne
  Ny <- Q0$Ny
  Paths <- matrix(0,ncol=iter+1,nrow=M)
  Paths[,1]<- Q0$Paths
  theta.old <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
  phi.old <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
  out <- matrix(0,nrow=iter,ncol=4)
  # blocked gibbs
  for(i in 1:iter){
    for(m in 1:M){
      
      # statistics -m
      path <- Paths[m,i]
      Ne <- rm.cted(path,ct=Ne,path.ct=path.Ne)
      ys <- pa[[path]]
      ind <- ch[ys]
      va<-O[m,ind]
      for(j in 1:length(ys)){
        Ny[[ys[j]]][va[j]] <- Ny[[ys[j]]][va[j]]-1
      }
      
      theta.rm.m <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
      phi.rm.m <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
      pi.path <-  pathprob_est(theta=theta.rm.m,path.ct=path.Ne,npath=n.path)
      # sample a path and update the path
      pool <- pathpool[[m]]
      pool.p <- pi.path[pool]
      for(l in 1:length(pool)){
        ll=pool[l]
        pool.p[l]=pool.p[l] * prod(mapply('[[',phi.rm.m[pa[[ll]]],O[m,ch[pa[[ll]]]]))
      }
      
      path <- sample(pool,1,prob=pool.p/sum(pool.p))
      Paths[m,i+1]=path
      # update Ne, Ny
      Ne <- cted(path,ct=Ne,path.ct=path.Ne)
      ys <- pa[[path]]
      ind <- ch[ys]
      va<-O[m,ind]
      for(j in 1:length(ys)){
        Ny[[ys[j]]][va[j]] <- Ny[[ys[j]]][va[j]] + 1
      } 
    }
    theta.new <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
    out[i,]=theta.new[[2]]
    phi.new <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
    eps.theta[i] <- theta_diff(theta.old,theta.new)
    eps.phi[i]<-theta_diff(phi.old,phi.new)
    theta.old <- theta.new
    phi.old <- phi.old
  }
  return(list(eps.phi=eps.phi,eps.theta=eps.theta,
              Ne=Ne,Ny=Ny,Paths=Paths,out=out))
}




HCAgibbs_input <- function(M,n.path,pathpool,pathp,Ny,pa,ch,O,Ne,path.Ne,iter,
                     alpha_prior,beta_prior,iniNe,iniNy,iniPaths){
  # set convergence check
  eps.theta <-c()
  eps.phi <- c()
  # initialisation
  Ne <- iniNe
  Ny <- iniNy
  Paths <- iniPaths
  theta.old <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
  phi.old <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
  out <- matrix(0,nrow=iter,ncol=4)
  # blocked gibbs
  for(i in 1:iter){
    for(m in 1:M){
      
      # statistics -m
      path <- Paths[m]
      Ne <- rm.cted(path,ct=Ne,path.ct=path.Ne)
      ys <- pa[[path]]
      ind <- ch[ys]
      va<-O[m,ind]
      for(j in 1:length(ys)){
        Ny[[ys[j]]][va[j]] <- Ny[[ys[j]]][va[j]]-1
      }
      theta.rm.m <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
      phi.rm.m <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
      pi.path <-  pathprob_est(theta=theta.rm.m,path.ct=path.Ne,npath=n.path)
      
      # sample a path and update the path
      pool <- pathpool[[m]]
      pool.p <- pi.path[pool]
      for(l in 1:length(pool)){
        ll=pool[l]
        pool.p[l]=pool.p[l] * prod(mapply('[[',phi.rm.m[pa[[ll]]],O[m,ch[pa[[ll]]]]))
      }
      path <- sample(pool,1,prob=pool.p/sum(pool.p))
      Paths[m]=path
      # update Ne, Ny
      Ne <- cted(path,ct=Ne,path.ct=path.Ne)
      ys <- pa[[path]]
      ind <- ch[ys]
      va<-O[m,ind]
      for(j in 1:length(ys)){
        Ny[[ys[j]]][va[j]] <- Ny[[ys[j]]][va[j]] + 1
      } 
    }
    theta.new <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
    out[i,]=theta.new[[2]]
    phi.new <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
    eps.theta[i] <- theta_diff(theta.old,theta.new)
    eps.phi[i]<-theta_diff(phi.old,phi.new)
    theta.old <- theta.new
    phi.old <- phi.old
  }
  return(list(eps.phi=eps.phi,eps.theta=eps.theta,
              Ne=Ne,Ny=Ny,Paths=Paths,out=out))
}

HCAmh_input <- function(M,n.path,pathpool,pathp,Ny,pa,ch,O,Ne,path.Ne,iter,
                           alpha_prior,beta_prior,iniNe,iniNy,iniPaths){
  # set convergence check
  eps.theta <-c()
  eps.phi <- c()
  # initialisation
  Ne <- iniNe
  Ny <- iniNy
  Paths <- iniPaths
  out <- matrix(0,nrow=iter,ncol=4)
  # blocked gibbs
  for(i in 1:iter){
    for(m in 1:M){
      oldpath <- Paths[m]
      theta.old<- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
      phi.old <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
      pi.path <-  pathprob_est(theta=theta.old,path.ct=path.Ne,npath=n.path)
      pool <- pathpool[[m]]
      pool.p.old <- pi.path[pool]
      for(l in 1:length(pool)){
        ll=pool[l]
        pool.p.old[l]=pool.p.old[l] * prod(mapply('[[',phi.old[pa[[ll]]],O[m,ch[pa[[ll]]]]))
      }
      old.prob <- pool.p.old/sum(pool.p.old)
      old.prob<-old.prob[which(pool == oldpath)]
      newpath <- sample(pool,1)
      Ne0 <- cted(newpath,ct=Ne,path.ct=path.Ne)
      ys <- pa[[newpath]]
      ind <- ch[ys]
      va<-O[m,ind]
      Ny0=Ny
      for(j in 1:length(ys)){
        Ny0[[ys[j]]][va[j]] <- Ny0[[ys[j]]][va[j]] + 1
      } 
      theta.new<- lapply(mapply("+",alpha_prior,Ne0),function(x){x/sum(x)})
      phi.new <- lapply(mapply("+",beta_prior,Ny0),function(x){x/sum(x)})
      pi.path <-  pathprob_est(theta=theta.new,path.ct=path.Ne,npath=n.path)
      pool.p.new <- pi.path[pool]
      for(l in 1:length(pool)){
        ll=pool[l]
        pool.p.new[l]=pool.p.new[l] * prod(mapply('[[',phi.new[pa[[ll]]],O[m,ch[pa[[ll]]]]))
      }
      new.prob <- pool.p.new/sum(pool.p.new)
      new.prob <- new.prob[which(pool == newpath)]
      
      if(runif(1,0,1)<(new.prob/old.prob)){
        Paths[m]=newpath
        Ny=Ny0
        Ne=Ne0
      }else{
        Paths[m]=oldpath
      }
    }
  }
  return(list(Paths=Paths))
}


time.p <- function(ingashape, ingascale, weishape, paths, ft,pathpool,doc){
  density<-pathpool[[doc]]
  for(i in 1:length(pathpool[[doc]])){
    lambda <-pathpool[[doc]][i]
    nlam <- sum(paths==lambda)
    upshape <- ingashape[lambda] + nlam
    upscale <- sum(ft[which(paths==lambda)]^(weishape[lambda])) + ingascale[lambda]
    density[i]<-exp(log(upshape)+log(weishape[lambda])+((weishape[lambda]-1)*log(ft[lambda]))+
                      (upshape-1)*log(upscale-ft[lambda])-(upshape*log(upscale)))
  }
  return(density/sum(density))
}

HCAgibbs_time <- function(M,n.path,pathpool,pathp,Ny,pa,ch,O,Ne,path.Ne,iter,
                     alpha_prior,beta_prior,
                     ingashape, ingascale, weishape,ft){
  # set convergence check
  eps.theta <-c()
  eps.phi <- c()
  # initialisation
  Q0<-ini_p(M=M,n.path=n.path,pathpool=pathpool,pathp=pathp,Ny=Ny,pa=pa,ch=ch,O=O,Ne=Ne,path.Ne=path.Ne)
  Ne <- Q0$Ne
  Ny <- Q0$Ny
  Paths <- matrix(0,ncol=iter+1,nrow=M)
  Paths[,1]<- Q0$Paths
  theta.old <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
  phi.old <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
  #out <- matrix(0,nrow=iter,ncol=4)
  # blocked gibbs
  for(i in 1:iter){
    for(m in 1:M){
      
      # statistics -m
      path <- Paths[m,i]
      Ne <- rm.cted(path,ct=Ne,path.ct=path.Ne)
      ys <- pa[[path]]
      ind <- ch[ys]
      va<-O[m,ind]
      for(j in 1:length(ys)){
        Ny[[ys[j]]][va[j]] <- Ny[[ys[j]]][va[j]]-1
      }
      theta.rm.m <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
      phi.rm.m <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
      pi.path <-  pathprob_est(theta=theta.rm.m,path.ct=path.Ne,npath=n.path)
      
      # sample a path and update the path
      pool <- pathpool[[m]]
      pool.p <- pi.path[pool]
      for(l in 1:length(pool)){
        ll=pool[l]
        pool.p[l]=pool.p[l] * prod(mapply('[[',phi.rm.m[pa[[ll]]],O[m,ch[pa[[ll]]]]))
      }
      hpath <- Paths[,i]
      density=time.p(ingashape=ingashape, ingascale=ingascale, weishape=weishape, paths=hpath,
             ft=ft,pathpool=pathpool,doc=m)
      
      path <- sample(pool,1,prob=pool.p*density/sum(pool.p*density))
      Paths[m,i+1]=path
      # update Ne, Ny
      Ne <- cted(path,ct=Ne,path.ct=path.Ne)
      ys <- pa[[path]]
      ind <- ch[ys]
      va<-O[m,ind]
      for(j in 1:length(ys)){
        Ny[[ys[j]]][va[j]] <- Ny[[ys[j]]][va[j]] + 1
      } 
    }
    theta.new <- lapply(mapply("+",alpha_prior,Ne),function(x){x/sum(x)})
    #out[i,]=theta.new[[2]]
    phi.new <- lapply(mapply("+",beta_prior,Ny),function(x){x/sum(x)})
    eps.theta[i] <- theta_diff(theta.old,theta.new)
    eps.phi[i]<-theta_diff(phi.old,phi.new)
    theta.old <- theta.new
    phi.old <- phi.old
  }
  return(list(eps.phi=eps.phi,eps.theta=eps.theta,Paths=Paths))
}



