###########################
#### model search code ####
###########################

#### collect counts
counts.collector <- function(nsit,nedge,n,paths,vertex,edge){
  # nsit: number of situations
  # nedge: number of edges for each situation
  # n: size of dataset
  # paths: paths for each doc
  # vertex: vertices matrix of the tree
  # edge: edges matrix of the tree
  out <- vector("list",nsit)
  for(i in 1:nsit){
    out[[i]]=rep(0,nedge[i])
  }
  for(i in 1:n){
    p = paths[i]
    w = vertex[p,]+1
    w = w[is.na(w) == FALSE]
    e = edge[p,]
    e=e[is.na(e) == FALSE]
    for(j in 1:length(w)){
      out[[w[j]]][e[j]] = out[[w[j]]][e[j]] + 1
    }
  }
  return(out)
}


llk_local<-function(stage,alphaprior,alphapost){
  alpha_u_plus = sum(alphapost[[stage]])
  alpha_u = sum(alphaprior[[stage]])
  return(lgamma(alpha_u) - lgamma(alpha_u_plus) -
           sum(lgamma(alphaprior[[stage]]) - lgamma(alphapost[[stage]])))
}
llk_local_mer<-function(stagea,stageb,alphaprior,alphapost){
  newalpha = alphaprior[[stagea]] + alphaprior[[stageb]]
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


ahc_search<-function(alphaprior,alphapost,to.merge,localscores){
  #each entry of liststages is a list stages (merged) for each variable, start from the non-root
  liststages = to.merge
  listscores = localscores
  listposteriors =vector("list",length(to.merge))
  
  for(i in length(to.merge):1){ ## check from the leaf to root
    nowstages = as.list(liststages[[i]]) ## list of current stages
    nowscore = listscores[[i]] ## vector of current scores for every current stage
    nowprior = vector("list",length(nowstages)) ## vector of current prior for every current stage
    nowpost = vector("list",length(nowstages)) ## list of current posterior for every current stage
    for(j in 1:length(nowstages)){
      nowpost[[j]] = alphapost[[nowstages[[j]]]]
      nowprior[[j]] = alphaprior[[nowstages[[j]]]]
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
          prior1 = nowprior[[pairindex[1]]]
          prior2 = nowprior[[pairindex[2]]]
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
          nowprior[[pairA[1]]] = nowprior[[pairA[1]]] + nowprior[[pairA[2]]]
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

llk_weibull <- function(shape,invscale,invshape,time,path,npath){
  # time: time for each document
  # path: path index for each document
  # shape: shape parameter of weibull, beta
  # npath: total number of path
  # invscale: scale parameter of invgamma, mu
  # invshape: shape paramter of invgamma, zeta
  score=rep(0,npath)
  for(i in 1:npath){
    d = which(path == i)
    H = time[d]
    postzeta=invshape[i]+length(d)
    postmu=invscale[i]+ sum((H)^shape[i])
    score[i] =  invshape[i]*log(invscale[i])-postzeta*log(postmu)+ lgamma(postzeta)-lgamma(invshape[i])
  }
  return(sum(score))
}

