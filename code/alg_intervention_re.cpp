#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
Rcpp::List theta_est(List stages,int num_stages,List alpha_stage,List n_eID,int num_pos,int num_ID,IntegerVector num_pos_edge){
  List theta(num_pos);
  for(int w=0;w<num_pos;w++){
    NumericMatrix o(num_ID,num_pos_edge(w));
    theta[w] = o;
  }
  for(int m=0;m<num_ID;m++){
    for(int u=0;u<num_stages;u++){
      IntegerVector positions = stages[u];
      NumericVector countsofe(num_pos_edge(positions(0)));
      for(int w=0;w<positions.size();w++){
        int p = positions(w);
        countsofe +=  Rcpp::as<NumericMatrix>(n_eID[p])(m,_);
      }
      NumericVector t = countsofe + Rcpp::as<NumericVector>(alpha_stage[u]);
      for(int w=0;w<positions.size();w++){
        int p = positions(w);
        Rcpp::as<NumericMatrix>(theta[p])(m,_) = t/sum(t);
      }
    }
  }
  return(theta);
}

//[[Rcpp::export]]
Rcpp::NumericMatrix rho_est(NumericVector psi,NumericMatrix n_I,int num_path,int length_I){
  NumericMatrix out(num_path,length_I);
  for(int l=0;l<num_path;l++){
    NumericVector JJ = n_I(l,_) + psi;
    out(l,_) = JJ/sum(JJ);
  }
  return(out);
}

//[[Rcpp::export]]
Rcpp::List s_est(NumericVector sigma,List n_xr,int num_path,int length_I,int num_xr){
  List out(num_path);
  for(int i=0;i<num_path;i++){
    NumericMatrix l(length_I,num_xr);
    out[i] = l;
    for(int j=0;j<length_I;j++){
      NumericVector JJ=Rcpp::as<NumericMatrix>(n_xr[i])(j,_) + sigma;
      Rcpp::as<NumericMatrix>(out[i])(j,_) = JJ/sum(JJ);
    }
  }
  return(out);
}

//[[Rcpp::export]]
double theta_diff(List theta1,List theta2, int num_pos,int num_ID){
  double out = 0;
  for(int w=0;w<num_pos;w++){
    for(int m=0;m<num_ID;m++){
      out = out + sum(pow(Rcpp::as<NumericMatrix>(theta1[w])(m,_) - Rcpp::as<NumericMatrix>(theta2[w])(m,_),2));
    }
  }
  return(sqrt(out)); 
}

//[[Rcpp::export]]
double phi_diff(List phi1,List phi2, int num_paH,IntegerVector num_paH_va){
  double out = 0;
  for(int w=0;w<num_paH;w++){
    for(int j=0;j<num_paH_va(w);j++){
      out = out + sum(pow(Rcpp::as<NumericVector>(Rcpp::as<List>(phi1[w])[j]) - Rcpp::as<NumericVector>(Rcpp::as<List>(phi2[w])[j]),2));
    }
  }
  return(sqrt(out));
}

//[[Rcpp::export]]
double rho_diff(NumericMatrix rho1,NumericMatrix rho2,int num_path){
  double out = 0;
  for(int r=0;r<num_path;r++){
    out = out + sum(pow(rho1(r,_)-rho2(r,_),2));
  }
  return(sqrt(out));
}

//[[Rcpp::export]]
double s_diff(List s1,List s2,int num_path,int length_I){
  double out = 0;
  for(int w=0;w<num_path;w++){
    for(int j=0;j<length_I;j++){ 
      out = out + sum(pow(Rcpp::as<NumericMatrix>(s1[w])(j,_) - Rcpp::as<NumericMatrix>(s2[w])(j,_),2));
    }
  }
  return(sqrt(out));
}
//[[Rcpp::export]]
Rcpp::List phi_est(List beta, List n_pa,int num_paH,IntegerVector num_paH_va,IntegerVector num_ch_va){
  List phi(num_paH);
  for(int i=0;i<num_paH;i++){
    NumericMatrix m0(num_paH_va(i),num_ch_va(i));
    phi[i] = m0;
    for(int j=0;j<num_paH_va(i);j++){
      NumericVector r = Rcpp::as<NumericMatrix>(n_pa[i])(j,_) + Rcpp::as<NumericMatrix>(beta[i])(j,_);
      Rcpp::as<NumericMatrix>(phi[i])(j,_) = r/(sum(r));
    }
  }
  return(phi);
}

//[[Rcpp::export]]
NumericVector eta_est(NumericVector zeta,NumericVector g,NumericVector ftime){
  NumericVector eta = (zeta + g(0))/(ftime + g(1));
  return(eta);
}


//[[Rcpp::export]]

Rcpp::List ga_est(List n_A, List r, int A, List rangeA,IntegerVector Y_A,IntegerVector Y_num_states){
  List ga(A);
  for(int i=0; i<A; i++){
    IntegerVector con = rangeA[i];
    int dim2 = con.size();
    int p = Y_A(i);
    int dim1 = Y_num_states(p-1);
    NumericMatrix inst(dim1,dim2);
    ga[i] = inst;
    for(int j=0; j<dim1; j++){
      NumericVector h = Rcpp::as<NumericMatrix>(n_A[i])(j,_)+ Rcpp::as<NumericVector>(r[i]);
      Rcpp::as<NumericMatrix>(ga[i])(j,_) = h/(sum(h));
    }
  }
  return(ga);
}

//[[Rcpp::export]]
Rcpp::List HCAGIBBS_intvt(Rcpp::List edgeIDcounts,IntegerVector pathdoc, Rcpp::List indY,
                          Rcpp::List Acounts,Rcpp::List Adoc, Rcpp::List indpa,Rcpp::List pacounts,
                          NumericMatrix Icounts, List xrcounts, IntegerVector indI,// initialisation
                          int M, IntegerMatrix D, int num_o, IntegerVector num_ch_va,Rcpp::List states, NumericVector ftime,// observations
                          int num_path,int num_pos,IntegerVector num_pos_edge,Rcpp::List path_pos,
                          Rcpp::List path_edge, IntegerMatrix path_y, Rcpp::List y_edge, // candidate CEG setup
                          int num_paH, IntegerVector num_paH_va,
                          Rcpp::List pa_gn_ind,IntegerVector dsup_ind,IntegerVector child_ind, Rcpp::List pa_child_ind,
                          Rcpp::List state_parents,Rcpp::List path_miss_f, Rcpp::List path_nm_f,
                          Rcpp::List path_miss_ok, Rcpp::List path_nm_ok, IntegerVector findicator, // flattening setups
                          int first,int A,Rcpp::List rangeA,IntegerVector miss_A,IntegerVector Y_A,IntegerVector Y_num_states,Rcpp::List Y_states, // cuts setups
                          List stages,int num_stages,List alpha_stage, Rcpp::List beta, Rcpp::List r, NumericVector zeta, NumericVector g,// hyperparameters
                          int iteration,IntegerVector ID, IntegerVector intervention, IntegerVector remedy,
                          int length_I,int num_xr,int num_ID,NumericVector sigma,NumericVector psi
){
  // converge measurement
  NumericVector epsilon(iteration);
  NumericVector boundary(iteration);
  NumericVector boundrho(iteration);
  NumericVector bounds(iteration); 
  // Parameters initialisation (needs update):
  NumericVector eta(M); 
  NumericVector time_d(num_path); // time density for each path 
  List theta(num_pos); // transition probability
  List phi(num_paH); // emission probability 
  NumericMatrix rho(num_path,length_I);
  List s(num_path); // emission of remedy probability
  List n_eID(num_pos);
  List n_pa(num_paH);
  NumericMatrix n_I(num_path,length_I);
  List n_xr(num_path);
  for(int i=0;i<num_path;i++){
    for(int j=0;j<length_I;j++){
      n_I(i,j) = Icounts(i,j);
    }
  }
  for(int i=0;i<num_path;i++){
    NumericMatrix t(length_I,num_xr);
    n_xr[i] = t;
    s[i] = t;
    for(int j=0;j<length_I;j++){
      for(int k=0;k<num_xr;k++){
        Rcpp::as<NumericMatrix>(n_xr[i])(j,k) = Rcpp::as<NumericMatrix>(xrcounts[i])(j,k);
      }
    }
  }
  for(int i=0;i<num_pos;i++){
    NumericMatrix t0(num_ID,num_pos_edge(i));
    theta[i] = t0;
    n_eID[i] = t0;
    for(int j=0;j<num_ID;j++){
      for(int k=0;k<num_pos_edge(i);k++){
        Rcpp::as<NumericMatrix>(n_eID[i])(j,k) =Rcpp::as<NumericMatrix>(edgeIDcounts[i])(j,k);
      }
    }
  }
  for(int i=0;i<num_paH;i++){
    NumericMatrix m0(num_paH_va(i),num_ch_va(i));
    phi[i] = m0;
    n_pa[i] = m0;
    n_pa[i] = Rcpp::as<NumericMatrix>(pacounts[i]);
  }
  List n_A(A);
  for (int i=0; i<A; i++) {
    int dim1 = Y_num_states(Y_A(i)-1);
    int dim2 = Rcpp::as<IntegerVector>(rangeA[i]).size();
    NumericMatrix m1(dim1,dim2);
    n_A[i] = m1;
    for(int l=0;l<dim1;l++){
      Rcpp::as<NumericMatrix>(n_A[i])(l,_) = Rcpp::as<NumericMatrix>(Acounts[i])(l,_);
    }
  }
  IntegerVector lambda_M(M);
  List A_M(M);
  for(int i=0;i<M;i++){
    lambda_M(i) = pathdoc(i);
    IntegerVector m0(A);
    A_M[i] = m0;
    for(int j=0;j<A;j++){
      Rcpp::as<IntegerVector>(A_M[i])(j) = Rcpp::as<IntegerVector>(Adoc[i])(j);
    }
  }
  
  List PAind(M);
  List Yind(M);
  IntegerVector Iind(M);
  for(int i=0;i<M;i++){
    Iind(i) = indI(i);
    IntegerVector v(A);
    Yind[i] = v;
    for(int j=0;j<A;j++){
      Rcpp::as<IntegerVector>(Yind[i])(j) = Rcpp::as<IntegerVector>(indY[i])(j);
    }
    IntegerMatrix mt(num_o,3);
    PAind[i] = mt;
    for(int j=0;j<num_o;j++){
      Rcpp::as<IntegerMatrix>(PAind[i])(j,_) = Rcpp::as<IntegerMatrix>(indpa[i])(j,_);
    }
  }
  
  Function q("any");
  Function G("sample");
  Function B("beta");
  Function T("all");
  Function f("which");
  Function Q("row.match");
  Function I("intersect");
  Function pr("print");
  
  IntegerVector ind0 = Rcpp::Range(1,A);
  IntegerVector fullcut(A+2);
  IntegerVector pathpool;
  IntegerVector pool;
  NumericVector zeta_M(M);
  double g0 = g(0);
  double g1 = g(1);
  List oldtheta = theta_est(stages,num_stages,alpha_stage,n_eID,num_pos,num_ID,num_pos_edge); 
  List oldphi = phi_est(beta, n_pa, num_paH,num_paH_va,num_ch_va);
  NumericMatrix oldrho = rho_est(psi,n_I,num_path,length_I);
  List olds = s_est(sigma,n_xr,num_path,length_I,num_xr);
  for(int tau=0;tau< iteration; tau++){
    
    //blockedGibbs
    
    for(int m=0; m < M; m++){
      int machineID = ID(m) - 1; // for subsetting
      int fail = findicator(m);
      // compute the statistics -m
      
      int lambda = lambda_M(m);
      IntegerVector positions = path_pos[lambda];
      IntegerVector edges = path_edge[lambda];
      for(int k=0; k < edges.size(); k++){
        Rcpp::as<NumericMatrix>(n_eID[positions(k) - 1])(machineID,edges(k) - 1) -= 1;
      }
      IntegerVector Yhold = Yind[m];
      IntegerVector cuts = A_M[m];
      int Ihold = Iind(m);
      
      for(int j=0;j<A;j++){
        IntegerVector con = rangeA[j];
        IntegerVector place = f(con - 1 == cuts(j));
        Rcpp::as<NumericMatrix>(n_A[j])(Yhold(j),place(0) - 1) -= 1;
      }
      IntegerMatrix indices = PAind[m];
      for(int j=0;j<num_o;j++){
        Rcpp::as<NumericMatrix>(n_pa[indices(j,0)])(indices(j,1),indices(j,2)) -= 1;
      }
      if(intervention(m) == 2){
        n_I(lambda,Ihold-1) -= 1; 
        Rcpp::as<NumericMatrix>(n_xr[lambda])(Ihold-1,remedy(m)-1) -= 1; 
        //pr(m);
        //pr(n_I(lambda,Ihold-1));
        //pr( Rcpp::as<NumericMatrix>(n_xr[lambda])(Ihold-1,remedy(m)-1));
      }
      
      // resample a and update n_A and A_M
      IntegerVector KA(A);
      for(int i=0;i<A;i++){
        NumericVector n_Ayi = Rcpp::as<NumericMatrix>(n_A[i])(Yhold(i),_);
        NumericVector r_i = r[i];
        NumericVector cut_p = (n_Ayi + r_i)/sum(n_Ayi + r_i);
        IntegerVector candidate_A = rangeA[i];
        IntegerVector index_A = Rcpp::Range(0,candidate_A.size()-1);
        int a = Rcpp::as<IntegerVector>(G(index_A,1,false,cut_p))(0);
        cuts(i) = candidate_A(a) - 1;
        KA(i) = a;
      }
      A_M[m] = cuts;
      // update theta
      theta = theta_est(stages,num_stages,alpha_stage,n_eID,num_pos,num_ID,num_pos_edge);
      // update phi
      phi = phi_est(beta, n_pa, num_paH,num_paH_va,num_ch_va);
      // time density
      double t = ftime(m);
      for(int i=0;i<num_path;i++){
        time_d(i) = pow(t,zeta(i)-1)/(pow(t+g1,zeta(i)+g0) * Rcpp::as<NumericVector>(B(zeta(i),g0))(0));
      }
      
      // update rho
      rho = rho_est(psi,n_I,num_path,length_I);
      
      // update s
      s = s_est(sigma,n_xr,num_path,length_I,num_xr);
      
      //pathpool given a:
      IntegerVector doc = D(m,_);
      fullcut(0) = 0;
      fullcut(A+1) = num_o;
      fullcut[ind0] = cuts;
      for(int j=0; j < miss_A.size(); j++){ //j  = 0,1
        IntegerVector idx = Rcpp::Range(fullcut(miss_A(j)-1),fullcut(miss_A(j))-1);
        IntegerVector mblock = doc[idx];
        LogicalVector t = T(unique(mblock) == 2);
        if(t(0) == true){
          if(fail == 1){
            pool = Rcpp::as<IntegerVector>(path_miss_f[j]);
          }else{
            pool = Rcpp::as<IntegerVector>(path_miss_ok[j]);
          }
        }else{
          if(fail == 1){
            pool = Rcpp::as<IntegerVector>(path_nm_f[j]);
          }else{
            pool = Rcpp::as<IntegerVector>(path_nm_ok[j]);
          }
        }
        if(j == 0){
          pathpool = pool;
        }else{
          pathpool = I(pathpool,pool);
        }
      }
      
      // update pathpool probabilities
      IntegerVector entries(num_o);
      List rows(num_path);
      IntegerVector cols(num_o);
      List pa_va(num_o);
      IntegerVector dsupall(num_o); // direct superior index
      IntegerVector pa; //  parents on GN index
      IntegerVector dsup_va;// dsup value
      IntegerVector pah_va;
      int dsup;
      for(int k=0; k < A + 1; k++){
        IntegerVector loc = Rcpp::Range(fullcut(k),fullcut(k+1)-1);
        dsupall[loc] = k + 1;
      }
      for(int k=0; k < num_o; k++){
        dsup = dsupall(k);
        IntegerVector loc = Rcpp::as<IntegerVector>(f(child_ind == k + 1)) - 1;
        int entry;
        if(loc.size() == 1){
          entry = loc(0);
        }else{
          IntegerVector temp = f(Rcpp::as<IntegerVector>(dsup_ind[loc]) == dsup); 
          entry = loc(temp(0)-1);
        }
        entries(k) = entry;
        cols(k) = Rcpp::as<IntegerVector>(f(Rcpp::as<IntegerVector>(states[k]) == doc(k)))(0) - 1; 
        if(k > first){
          pa = pa_gn_ind[entry];
          pa = doc[pa - 1];
          pa_va[k] = pa;
        }
      }
      
      NumericVector path_probs(pathpool.size());
      for(int th=0;th<pathpool.size();th++){
        int j = pathpool(th) - 1;
        // path index is j
        IntegerVector m1(num_o);
        rows[j] = m1;
        // transition probabilities along path j:
        IntegerVector positions_j = path_pos[j];
        IntegerVector edges_j = path_edge[j];
        IntegerVector p = path_y(j,_);
        double transit_prob = 1;
        for(int l=0;l < edges_j.size();l++){
          int w_l = Rcpp::as<IntegerVector>(path_pos[j])(l) - 1;
          int e_l = Rcpp::as<IntegerVector>(path_edge[j])(l) - 1;
          transit_prob = transit_prob * Rcpp::as<NumericMatrix>(theta[w_l])(machineID,e_l);
        }
        
        // emission probabilities along path j:
        double emiss_probs=1;
        for(int k=0; k < num_o; k++){
          dsup_va = p[Rcpp::as<IntegerVector>(y_edge[dsupall(k) - 1]) - 1];
          if(k > first){
            int len = dsup_va.size() + Rcpp::as<IntegerVector>(pa_va[k]).size();
            IntegerVector rep(len);
            IntegerVector len1 = Rcpp::Range(0,dsup_va.size()-1);
            IntegerVector len2 = Rcpp::Range(dsup_va.size(),len -1);
            rep[len1] = dsup_va;
            rep[len2] = Rcpp::as<IntegerVector>(pa_va[k]);
            pah_va = rep;
          }else{
            pah_va = dsup_va;
          }
          int out = Rcpp::as<IntegerVector>(Q(pah_va, Rcpp::as<IntegerMatrix>(state_parents[entries(k)])))(0)-1;
          Rcpp::as<IntegerVector>(rows[j])(k) = out;
          emiss_probs = emiss_probs * Rcpp::as<NumericMatrix>(phi[entries(k)])(out,cols(k));
          
        }
        // intervention probability and emission of xr
        if(intervention(m) == 2){
          
          double interventionprob = rho(j,Ihold-1) * Rcpp::as<NumericMatrix>(s[j])(Ihold-1,remedy(m)-1);
          path_probs(th) = time_d(j) * transit_prob * emiss_probs * interventionprob;
          
        //  pr(time_d(j));
        //  pr(transit_prob);
        //  pr(emiss_probs);
         // pr(interventionprob);
        //  pr(path_probs(th));
        
        }else{
          path_probs(th) = time_d(j) * transit_prob * emiss_probs;
        }
      }
      
      // normalise the path probabilities
      path_probs = path_probs/(sum(path_probs));
      
      
      // resample a path
      IntegerVector hold = G(pathpool,1,false,path_probs);
      lambda = hold(0) - 1;
      
      // update lambda_M,n_eID,n_pa,PAind,Yind
      lambda_M(m) = lambda;
      
      for(int k=0;k<num_o;k++){
        Rcpp::as<IntegerMatrix>(PAind[m])(k,0) = entries(k);
        Rcpp::as<IntegerMatrix>(PAind[m])(k,1) = Rcpp::as<IntegerVector>(rows[lambda])(k);
        Rcpp::as<IntegerMatrix>(PAind[m])(k,2) = cols(k);
        Rcpp::as<NumericMatrix>(n_pa[entries(k)])(Rcpp::as<IntegerVector>(rows[lambda])(k),cols(k)) += 1;
      }
      IntegerVector p =path_y(lambda,_);
      
      positions = path_pos[lambda];
      edges = path_edge[lambda];
      for(int k=0; k < edges.size(); k++){
        Rcpp::as<NumericMatrix>(n_eID[positions(k) - 1])(machineID,edges(k) - 1) += 1;
      }
      
      for(int l=0;l<A;l++){
        dsup_va = p[Rcpp::as<IntegerVector>(y_edge[Y_A(l) - 1])-1];
        IntegerMatrix statemat = Y_states[Y_A(l)- 1]; 
        int L = Rcpp::as<IntegerVector>(Q(dsup_va, statemat))(0) - 1;
        Rcpp::as<IntegerVector>(Yind[m])(l) = L;
        Rcpp::as<NumericMatrix>(n_A[l])(L,KA(l)) += 1;
      }
      // resample the intervention indicator
      // update n_I and n_xr
      if(intervention(m) == 2){
        IntegerVector newI = G(length_I,1,false,rho(lambda,_));
        n_I(lambda,newI(0)-1) += 1;
        Rcpp::as<NumericMatrix>(n_xr[lambda])(newI(0)-1,remedy(m)-1) += 1;
        Iind(m) = newI(0);
      }
      
    }
    theta = theta_est(stages,num_stages,alpha_stage,n_eID,num_pos,num_ID,num_pos_edge);
    epsilon(tau) = theta_diff(theta,oldtheta,num_pos,num_ID);
    oldtheta = theta;
    phi = phi_est(beta,n_pa, num_paH,num_paH_va,num_ch_va);
    boundary(tau) = phi_diff(phi,oldphi,num_paH,num_paH_va);
    oldphi = phi;
    rho = rho_est(psi,n_I,num_path,length_I);
    boundrho(tau) = rho_diff(rho,oldrho,num_path);
    oldrho = rho;
    s = s_est(sigma,n_xr,num_path,length_I,num_xr);
    bounds(tau) = s_diff(s,olds,num_path,length_I);
    olds = s;
  }
  // estimate theta
  List theta_hat = theta_est(stages,num_stages,alpha_stage,n_eID,num_pos,num_ID,num_pos_edge);
  // estimate phi
  List phi_hat = phi_est(beta,n_pa, num_paH,num_paH_va,num_ch_va);
  // estimate eta
  zeta_M = zeta[lambda_M];
  NumericVector eta_hat = eta_est(zeta_M,g,ftime);
  // estimate gamma
  List ga_hat = ga_est(n_A, r, A, rangeA,Y_A,Y_num_states);
  // estimate rho
  NumericMatrix rho_hat = rho_est(psi,n_I,num_path,length_I);
  // estimate s
  List s_hat = s_est(sigma,n_xr,num_path,length_I,num_xr);
  // output the last round lambda_M,A_M,n_eID,n_A,PAind,Iind,n_I,n_xr
  
  return(Rcpp::List::create(Rcpp::Named("n_eID") = n_eID,
                            Rcpp::Named("Yind") = Yind,
                            Rcpp::Named("n_A") = n_A,
                            Rcpp::Named("path") = lambda_M,
                            Rcpp::Named("cuts") = A_M,
                            Rcpp::Named("n_pa") = n_pa,
                            Rcpp::Named("PAind") = PAind,
                            Rcpp::Named("Iind")=Iind,
                            Rcpp::Named("n_I")=n_I,
                            Rcpp::Named("n_xr")=n_xr,
                            Rcpp::Named("theta") = theta_hat,
                            Rcpp::Named("phi")=phi_hat,
                            Rcpp::Named("eta")=eta_hat,
                            Rcpp::Named("ga")=ga_hat,
                            Rcpp::Named("rho")=rho_hat,
                            Rcpp::Named("s")=s_hat,
                            Rcpp::Named("epsilon")=epsilon,
                            Rcpp::Named("boundary")=boundary,
                            Rcpp::Named("boundrho")=boundrho,
                            Rcpp::Named("bounds")=bounds));
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

