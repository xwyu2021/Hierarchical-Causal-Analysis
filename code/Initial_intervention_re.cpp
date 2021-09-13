#include <Rcpp.h>
using namespace Rcpp;


/* a function to initialise the initial counts */
// [[Rcpp::export]]
Rcpp::List initial_intvt(int M, // number of documents
                         int num_o,// number of core event variables
                         IntegerMatrix D, // trainCORE
                         int A, // number of cuts to choose
                         Rcpp::List rangeA, // sample index from rangeA as cuts
                         int num_path, // number of paths on the CEG
                         int num_pos, // number of positions excluding sink nodes
                         IntegerVector num_pos_edge, // number of edges for each position
                         Rcpp::List path_pos,
                         Rcpp::List path_edge, // give path, can identify the edges calong this path (which emanating edge of the vertex)
                         IntegerMatrix path_y, // give path, can identify the
                         IntegerVector miss_A, // which correspondent block is a missing indicator
                         Rcpp::List y_edge, // the index of edge corresponding to each y in the path.y matrix
                         Rcpp::List states, //  states of events
                         Rcpp::List state_parents,
                         Rcpp::List path_miss_f,
                         Rcpp::List path_nm_f,
                         Rcpp::List path_miss_ok,
                         Rcpp::List path_nm_ok,
                         IntegerVector findicator, // failure or not
                         IntegerVector Y_num_states,
                         IntegerVector Y_A,
                         Rcpp::List Y_states,
                         Rcpp::List pa_gn_ind,
                         IntegerVector dsup_ind,
                         IntegerVector child_ind,
                         Rcpp::List pa_child_ind,
                         int first,
                         int num_paH,
                         IntegerVector num_paH_va,
                         IntegerVector num_ch_va,
                         IntegerVector ID, // machine ID=trainID
                         IntegerVector intervention,//intervention type=trainINTVT
                         IntegerVector remedy,// action value=trainREMEDY
                         int length_I, // number of length of intervention indicator
                         int num_xr, // number of states of remedies
                         int num_ID //number of different types of machines
){
  // counts to output: n_e,n_y,pa(o),n_a,1(y,o),n_a,0(y,o),n_lambda
  // lambda_M: create a list (? or vector) of length M to store the path for each M
  // A_M:  a list of length M to store the cut position for each M
  // n_e: create a list  of length =numpos, each entry has length num_pos_edge
  // n_lambda: (? we have lambda_M do we still need this count?):a vector of length = total paths in the tree
  // n_A1: a list of length= total parents, each entry has a list of length states of the parents, each entry is a list of length child states
  // n_A0: constructed same as n_A1
  // initialise n_I (for each path lambda)
  // Iind: record the sampled I for each document
  // initialise n_xr (count for remedy for each comb of lambda and I)
  IntegerVector lambda_M(M);
  List A_M(M);
  List n_eID(num_pos);
  IntegerVector n_lambda(num_path);
  IntegerMatrix n_I(num_path,length_I);
  IntegerVector Iind(M);
  List n_xr(num_path);
  List Yind(M);
  List n_A(A);
  List n_pa(num_paH);
  List PAind(M);
  Function g("sample");
  Function f("which");
  //Function q("any");
  Function T("all");
  Function Q("row.match");
  Function I("intersect");
  // initialise n_eID now n_e is machine wise
  for(int i=0; i < num_pos; i++){
    IntegerMatrix l1(num_ID,num_pos_edge(i));
    n_eID[i] = l1;
  }
  for(int i=0; i < num_paH; i++){
    IntegerMatrix m1(num_paH_va[i],num_ch_va[i]);
    n_pa[i] = m1;
  }
  
  for(int i=0;i<A;i++){
    IntegerVector con = rangeA[i];
    int dim2 = con.size();
    int p = Y_A(i);
    int dim1 = Y_num_states(p-1);
    IntegerMatrix inst(dim1,dim2);
    n_A[i] = inst;
  }
  for(int i=0;i<M;i++){
    IntegerVector v(A);
    Yind[i] = v;
    IntegerMatrix mt(num_o,3);
    PAind[i] = mt;
  }
  for(int i=0;i<num_path;i++){
    IntegerMatrix s1(length_I,num_xr);
    n_xr[i] = s1;
  }
  
  // for each document
  for(int i=0; i < M; i++){
    int machineID = ID(i);
    int fail = findicator(i);
    IntegerVector doc = D(i,_);
    IntegerVector cuts(A+2);
    cuts(0) = 0;
    cuts(A+1) = num_o;
    int path;
    for(int j=0;j<A;j++){
      IntegerVector thispool = rangeA[j];
      IntegerVector hold = g(thispool,1,false);
      cuts(j+1) = hold(0) - 1;
    }
    IntegerVector ind0 = Rcpp::Range(1,A); // ind0=c(1,2)
    IntegerVector pathpool;
    IntegerVector pool;
    for(int j=0; j < miss_A.size(); j++){ //j  = 0,1
      int block = miss_A(j); //block = 1,3
      IntegerVector idx = Rcpp::Range(cuts(block-1),cuts(block)-1);
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
    IntegerVector here = g(pathpool,1,false);
    path = here(0) - 1;
    
    // update lambda_M
    lambda_M(i) = path;
    // update n_lambda
    n_lambda(path) += 1;
    // update n_eID
    IntegerVector nodes = path_pos[path];
    IntegerVector edges = path_edge[path];
    for(int k=0; k < edges.size(); k++){
      int ind = nodes(k) - 1;
      int ind2 = edges(k) - 1;
      Rcpp::as<IntegerMatrix>(n_eID[ind])(machineID-1,ind2) += 1;
    }
    // sample an intervention indicator if it is random remedy
    if(intervention(i) == 2){
      IntegerVector temp = g(length_I,1,false);
      Iind(i) = temp(0);  
      n_I(path,temp(0)-1) += 1;
      // then given xr, update the n_xr
      Rcpp::as<IntegerMatrix>(n_xr[path])(temp(0)-1,remedy(i)-1) += 1;
    }
    
    // update A_M, excluding the first and the last
    A_M[i] = cuts[ind0];
    // update n_A and yind
    IntegerVector p =path_y(path,_);
    for(int l=0;l<A;l++){
      int dsup = Y_A(l);
      IntegerVector va = y_edge[dsup - 1];
      IntegerVector dsup_va = p[va-1];
      IntegerMatrix statemat = Y_states[dsup - 1];
      IntegerVector out = Q(dsup_va, statemat);
      int row = out(0) - 1;
      int thiscut = cuts(l+1);
      IntegerVector con = rangeA[l];
      IntegerVector place = f(con - 1 == thiscut);
      int col = place(0) - 1;
      Rcpp::as<IntegerMatrix>(n_A[l])(row,col) += 1;
      Rcpp::as<IntegerVector>(Yind[i])(l) = row;
    }
    // update n_pa and PAind
    IntegerVector dsupall(num_o); // direct superior index
    IntegerVector pa; //  parents on GN index
    IntegerVector dsup_va;// dsup value
    IntegerVector pa_va;// gn par value
    IntegerVector pah_va;
    for(int k=0; k < A + 1; k++){
      IntegerVector loc = Rcpp::Range(cuts(k),cuts(k+1)-1);
      dsupall[loc] = k + 1;
    }
    int dsup;
    for(int k=0; k < num_o; k++){
      dsup = dsupall(k);
      IntegerVector va = y_edge[dsup - 1];
      IntegerVector p = path_y(path,_);
      dsup_va = p[va-1];
      IntegerVector b = f(child_ind == k + 1);
      IntegerVector loc = b - 1;
      int entry;
      if(loc.size() == 1){
        entry = loc(0);
      }else{
        IntegerVector can = dsup_ind[loc];
        IntegerVector temp = f(can == dsup);
        entry = loc(temp(0)-1);
      }
      if(k > first){
        pa = pa_gn_ind[entry];
        pa_va = doc[pa - 1];
        int len = dsup_va.size() + pa.size();
        IntegerVector rep(len);
        IntegerVector len1 = Rcpp::Range(0,dsup_va.size()-1);
        IntegerVector len2 = Rcpp::Range(dsup_va.size(),len -1);
        rep[len1] = dsup_va;
        rep[len2] = pa_va;
        pah_va = rep;
      }else{
        pah_va = dsup_va;
        
      }
      IntegerMatrix statemat = state_parents[entry];
      IntegerVector out = Q(pah_va, statemat);
      int row = out(0) - 1;
      IntegerVector com = states[k];
      IntegerVector a = f(com == D(i,k));
      int col = a(0) - 1;
      Rcpp::as<IntegerMatrix>(PAind[i])(k,_) = IntegerVector::create(entry,row,col);
      Rcpp::as<IntegerMatrix>(n_pa[entry])(row,col) += 1;
    }
    
  }
  return(Rcpp::List::create(Rcpp::Named("n_eID") = n_eID,
                            Rcpp::Named("n_lambda") = n_lambda,
                            Rcpp::Named("Yind") = Yind,
                            Rcpp::Named("n_A") = n_A,
                            Rcpp::Named("path") = lambda_M,
                            Rcpp::Named("cuts") = A_M,
                            Rcpp::Named("n_pa") = n_pa,
                            Rcpp::Named("PAind") = PAind,
                            Rcpp::Named("n_I") = n_I,
                            Rcpp::Named("Iind") = Iind,
                            Rcpp::Named("n_xr") = n_xr));
}
