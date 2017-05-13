#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double cmasum(NumericMatrix te) { // te enthaelt die scagnostics
  unsigned int nrow = te.nrow();
  unsigned int ncol = te.ncol();
  unsigned int lastv = ncol -2;
  double hsum = 0;
  double vsum = 0;
  double sum;
  NumericVector dif(lastv);
  unsigned int p = max(te(_,ncol-1)); // Vorsicht: in C: Indizierung ab 0 bis #-1
  std::vector<int> indices;
  
  for (unsigned int i = 0; i < p; i++){
    // Zeilennachbarn:
    if(i<(p-2)){
      for(unsigned int j=0; j<nrow;j++){
        if(te(j,ncol-2)==(i+1)){
          indices.push_back(j); // Fuegt j am Ende an
        }
      }
      for(unsigned int j=1; j<indices.size();j++){
        sum =0;
        for(unsigned int k=0; k < lastv; k++){ // quadrierte Differenzen 
          dif[k] = (te(indices[j],k)- te(indices[j-1],k))*(te(indices[j],k)- te(indices[j-1],k));
          sum += dif[k];
        }
        hsum = hsum + sqrt(sum); 
      }
      indices.clear();
    }
    // Spaltennachbarn:
    if(i>1){
      for(unsigned int j=0; j<nrow;j++){
        if(te(j,ncol-1)==(i+1)){
          indices.push_back(j); // Fuegt j am Ende an
        }
      }
      for(unsigned int j=1; j<indices.size();j++){
        sum =0;
        for(unsigned int k=0; k < lastv; k++){
          dif[k] = (te(indices[j],k)- te(indices[j-1],k))*(te(indices[j],k)- te(indices[j-1],k));
          sum += dif[k];
        }
        vsum = vsum + sqrt(sum);
      }
      indices.clear();
    }  
    
  } 
  return vsum + hsum;
}
  

