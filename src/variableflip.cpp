#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix variableflip(NumericMatrix te, int i, int j) {
  int nrow = te.nrow();
  int ncol = te.ncol();
  std::vector<int> indices;
  NumericMatrix xy_n (nrow,2);
  for (int k=0;k<nrow;k++){
    if((te(k,ncol-2)!=i) & (te(k,ncol-2)!=j)) {xy_n(k,0) = te(k,ncol-2);}
    if(te(k,ncol-2)==i) {xy_n(k,0) = j;}
    if(te(k,ncol-2)==j) {xy_n(k,0) = i;}
    if((te(k,ncol-1)!=i) & (te(k,ncol-1)!=j))  {xy_n(k,1) = te(k,ncol-1);}
    if(te(k,ncol-1)==i) {xy_n(k,1) = j;}
    if(te(k,ncol-1)==j) {xy_n(k,1) = i;}
    if(xy_n(k,0)>xy_n(k,1)){
      int zv = xy_n(k,1);
      xy_n(k,1) = xy_n(k,0);
      xy_n(k,0) = zv;
    }
  }
  return xy_n;
}

