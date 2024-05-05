#include <Rcpp.h>
using namespace Rcpp;

int sn;  int rerolls;  int holdn;  int thrown;  int dice;  int dn;
int ii1 = 0;  int n = 0;  int m = 0;  int reroll = 0;  int saved = 0;  int o = 0;  int rr = 0;  double p = 0;  int I3 = 0;

IntegerVector Ind;
IntegerVector Inds;
IntegerVector IndsInd;
IntegerVector HoldingInds;
NumericVector Chances;
IntegerVector Throwsn;
IntegerVector Scores;

//[[Rcpp::export]]
void Yahtzeeintcpp(int sn1, int rerolls1, int holdn1, int thrown1, int dice1){
  sn = sn1;  rerolls = rerolls1;  holdn = holdn1;  thrown = thrown1;  dice = dice1;
  n = 1;  for(int i = 0; i<sn; i++){ n *= 2; }
  m = 1 + rerolls*holdn;
  dn = 1;  for(int i = 0; i<dice; i++){ dn *= 2; };  dn --;
  Ind = IntegerVector(n-1);  Inds = IntegerVector(thrown*holdn);  IndsInd = IntegerVector(holdn);
  HoldingInds = IntegerVector(dn*thrown);  Chances = NumericVector(thrown*dice);
  Throwsn = IntegerVector(dice);  Scores = IntegerVector(sn*thrown);
}

//[[Rcpp::export]]
void YahtzeeIndcpp(int i, int val){ Ind[i] = val; }
//[[Rcpp::export]]
void YahtzeeIndscpp(int i, int val){ Inds[i] = val; }
//[[Rcpp::export]]
void YahtzeeIndsIndcpp(int i, int val){ IndsInd[i] = val; }
//[[Rcpp::export]]
void YahtzeeHoldingIndscpp(int i, int val){ HoldingInds[i] = val; }
//[[Rcpp::export]]
void YahtzeeChancescpp(int i, double val){ Chances[i] = val; }
//[[Rcpp::export]]
void YahtzeeThrowsncpp(int i, int val){ Throwsn[i] = val; }
//[[Rcpp::export]]
void YahtzeeScorescpp(int i, int val){ Scores[i] = val; }


//[[Rcpp::export]]
NumericVector SolveYahtzeecpp(){
  NumericVector A (n*m);  NumericVector D (sn);  bool VV[sn];
  int i1 = 0;  int i2 = 0;  int i3 = 0;  int i4 = 0;  int Si = 0;  int Hi = 0;
  for(i1 = 0; i1 < n-1; i1++){
    ii1 = m*Ind[i1];  int iii1 = Ind[i1];  int ii2 = m;
    for(i2 = 0; i2 < sn; i2++){
      if(iii1%2==0){ D[i2] = A[ii1+ii2];  VV[i2] = true; }
      else{ VV[i2] = false; }
      iii1 /= 2;  ii2 *= 2;
    }
    for(i2 = m-1; i2 >= 0; i2--){
      reroll = (i2+holdn-1)/holdn;
      saved = (i2+holdn-1)%holdn;
      if(reroll == 0){ o = thrown; }
      else{ o = IndsInd[saved]; }
      rr = holdn*reroll;
      I3 = 0;
      for(i3 = 0; i3 < dice; i3++){
        if(Throwsn[i3] == o){ break; }
        I3 += Throwsn[i3];
      }
      saved = thrown*saved;
      for(i3 = 0; i3 < o; i3++){
        Si = sn*Inds[i3+saved];
        for(i4 = 0; i4 < sn; i4++){
          if(VV[i4]){ p = std::max(p,Scores[Si] + D[i4]); }
          Si++;
        }
        if(reroll < rerolls){
          Hi = dn*Inds[i3+saved];
          for(i4 = 0; i4 < dn; i4++){
            p = std::max(p,A[ii1+rr+HoldingInds[Hi]]);
            Hi++;
          }
        }
        A[ii1+i2] += p*Chances[I3+i3];
        p = 0;
      }
    }
  }
  return A;
}


