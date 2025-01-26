Yahtzee = function(){
  thrown <<- factorial(sides+dice-1)/factorial(sides-1)/factorial(dice)
  holdn <<- factorial(sides+dice-1)/factorial(sides)/factorial(dice-1)
  
  Throw <<- NOrdWRep(sides,dice)
  
  IndThrow <<- (Throw%*%(sides^c(1:dice-1)))[1:nrow(Throw)]
  IndThrow2 <<- numeric(max(IndThrow))
  IndThrow2[IndThrow] <<- 1:length(IndThrow)
  
  Throws <<- list()
  for(i in dice:1){ Throws <<- append(Throws,list(NOrdWRep(sides,i))) }
  
  Scores <<- Score(Throw)
  Chance <<- Probabilities(Throw,sides)
  
  Chances <<- list(); Throwsn <<- c()
  for(i in 1:dice){ Chances <<- append(Chances,list(Probabilities(Throws[[i]],sides)));  Throwsn <<- c(Throwsn,nrow(Throws[[i]])) }
  
  HoldingInds <<- Holding()
  
  Holds <<- NOrdWRep(sides+1,dice-1)-1
  
  Inds <<- matrix(0,thrown,holdn)
  IndsInd <<- numeric(holdn)
  for(i1 in 1:holdn){
    H = Holds[i1,];  H = H[(H!=0)]
    H2 = Throws[[length(H)+1]];  IndsInd[i1] <<- length(H2[,1])
    for(i2 in 1:length(H2[,1])){
      h = sort(c(H,H2[i2,]))
      Inds[i2,i1] <<- IndThrow2[h%*%(sides^c(1:dice-1))]
    }
  }
  n = 2^sn
  Ind = (n-1):1
  Ind = sapply(Ind-1,function(x){ sum(as.integer(intToBits(x)))})
  Ind = ((n-1):1)[order(Ind,decreasing = T)]
  
  Ind <<- Ind
  Yahtzeeintcpp(sn,rerolls,holdn,thrown,dice)
  HoldingInds = t(HoldingInds);  Chances = unlist(Chances);  Scores = t(Scores)
  
  for(i in 1:(n-1)){ YahtzeeIndcpp(i-1,Ind[i]-1) }
  for(i in 1:(thrown*holdn)){ YahtzeeIndscpp(i-1,Inds[i]-1) }
  for(i in 1:holdn){ YahtzeeIndsIndcpp(i-1,IndsInd[i]) }
  for(i in 1:(thrown*31)){ YahtzeeHoldingIndscpp(i-1,HoldingInds[i]) }
  for(i in 1:length(Chances)){ YahtzeeChancescpp(i-1,Chances[i]) }
  for(i in 1:dice){ YahtzeeThrowsncpp(i-1,Throwsn[i]) }
  for(i in 1:(sn*thrown)){ YahtzeeScorescpp(i-1,Scores[i]) }
  return(matrix(SolveYahtzeecpp(),n,byrow = T))
}

SimYahtzee = function(){
  Crosses = numeric(sn);  H = numeric(dice-1);  W = T;  reroll = 0;  ind = 1;  S = 0
  while(W){
    roll = H[H!=0];  roll = sort(c(roll,ceiling(runif(dice-length(roll))*sides)))
    M = -1;  Mind = 0;  C = T;  Sc = Scoring(roll)
    for(i in 1:sn){ if(Crosses[i]>0){ next }
      if(Y[ind+2^(i-1),1]+Sc[i]>M){ Mind = i;  M = Y[ind+2^(i-1),1]+Sc[i] }
    }
    if(reroll<rerolls){
      rind = IndThrow2[(roll%*%(sides^c(1:dice-1)))]
      for(i in 1:(2^dice-1)){
        if(Y[ind,1+holdn*reroll+HoldingInds[rind,i]]>M){ Mind = i;  M = Y[ind,1+holdn*reroll+HoldingInds[rind,i]];  C = F }
      }
    }
    if(C){ ind = ind + 2^(Mind-1);  Crosses[Mind] = 1;  S = S + Sc[Mind];  H = numeric(dice-1);  reroll = 0 }
    else{ reroll = reroll + 1;  H = roll[(as.integer(intToBits(Mind-1))[1:dice]==1)] }
    
    if(sum(Crosses)==sn){ W = F }
  }
  return(S)
}

Average = function(){
  M = 0;  M2 = 0;  n = 1000
  for(i in 1:n){
    m = SimYahtzee()
    M = M + m;  M2 = M2 + m^2
  }
  M = M/n;  M2 = sqrt(M2/n-M^2)
  return(c(M-2*M2/sqrt(n),M+2*M2/sqrt(n)))
}



{
  library("Rcpp")
  library("microbenchmark")
  sourceCpp("Cpp.cpp")
}

Y = Yahtzee()
Yahtzeeintcpp(13,2,210,252,5)
system.time({SolveYahtzeecpp()})
dice = 5
