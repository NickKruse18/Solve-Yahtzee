NOrdWRep = function(n,k){
  w = T;  m = 0;  C = rep(n,k);  D = matrix(0,factorial(n+k-1)/factorial(n-1)/factorial(k),k)
  if(k==1){ return(matrix(n:1,n,1)) }
  while(w){
    m = m + 1;  D[m,] = C;  C[1] = C[1] - 1
    for(i in 1:(k-1)){
      if(C[i]==0){ C[i+1] = C[i+1] - 1;  C[1:i] = C[i+1]; }
    }
    if(C[k]==0){ w = F }
  }
  return(D)
}

Holding = function(){
  Hold = NOrdWRep(sides+1,dice-1)
  IndHold = (Hold%*%((sides+1)^(2:dice-2)))[1:nrow(Hold)]
  IndHold2 = numeric(max(IndHold))
  IndHold2[IndHold] = 1:length(IndHold)
  dn = 2^dice-1
  Inds = matrix(0,thrown,dn)
  for(i1 in 1:thrown){
    throw = Throw[i1,]
    for(i2 in 1:dn){
      H = throw[(as.integer(intToBits(i2-1))==1)]
      H = sort(c(H+1,rep(1,dice-1-length(H))))
      Inds[i1,i2] = IndHold2[H%*%((sides+1)^c(2:dice-2))]
    }
  }
  return(Inds)
}

Probabilities = function(Throw,n){
  k = length(Throw[1,]);  m = length(Throw[,1]);  Chance = numeric(m)
  for(i in 1:m){
    Chance[i] = factorial(k)/(n^k)
    count = numeric(k)
    for(j in 1:n){
      Chance[i] = Chance[i]/factorial(sum(Throw[i,]==j))
    }
  }
  return(Chance)
}


