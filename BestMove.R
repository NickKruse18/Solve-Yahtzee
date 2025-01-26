BestMove = function(roll,X,reroll,currentScore){
  Crosses = rep(F,sn)
  Crosses[X] = T
  if(sum(Crosses)==sn){ return("done") }
  H = numeric(dice-1);  ind = 1 + sum(2^((0:(sn-1))[Crosses]))
  M = -1;  Mind = 0;  C = T;  Sc = Scoring(roll)
  for(i in 1:sn){ if(Crosses[i]){ next }
    if(Y[ind+2^(i-1),1]+Sc[i]>M){ Mind = i;  M = Y[ind+2^(i-1),1]+Sc[i] }
  }
  if(reroll<rerolls){
    rind = IndThrow2[(roll%*%(sides^c(1:dice-1)))]
    for(i in 1:(2^dice-1)){
      if(Y[ind,1+holdn*reroll+HoldingInds[rind,i]]>M){ Mind = i;  M = Y[ind,1+holdn*reroll+HoldingInds[rind,i]];  C = F }
    }
  }
  if(C){ ind = ind + 2^(Mind-1);  Crosses[Mind] = T;  H = numeric(dice-1);  reroll = 0
    return(paste0("Score it in ",Rn[Mind],". Expected Score: ",round(M+currentScore,1))) }
  else{ reroll = reroll + 1;  H = roll[(as.integer(intToBits(Mind-1))[1:dice]==1)]
  if(length(H[H!=0])>0){
    A = "Keep"
    for(i in H[H!=0]){ A = paste0(A," ",i) }
    return(paste0(A," and reroll the rest. Expected Score: ",round(M+currentScore,1)))
  }
  else{ return(paste0("Reroll everything. Expected Score: ",round(M+currentScore,1))) } }
}

SetupGame = function(){
  return(list(sides=1,rerolls=0,dice=1,Rules=matrix(0,0,4),Rn=c(),sn=0))
}

CurrentGame = function(A){
  sides <<- A$sides;  rerolls <<- A$rerolls;  dice <<- A$dice
  Rules <<- A$Rules;  Rn <<- A$Rn;  sn <<- A$sn
}

ComplexityChange = function(A){
  a = 2^A$sn*(1+A$rerolls*factorial(A$sides+A$dice-1)/factorial(A$sides)/factorial(A$dice-1))*factorial(A$sides+A$dice-1)/factorial(A$sides-1)/factorial(A$dice)*(A$sn+2^A$dice)
  b = 2^sn*(1+rerolls*factorial(sides+dice-1)/factorial(sides)/factorial(dice-1))*factorial(sides+dice-1)/factorial(sides-1)/factorial(dice)*(sn+2^dice)
  return(paste0("The new game will increase the solution complexity by: ",round(a/b,2)))
}

StandardGame = function(){
  A = SetupGame()
  A$dice = 5;  A$sides = 6;  A$rerolls = 2;
  A$Rules = matrix(c(1,1,0,0,
                     1,2,0,0,
                     1,3,0,0,
                     1,4,0,0,
                     1,5,0,0,
                     1,6,0,0,
                     2,0,3,0,
                     2,0,4,0,
                     2,25,2,3,
                     3,30,4,0,
                     3,40,5,0,
                     4,0,0,0,
                     5,50,0,0),13,4,byrow = T)
  A$Rn = RuleNames(A$Rules);  A$sn = nrow(A$Rules)
  return(A)
}

SQ = function(n){
  if(n==0){ return(0) }
  return(1:n)
}

SortRules = function(Rules){
  Rules = Rules[order(Rules[,2]),]
  Rules = Rules[order(Rules[,3]),]
  return(Rules[order(Rules[,1]),])
}
