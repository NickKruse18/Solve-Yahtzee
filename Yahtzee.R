Yahtzee = function(){
  print("Welcome to Solve Yahtzee")
  print("This program can solve Yahtzee for any amount of dice & any size of dice.")
  print("It also allows for any scoring category.")
  print("However, it can't account for some common rules like, upper section bonus or Yahtzee bonus.")
  die <<- 5;  sides <<- 6;  rerolls <<- 2;
  Rules <<- matrix(c(1,1,0,0,
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
  x = readline(prompt="Is a standard game of Yahtzee desired? (Y/N): ")
  while((x!="Y")&(x!="N")){ x = readline(prompt="Please type either Y or N: ") }
  if(x=="N"){
    x = as.integer(readline(prompt="How many dice should be used? "))
    die<<- x
    x = as.integer(readline(prompt="How many sides should each dice have? "))
    sides <<- x
    x = as.integer(readline(prompt="How many rerolls in each round? "))
    rerolls <<- x
    Rules <<- WriteRules()
  }
  sn <<- nrow(Rules)
  thrown <<- factorial(sides+die-1)/factorial(sides-1)/factorial(die)
  holdn <<- factorial(sides+die-1)/factorial(sides)/factorial(die-1)
  
  Throw <<- NOrdWRep(sides,die)
  
  IndThrow <<- (Throw%*%(sides^c(1:die-1)))[1:nrow(Throw)]
  IndThrow2 <<- numeric(max(IndThrow))
  IndThrow2[IndThrow] <<- 1:length(IndThrow)
  
  Throws <<- list()
  for(i in die:1){ Throws <<- append(Throws,list(NOrdWRep(sides,i))) }
  
  Scores <<- Score(Throw)
  Chance <<- Probabilities(Throw,sides)
  
  Chances <<- list(); Throwsn <<- c()
  for(i in 1:die){ Chances <<- append(Chances,list(Probabilities(Throws[[i]],sides)));  Throwsn <<- c(Throwsn,nrow(Throws[[i]])) }
  
  HoldingInds <<- Holding()
  Rn <<- RuleNames()
}

WriteRules = function(){
  k = max(die%/%2+2,3);  Rules = matrix(0,0,k)
  print("Now for the scoring categories.")
  print("Type the categories that should be in the uppersection.")
  print("For example if a Threes, a Fives and a Sixes are desired then type 356")
  print("If multiple of the same category is desired then type the number more than once")
  print("Be aware that each added category doubles the computation time.")
  x = readline(prompt = "Type the Uppersection categories (type N to skip): ")
  if(x!="N"){
    x = as.integer(strsplit(x,"")[[1]])
    M = matrix(0,length(x),k);  M[,1] = 1;  M[,2] = x
    Rules = rbind(Rules,M)
  }
  print("Next type the categories that are based on having duplicates like, Pair or Three of a Kind.")
  print("Each of these are typed individually.")
  print("Examples:")
  print("Three of a Kind should be typed as 3.")
  print("2 Pairs of 2 should be typed as 22.")
  print("Full House should be typed as 23.")
  W = T
  while(W){
    x = readline(prompt = "Type a Pair category (if finished type N): ")
    if(x=="N"){ break }
    x = as.integer(strsplit(x,"")[[1]])
    M = c(2,0,sort(x),numeric(k-2-length(x)))
    x = readline(prompt="What should the score for this be?, type 0 if it should be the sum of all dice: ")
    M[2] = as.integer(x)
    Rules = rbind(Rules,M)
  }
  print("Next type the categories that are based on straights.")
  print("Write the number of how many consecutive numbers it should have.")
  print("Each of these are typed individually.")
  print("Examples:")
  print("A Small Straight, 1,2,3,4, is typed as 4")
  print("A Large Straight, 1,2,3,4,5, is typed as 5")
  while(W){
    x = readline(prompt = "Type a Straight category (if finished type N): ")
    if(x=="N"){ break }
    x = as.integer(x)
    M = c(3,0,x,numeric(k-3))
    x = readline(prompt="What should the score for this be?: ")
    M[2] = as.integer(x)
    Rules = rbind(Rules,M)
  }
  print("Next type how many Chance categories that should be used.")
  print("Examples:")
  print("If two Chances should be included then type 2")
  x = readline(prompt = "Type how many Chances (if finished type N): ")
  if(x!="N"){
    x = as.integer(x)
    M = matrix(0,x,k);  M[,1] = 4
    Rules = rbind(Rules,M)
  }
  print("Next type the Yahtzee categories.")
  print("Write the number of how many consecutive numbers it should have.")
  print("Each of these are typed individually.")
  while(W){
    x = readline(prompt = "Should the sum of all dice be included? (1/0) OR (if finished type N): ")
    if(x=="N"){ break }
    x = as.integer(x)
    M = c(5,0,x,numeric(k-3))
    x = readline(prompt="What should the base score for this be?: ")
    M[2] = as.integer(x)
    Rules = rbind(Rules,M)
  }
  return(Rules)
}

SimYahtzee = function(){
  Crosses = numeric(sn);  H = numeric(die-1);  W = T;  reroll = 0;  ind = 1;  S = 0
  while(W){
    roll = H[H!=0];  roll = sort(c(roll,ceiling(runif(die-length(roll))*sides)))
    M = -1;  Mind = 0;  C = T;  Sc = Scoring(roll)
    for(i in 1:sn){ if(Crosses[i]>0){ next }
      if(Y[ind+2^(i-1),1]+Sc[i]>M){ Mind = i;  M = Y[ind+2^(i-1),1]+Sc[i] }
    }
    if(reroll<rerolls){
      rind = IndThrow2[(roll%*%(sides^c(1:die-1)))]
      for(i in 1:(2^die-1)){
        if(Y[ind,1+holdn*reroll+HoldingInds[rind,i]]>M){ Mind = i;  M = Y[ind,1+holdn*reroll+HoldingInds[rind,i]];  C = F }
      }
    }
    if(C){ ind = ind + 2^(Mind-1);  Crosses[Mind] = 1;  S = S + Sc[Mind];  H = numeric(die-1);  reroll = 0 }
    else{ reroll = reroll + 1;  H = roll[(as.integer(intToBits(Mind-1))[1:die]==1)] }
    
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

PlayYahtzee = function(){
  Crosses = rep(-1,sn);  H = numeric(die-1);  W = T;  reroll = 0;  ind = 1;  S = 0
  while(W){
    w = T
    while(w){
      roll = readline(prompt="What did you roll? ")
      if(roll=="T"){ return() }
      suppressWarnings({
        roll = sort(as.integer(strsplit(roll,"")[[1]]))
      })
      if(length(roll)==die){ if(max(roll)>sides){ print(paste0("The numbers must be between 1 and ",sides));  next }
        if(min(roll)<1){ print(paste0("The numbers must be between 1 and ",sides));  next };  w = F }
      else{ print(paste0("The dice must be written as exactly ",die," consequtive numbers without spaces.")) }
    }
    print(roll)
    M = -1;  Mind = 0;  C = T;  Sc = Scoring(roll)
    for(i in 1:sn){ if(Crosses[i]>-1){ next }
      if(Y[ind+2^(i-1),1]+Sc[i]>M){ Mind = i;  M = Y[ind+2^(i-1),1]+Sc[i] }
    }
    if(reroll<rerolls){
      rind = IndThrow2[(roll%*%(sides^c(1:die-1)))]
      for(i in 1:(2^die-1)){
        if(Y[ind,1+holdn*reroll+HoldingInds[rind,i]]>M){ Mind = i;  M = Y[ind,1+holdn*reroll+HoldingInds[rind,i]];  C = F }
      }
    }
    if(C){ ind = ind + 2^(Mind-1);  Crosses[Mind] = Sc[Mind];  S = S + Sc[Mind];  H = numeric(die-1);  reroll = 0
    if(Sc[Mind]!=1){ print(paste0("Score it in ",Rn[Mind]," for ",Sc[Mind]," points.")) }else{ print(paste0("Score it in ",Rn[Mind]," for 1 point.")) } }
    else{ reroll = reroll + 1;  H = roll[(as.integer(intToBits(Mind-1))[1:die]==1)]
    if(length(H[H!=0])>0){ cat("Keep",H[H!=0],"and reroll the rest.") }else{ print("Reroll everything.") } }
    if(sum(Crosses>-1)==sn){ W = F }
  }
  print("Final Results:")
  for(i in 1:sn){
    print(paste0(Rn[i],": ",Crosses[i]))
  }
  print(paste0("With a final score of ",S))
}


Yahtzee()
Y = SolveYahtzee()
Y[1,1]
Average()
PlayYahtzee()

