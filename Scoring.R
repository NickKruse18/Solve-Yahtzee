Scoring = function(throw){
  s = numeric(sn)
  for(i in 1:sn){
    if(Rules[i,1]==1){ s[i] = sum(throw==Rules[i,2])*Rules[i,2] }
    if(Rules[i,1]==2){
      Dups = c();  dup = 1;  j = 1
      while(j < die){
        if(throw[j] == throw[j+1]){ dup = dup + 1 }
        else if(dup>1){ Dups = c(Dups,dup);  dup = 1 }
        j = j + 1
      }
      if(dup>1){ Dups = c(Dups,dup);  dup = 1 }
      Pairs = sort(Rules[i,3:ncol(Rules)],decreasing=T)
      Pairs = Pairs[Pairs!=0]
      j = 1
      while(j <= length(Dups)){ I = match(Dups[j],Pairs)
        if(!is.na(I)){ Pairs = Pairs[-I];  Dups = Dups[-j] } else{ j = j + 1 } }
      j = 1
      while(j <= length(Pairs)){
        if(Pairs[j]<=max(Dups,-Inf)){
          Dups[match(max(Dups),Dups)] = max(Dups) - Pairs[j];  Pairs = Pairs[-j]
        } else { j = j + 1 }
      }
      if(sum(Pairs)==0){ s[i] = Rules[i,2]+(Rules[i,2]==0)*sum(throw) }
    }
    if(Rules[i,1]==3){
      d = diff(throw);  con = 1
      for(j in 1:(die-1)){
        if(d[j]==1){ con = con + 1 } else { con = 1 }
        if(con>=Rules[i,3]){ s[i] = Rules[i,2];  break }
      }
    }
    if(Rules[i,1]==4){ s[i] = sum(throw) }
    if(Rules[i,1]==5){ 
      s[i] = (sum(throw==throw[1])==die)*(Rules[i,2]+sum(throw)*Rules[i,3])
    }
  }
  return(s)
}

Score = function(Throw){
  k = nrow(Throw);  S = matrix(0,k,sn)
  for(i in 1:k){ S[i,] = Scoring(Throw[i,]) }
  return(S)
}

RuleNames = function(){
  names = numeric(sn)
  for(i in 1:sn){
    if(Rules[i,1]==1){ names[i] = paste0(Rules[i,2],"'s") }
    if(Rules[i,1]==2){ 
      if(sum((Rules[i,3:4]-c(2,3))^2)==0){ names[i] = "Full House" }
      else{
        names[i] = paste0("Pair of ",Rules[i,3])
        for(j in 3:ncol(Rules)){
          if(j==3){ next };  if(Rules[i,j]==0){ next }
          names[i] = paste0(names[i]," + Pair of ",Rules[i,j])
        }
      }
      if(Rules[i,2]==0){ names[i] = paste0(names[i],", sum of dice") }
      else{ names[i] = paste0(names[i],", ",Rules[i,2]," points") }
    }
    if(Rules[i,1]==3){ names[i] = paste0("Staight with ", Rules[i,3]," consecutive numbers, ",Rules[i,2]," points") }
    if(Rules[i,1]==4){ names[i] = "Chance, sum of dice" }
    if(Rules[i,1]==5){ names[i] = paste0("Yahtzee, ",Rules[i,2]," points")
      if(Rules[i,3] == 1){ names[i] = paste0(names[i],", sum of dice") }
    }
  }
  return(names)
}

