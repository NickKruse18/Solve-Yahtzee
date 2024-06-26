SolveYahtzee = function(){
  n = 2^sn;  m = 1+rerolls*holdn
  A = matrix(0,n,m)
  time = Sys.time()
  i = 0;  pro = 0
  for(i1 in Ind){
    G = as.integer(intToBits(i1-1))[1:sn]
    I2 = rep(i1,sn)
    for(i2 in 1:sn){ if(G[i2]==0){ I2[i2] = G%*%(2^(1:sn-1))+1+2^(i2-1) } }
    D = A[I2,1]*(G==0)-1000*G
    for(i2 in m:1){
      reroll = (i2+holdn-2)%/%holdn
      saved = (i2-2)%%holdn + 1
      o = (reroll!=0)*IndsInd[saved]+(reroll==0)*thrown
      rr = 1+holdn*reroll
      P = 1:o
      for(i3 in 1:o){
        p = max(Scores[Inds[i3,saved],] + D)
        if(reroll < rerolls){
          p = max(c(p,A[i1,rr+HoldingInds[Inds[i3,saved],]]))
        }
        P[i3] = p
      }
      A[i1,i2] = P%*%Chances[[match(o,Throwsn)]]
    }
    i = i + 1
    if(20*i/n>pro){ pri = "|";  pro = pro + 1
      for(j in 1:pro){ pri = paste0(pri,"-") }
      for(j in (pro+1):20){ if(j>20){ break };
        pri = paste0(pri," ") }
      pri = paste0(pri,"|")
      print(paste0(pri," Elapsed time: ",round(Sys.time()-time,1)))
    }
  }
  print(Sys.time()-time)
  return(A)
}
