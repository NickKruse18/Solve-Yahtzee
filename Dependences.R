source("Functions.R")
source("Solver.R")
source("Scoring.R")

dice = 5;  sides = 6;  rerolls = 2;
Rules = matrix(c(1,1,0,0,
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
Rn = RuleNames(Rules);  sn = nrow(Rules)
source("Yahtzee.R")
source("BestMove.R")
source("AddField.R")
source("RemoveField.R")