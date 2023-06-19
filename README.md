Welcome to Solve-Yahtzee: an R program that can solve user specified versions of Yahtzee.

1. To run the program download all four files into an instance of R.
2. In the bottom of the Yahtzee.R file there are five lines that should only be run after loading every function from the four files
3. Run all functions in the four files
4. In the Yahtzee.R file at the bottom there is the line "Yahtzee()" run it. A prompt will appear in the console asking if a standard game of Yahtzee should be created or a custom one.
5. When the desired game is setup, run the "SolveYahtzee()" line to solve the game.
6. The "Y[1,1]" is the theoretical expected value for an optimal game.
7. The "Average()" line will run 1000 simulated games of the setup version of Yahtzee to obtain a sample mean, which should be close to "Y[1,1]".
8. The last line "PlayYahtzee()". Guides the user through a game and gives the optimal choices.
