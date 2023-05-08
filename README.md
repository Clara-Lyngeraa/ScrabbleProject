# ScrabbleProject
#Fart City

----
# Running the program
From root, type in *make* in the terminal.

Alternatively, type in *dotnet run --project ScrabbleTemplate/ScrabbleTemplate*

The program cannot be executed directly through Rider, it has to be run via the terminal.

# Background
We are trying to get 2 points.
We have a list of anchor points – they are printed in the terminal. 
If we can't build any words starting from these anchor points we will try to swap tiles.
If there are no tiles left from the server and get this information, we pass. This means that we often end with having either no more anchor points 
or having a hand with a few consonants that we cannot use, like 'x' or 'z'.

We have run the program over ten times and we end in these scenarios:
1. We are out of anchor points and tiles, so the game ends
2. We are out of anchor points and have less than 5 tiles left, so we pass 3 times
3. We have less than 7 tiles left, but we cannot build a word because the letters are hard to place consonants, so we pass 3 times
  
  
Multi-player and dictionary: no
playing on all boards: no
parallelism: no
Respect the timeout flag: no
