# Clojure | Connect Four
Connect Four is a two-player connection game in which the players first choose a color and then take turns dropping colored discs from the top into a 7x6 grid. The pieces fall straight down, occupying the lowest available space within the column. The objective of the game is to be the first to form a horizontal, vertical, or diagonal line of four of one's own discs.

## Description

This program is a text-based implementation of the game Connect Four made with functional programming language Clojure. 

To play the game, the user will be prompted to enter the column number where they would like to drop their piece (0-6). The game will then display the updated board and then determine the move of the computer player. He does this by using a minimax algorithm. The game will continue until a player wins or the board is full.

## Notes
- Player 1 is the human player, and Player 2 is the computer player.
- The **difficulty** of the computer player can be adjusted by changing the `ai` variable in the `src/connectfour/core.clj` file. The value determines the depth of the minimax algorithm. The higher the value, the more difficult the computer player will be.
- To turn on **debug mode**, change the debug variable in the `src/connectfour/core.clj` file to true. This will display the minimax algorithm's scores for each possible move.

## Running the Program
To run the program:

1. Open a terminal
2. Navigate to the directory where the program is located (/path/to/ConnectFour). In this directory, you should see the `project.clj` file and a `lein.bat` file.
3. Run the program with the following command:
```bash
lein run
```

Terminating the program can be done by pressing `Ctrl + C` in the terminal or by finishing the game.
