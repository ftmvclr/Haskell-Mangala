# Haskell Mancala/Mangala

A command-line version of the traditional game **Mancala**, developed using Haskell.

## Overview
This program creates a variation of the classic two-player Mancala game inside command line. It takes care of creating board states, switching turns, and applying game mechanics like stone distribution, capturing stones, etc.

## Features
* **Two Players Game:** Two players can play against each other using command line input.
* **Mechanics:** 
  * Starting positions: Both players have 6 pits at the beginning of the game, which contain 4 stones per pit initially.
  * Capturing stones: Your last stone lands into your pit and that particular pit is empty then, all stones from that pit will be captured by you along with stones in opponent's corresponding pit.
  * Extra turns: Your last stone ends up landing into your own scoring pit, you do not pass your turn.
* **Display Board After Every Move:** Updated display of the board in command line after making moves.
* **Automatic Score Calculations:** Automatically checks for victory and declares winning player if anyone's side is completely empty.

## Authors
[Ayşe Şerife Sever](https://github.com/AyseSerife)  
[Alper Tam](https://github.com/AlprTm)  
[Fatıma Avcılar](https://github.com/ftmvclr)  
Entirety of this programme was written together over a cup of coffee.
