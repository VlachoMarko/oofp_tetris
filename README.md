# Tetris

This project was developed as part of the course **Object-Oriented and Functional Programming** at **Vrije Universiteit Amsterdam (VU)**. The goal of this assignment is to implement a simplified version of the classic Tetris game using functional and object-oriented principles in Scala.

## Project Overview

In this assignment, we implement the game logic for **Tetris**. The core mechanics of Tetris revolve around tetrominoes—geometric shapes consisting of four connected squares. Our version of Tetris includes simplified rules with no scoring, no "ghost" pieces, and no difficulty progression. The assignment is divided into two main parts:

1. **Tetris Assignment 3.1**: Basic Tetris functionality with piece rotation and movement.
2. **Tetris Assignment 3.2**: Full implementation of Tetris, including rotation, immutability, and use of higher-order functions.

## Tetris Features

### Tetrominoes
Tetris features seven distinct tetrominoes, each with unique shapes. These are represented by their letter codes: I, O, T, S, Z, J, and L. For each piece, rotation and anchor-based movement are implemented using functional principles. There are three types of rotations:

1. **Regular Rotation** (for J, L, S, T, Z): The tetromino rotates around the center of the shape.
2. **No Rotation** (for O): The O tetromino remains unchanged upon rotation.
3. **Special Rotation** (for I): The I tetromino rotates around its anchor with additional position adjustment to maintain correct alignment.

### Game Logic
The game logic is handled in the `TetrisLogic.scala` file. The main components of the game logic include:

- **Piece Movement**: Moving pieces left, right, and down.
- **Rotation**: Clockwise and counterclockwise rotations for each piece.
- **Hard Drop**: Dropping the current tetromino to its lowest valid position immediately.
- **Collision Detection**: Preventing invalid moves such as moving a tetromino outside the game board or overlapping with placed pieces.
- **Game Over Detection**: Checking whether new pieces can be placed. If not, the game ends.

### Interaction with Game State
The game is managed through an immutable game state. The drawing code (`TetrisGame.scala`) interacts with the logic by querying the `TetrisLogic` class for each cell's type. The game state is updated as tetrominoes are placed, and when the player presses specific keys to control the game.
