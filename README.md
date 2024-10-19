# Battleship

This project is a text-based version of the classic [Battleship](https://en.wikipedia.org/wiki/Battleship_(game)) game implemented in Haskell. The objective of the game is to guess the locations of hidden ships on a board by receiving feedback on each guess. The game continues until the correct locations are guessed.

## Implementation

This implementation is a simplified version of Battleship, where:

- Each battleship occupies **only one grid** on the board, as opposed to spanning multiple grids like in the original game.
- The grid size is **8 x 4**, giving you a smaller search space for guessing compared to the traditional 10 x 10 grid.
- There are three hidden ships, and you need to guess their locations based on feedback received after each guess.

After each guess, the game will return a triple as feedback:

- **first value**: number of ships in the correct position
- **second value**: number of ships 1-[Manhattan distance](https://en.wikipedia.org/wiki/Taxicab_geometry) away from the correct location
- **third value**: number of ships 2-Manhattan distance away from the correct location

## Usage

It is recommended to run the code for this project within a development container in VS Code.

1. Configure the environment:

```sh
git clone https://github.com/wille-wang/battleship.git
cd battleship
code .
devcontainer open .  # Install `ms-vscode-remote.remote-containers` in VS Code first
```

2. Activate GHCi:

```sh
cd src
ghci
```

3. Load the program:

```
:l Main.hs
```

4. Run the main function:

```
main
```

## Customization

The locations of the battleships can be configured by modifying the `testCase` variable in `Main.hs`:

```haskell
testCase = "F1 D2 G4"  -- Change this to any valid grid coordinates
```

|     | A   | B   | C   | D   | E   | F   | G   | H   |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1   |     |     |     |     |     | X   |     |     |
| 2   |     |     |     | X   |     |     |     |     |
| 3   |     |     |     |     |     |     |     |     |
| 4   |     |     |     |     |     |     | X   |     |
