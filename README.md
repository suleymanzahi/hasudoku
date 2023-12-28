# hasudoku

A Soduko solver and verifier written in Haskell.

## Getting Started

### Dependencies

To run this program, a Haskell compiler, like GHC is needed. Consult https://www.haskell.org/ghc/ on how to download GHC for your platform.

### Running the solver
The program can be run with the command ```runghc Solve.hs```. The program will prompt for a file, an example file named ```easy50.txt``` is provided. You can use other files as well, but they have to be in the same format as ```easy50.txt``` and contain valid sudokus. A menu is printed out. You can attempt to solve the selected sudoku step-by-step, let the solver solve it or solve all sudokus in the file.

### Running the verifier
The program can be run with the command ```runghc Verify.hs```. Like the solver, the program will prompt for a file. It prints```True``` if a sudoku is valid, ```False``` otherwise. The file ```inconsistent20.txt``` contains invalid sudokus, which can used for the verifier.

## License

This project is licensed under the MIT License - see the LICENSE.md file for details
