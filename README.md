# Alpha Beta Pruning in Pawn Only Chess

We were once tasked to think about a chess puzzle, that contained only pawns
and no King. So the goal was to capture all the pawns (similiar to the Horde
mode). Due to the simplicity of the modified version, this project just
tries to write an alpha beta pruning algorithm for the modified chess game.

## Usage

Currently there isn't any UI, but you can use the REPL to try things. Note
that due to most environments being in dark mode, the black pieces might look
white and vice versa.

Usage for the REPL (requires ghc to be installed):

```sh
$ ghc main.hs -o main && ghci main
[1 of 1] Compiling Main             ( main.hs, main.o )
Linking main ...
GHCi, version 9.2.8: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( main.hs, interpreted )
Ok, one module loaded.
ghci>
```

You can create new boards:

```sh
ghci> someBoard = createBoard [(2, 2, White, Pawn), (3, 3, Black, Pawn), (5, 5, White, Pawn), (6, 6, Black, Pawn)] Nothing
```

and evaluate them (the number is the depth to search):

```
ghci> evaluate board4 7 White
(Infinity,[Board (8,8) 2 vs. 1, outside: fromList []en passantable: Nothing
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │♙│ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │♟│ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │♙│ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ ,Board (8,8) 2 vs. 1, outside: fromList []en passantable: Nothing
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │♙│ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │♙│♟│ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ ,Board (8,8) 2 vs. 1, outside: fromList []en passantable: Nothing
 │ │ │ │ │ │♕│ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │♙│♟│ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ ,Board (8,8) 2 vs. 1, outside: fromList []en passantable: Nothing
 │ │ │ │ │ │♕│ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │♙│ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │♟│ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ ,Board (8,8) 2 vs. 1, outside: fromList []en passantable: Nothing
 │ │ │♕│ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │♙│ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │♟│ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ ,Board (8,8) 2 vs. 1, outside: fromList []en passantable: Nothing
 │ │ │♕│ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │♙│ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │♛│ │ │ │ ,Board (8,8) 2 vs. 0, outside: fromList []en passantable: Nothing
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │♙│ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │ │ │ │ │ 
─┼─┼─┼─┼─┼─┼─┼─
 │ │ │♕│ │ │ │ ])
```

## TODO:

- [x] Promoting pawns
- [x] En passant
- [ ] Find the shortest win, when win exists
- [ ] Some UI
