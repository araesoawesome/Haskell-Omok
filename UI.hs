module UI where

import Data.Char
import System.IO
import System.Exit
import Board

{-
main
    Main function to play an omok game between two human players.
    It returns an IO() value. The dimension of the board is 15x15, 
    and user inputs are read from the standard input (see the readXY 
    function below) and outputs like the board state and the game
    outcome are printed on the standard output.
-}
main = do
    --Prints the rules of Omok, in case the user has not played before.
    putStrLn "\nWelcome to Omok!\nOmok, meaning \"five pieces\", is a two-player strategy game typically played with go pieces, \nblack and white stones, on a go board with 15x15 intersections (or places) [Wikipedia]. \nIt can also be played with a paper and pencils because stones, once placed, are not allowed \nto be moved or removed from the board. The two players alternate in placing a stone of their color on an empty intersection, and the \ngoal of the game is to place one's stones in a row of five consecutive intersections vertically, horizontally, or diagonally. \nThe winner is the first player to get an unbroken row of five stones.\n\nHave fun!"
    let bd = mkBoard 15
    --Prints the initial board, essentially a bunch of '.'s
    putStrLn (boardToStr playerToChar bd)
    start bd
{-
Starts the game. Creates the players, and uses the board created in the main function.
-}
start bd = do
    c <- readXY bd mkPlayer
    let board1 = mark (fst c) (snd c) bd mkPlayer 
    --Prints the board for the user
    putStrLn (boardToStr playerToChar board1)
    --Checks if move results in game over. This must be done after every move.
    gameOverOut board1 mkPlayer

    d <- readXY board1 mkOpponent
    let board2 = mark (fst d) (snd d) board1 mkOpponent 
    --Prints the board for the user
    putStrLn (boardToStr playerToChar board2)
    --Checks if move results in game over. This must be done after every move.
    gameOverOut board2 mkOpponent

    --Uses recursion to play with the updated board.
    start board2

{-
gameOverOut bd p
This function uses the isWonBy and isDraw to test if the game is finished. The game ends when either player wins or a draw occurs (as in, the board becomes full and no wins are encountered).
-}
gameOverOut bd p = do
    if (isWonBy bd p)
    then do
        putStrLn ("Player " ++ (charToStr p :: String) ++ " wins. Congratulations!")
        main
    else if (isDraw bd)
    then do
        putStrLn "Draw, no one wins.\n\n"
        main
    else return()

{-
readXY bd p
     Read a 1-based pair of indices (x, y) for player p, denoting an 
     unmarked place in a board bd. The function reads inputs from the
     standard input (stdin) and returns an IO value such as IO(Int,Int)
     or IO(Integer,Integer).
-}
readXY bd p = do
    putStrLn ("Player " ++ (charToStr p :: String))

    putStr "Input x coordinate: "
    x <- getX

    putStr "Input y coordinate:  "
    y <- getX

    --Has the input space coordinate been taken already?
    if isEmpty x y bd
    then return (x,  y)
    else do
        putStrLn "\n\nSpace is not empty.\n\n"
        readXY bd p

getX = do
    putStrLn "Enter a positive value? (Enter -1 to exit program)"
    line <- getLine
    let parsed = reads line :: [(Int, String)] in
        if length parsed == 0
        then getX'
        else 
            let (x, _) = head parsed in
                -- added this bit in to the code block presented in the text document. 
                if x == -1 
                then exitGame
                else if (x >= 1) && (x <= 15)
                then return x
                else getX'
    where getX' = do
          putStrLn "Invalid input!"
          getX

{-
This function is used to gain an extra 5 points assignment credit. It also exits the program, as a bonus.
-}
exitGame = do
    putStrLn "Thank you for playing :) Exiting program..."
    --Exits the program successfully. exitFailure may also be used, it is not ideal for this situation, however.
    exitSuccess