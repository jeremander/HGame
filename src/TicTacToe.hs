{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module TicTacToe where

import Data.List (intercalate)
import Data.List.Split (chunksOf)

import Game


data Tile = X | O | Blank
    deriving (Enum, Eq)

instance Show Tile where
    show tile = case tile of
        X -> "X"
        O -> "O"
        Blank -> " "

newtype Winner = Winner Tile

instance Show Winner where
    show (Winner tile) = case tile of
        Blank -> "It's a draw."
        _     -> show tile ++ " wins!"

newtype Board = Board [Tile]

instance Show Board where
    show (Board tiles) = top ++ intercalate middle (showRow <$> rows) ++ bottom
        where
            rows = chunksOf 3 tiles
            showRow row = "│ " ++ intercalate " │ " (show <$> row) ++ " │\n"
            top    = "┌───┬───┬───┐\n"
            middle = "├───┼───┼───┤\n"
            bottom = "└───┴───┴───┘"

rows = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]]

-- returns X or O if either of them won, otherwise Blank
winner :: Board -> Tile
winner (Board tiles)
    | xWins = X
    | oWins = O
    | otherwise = Blank
    where
        getMask tile = [t == tile | t <- tiles]
        maskWins mask = or [and [mask !! i | i <- row] | row <- rows]
        xWins = maskWins $ getMask X
        oWins = maskWins $ getMask O

data TicTacToeState = TicTacToeState Board Turn2P
newtype TicTacToeAction = TicTacToeAction Int
    deriving (Read, Show) via Int
type instance Act TicTacToeState = TicTacToeAction
type instance Info TicTacToeState = TicTacToeState
type instance Outcome TicTacToeState = Winner

instance Show TicTacToeState where
    show state@(TicTacToeState board turn) = show board ++ "\n" ++ if (isTerminal state) then "" else show turn

instance GameState TicTacToeState where
    isTerminal (TicTacToeState board@(Board tiles) _) = (Blank `notElem` tiles) || (winner board /= Blank)
    outcome (TicTacToeState board _) = Winner $ winner board
    validateAction (TicTacToeState (Board tiles) _) action@(TicTacToeAction k)
        | (1 <= k) && (k <= 9) = case (tiles !! (k - 1)) of
            Blank -> Right action
            _     -> Left $ "ERROR: square " ++ show k ++ " is occupied"
        | otherwise = Left "ERROR: selected square must be from 1 to 9"

instance (Monad m) => GameAction m TicTacToeState where
    runAction (TicTacToeState (Board tiles) turn) (TicTacToeAction k) = return state
        where
            i = fromEnum turn
            tile = toEnum i
            board' = Board [if (i == k - 1) then tile else t | (i, t) <- zip [0..] tiles]
            state = TicTacToeState board' (nextTurn turn)

instance GameInfo TicTacToeState where
    getPlayerInfo = id

emptyBoard = Board $ replicate 9 Blank
ticTacToeStart = TicTacToeState emptyBoard (Turn2P False)

type TicTacToePlayer m = Player m TicTacToeState
type TicTacToe m = Game m TicTacToeState

mkTicTacToe :: (Monad m) => TicTacToePlayer m -> TicTacToePlayer m -> TicTacToe m
mkTicTacToe p1 p2 = Game { initialState = return ticTacToeStart, getPlayer = func }
    where func (TicTacToeState _ (Turn2P b)) = if b then p2 else p1

humanTicTacToe :: TicTacToe IO
humanTicTacToe = mkTicTacToe humanPlayer humanPlayer