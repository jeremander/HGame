{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.TicTacToe where

import Control.Monad.Identity
import Control.Monad.Random
import Data.List (intercalate)
import Data.List.Split (chunksOf)

import AI
import Game

newtype Tile = Tile Int
    deriving (Enum, Eq, Ord)

(x : o : blank : _) = [Tile 0 ..]

instance Show Tile where
    show tile
        | tile == x = "X"
        | tile == o = "O"
        | otherwise = " "

newtype Winner = Winner Tile

instance Show Winner where
    show (Winner tile) = if (tile == blank) then "It's a draw." else (show tile ++ " wins!")

newtype Board = Board [Tile]
    deriving (Eq, Ord)

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
    | xWins = x
    | oWins = o
    | otherwise = blank
    where
        getMask tile = [t == tile | t <- tiles]
        maskWins mask = or [and [mask !! i | i <- row] | row <- rows]
        xWins = maskWins $ getMask x
        oWins = maskWins $ getMask o

data TicTacToeState = TicTacToeState Board Turn2P
    deriving (Eq, Ord)
newtype TicTacToeAction = TicTacToeAction Int
    deriving (Read, Show) via Int
type instance Act TicTacToeState = TicTacToeAction
type instance Info TicTacToeState = TicTacToeState
type instance Outcome TicTacToeState = Winner

instance Show TicTacToeState where
    show state@(TicTacToeState board turn) = show board ++ "\n" ++ if (isTerminal state) then "" else show turn

instance GameState TicTacToeState where
    isTerminal (TicTacToeState board@(Board tiles) _) = (blank `notElem` tiles) || (winner board /= blank)
    outcome (TicTacToeState board _) = Winner $ winner board
    validActions (TicTacToeState (Board tiles) _) = TicTacToeAction <$> [k | k <- [1..9], tiles !! (k - 1) == blank]
    validateAction (TicTacToeState (Board tiles) _) action@(TicTacToeAction k)
        | (1 <= k) && (k <= 9) = if (tiles !! (k - 1)) == blank
            then Right action
            else Left $ "ERROR: square " ++ show k ++ " is occupied"
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

emptyBoard = Board $ replicate 9 blank
ticTacToeStart = TicTacToeState emptyBoard (Turn2P False)

type TicTacToePlayer m = Player m TicTacToeState
type TicTacToe m = Game m TicTacToeState

mkTicTacToe :: (Monad m) => TicTacToePlayer m -> TicTacToePlayer m -> TicTacToe m
mkTicTacToe p1 p2 = Game { initialState = return ticTacToeStart, getPlayer = func }
    where func (TicTacToeState _ (Turn2P b)) = if b then p2 else p1

ticTacToePrompt = "Select square (1-9)\n> "

humanPlayer' :: (MonadIO m, GameState s, GameAction m s, Read (Act s), GameInfo s, Show (Info s)) => Player m s
humanPlayer' = humanPlayer ticTacToePrompt

twoPlayerTicTacToe :: TicTacToe IO
twoPlayerTicTacToe = mkTicTacToe humanPlayer' humanPlayer'

ticTacToeExhaust :: TicTacToe []
ticTacToeExhaust = gameExhaust ticTacToeStart

ticTacToeTurn :: TicTacToeState -> IsTurn TicTacToeState
ticTacToeTurn (TicTacToeState _ (Turn2P b)) (TicTacToeState _ (Turn2P b')) = b == b'

ticTacToeEval :: GameEval TicTacToeState Int
ticTacToeEval = GameEval {
    isTurn = \(TicTacToeState _ (Turn2P b)) -> not b,
    stateEval = \(TicTacToeState board _) -> let w = winner board in
        if (w == x)
            then 1
            else if (w == o)
                then -1
                else 0
}

-- ticTacToeEvalMemo = memoizeGameEval ticTacToeEval

-- heuristic that (for efficiency) pre-computes the game value of zero for any state with at most one tile on the board
ticTacToeHeuristic :: Heuristic TicTacToeState Int
ticTacToeHeuristic (TicTacToeState (Board tiles) _)
    | (numBlank >= 8) = Just 0
    | otherwise       = Nothing
    where numBlank = sum $ (fromEnum . (== blank)) <$> tiles

ticTacToeMinimax = applyHeuristic ticTacToeHeuristic (minimax ticTacToeEval)

minimaxTicTacToePlayer :: (Monad m) => Player (RandT StdGen m) TicTacToeState
minimaxTicTacToePlayer = aiPlayer ticTacToeMinimax

onePlayerTicTacToe1 :: TicTacToe (RandT StdGen IO)
onePlayerTicTacToe1 = mkTicTacToe humanPlayer' minimaxTicTacToePlayer

onePlayerTicTacToe2 :: TicTacToe (RandT StdGen IO)
onePlayerTicTacToe2 = mkTicTacToe minimaxTicTacToePlayer humanPlayer'

zeroPlayerTicTacToe :: TicTacToe (RandT StdGen Identity)
zeroPlayerTicTacToe = mkTicTacToe minimaxTicTacToePlayer minimaxTicTacToePlayer

ticTacToeMenu :: Menu (TicTacToe (RandT StdGen IO))
ticTacToeMenu = Menu {
    title = "Choose player",
    options = [("X", "Player 1", onePlayerTicTacToe1), ("O", "Player 2", onePlayerTicTacToe2)],
    prompt = "> ",
    caseSensitive = False
}

playTicTacToe :: IO ()
playTicTacToe = do
    game <- getMenuInput ticTacToeMenu
    gen <- newStdGen
    evalRandT (runGameIO game) gen
