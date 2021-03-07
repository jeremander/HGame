{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module AI where

import Control.Monad.Identity
import Control.Monad.Memo
import Control.Monad.Random

import Game

idMemo :: (Ord k) => (k -> v) -> k -> v
idMemo f = startEvalMemo . memo (return . f)

-- function that evalutes states
type StateEval s v = s -> v
-- function determining whether the state is the current player's turn
type IsTurn s = s -> Bool

data GameEval s v = GameEval {
    -- determines if a state is the turn of the maximizing player
    isTurn :: IsTurn s,
    -- computes the value of a state
    stateEval :: StateEval s v
}

-- memoized version of GameEval that caches results for each state
memoizeGameEval :: (Ord s) => GameEval s v -> GameEval s v
memoizeGameEval (GameEval {isTurn, stateEval}) = GameEval {isTurn = idMemo isTurn, stateEval = idMemo stateEval}

getBestActions :: (Eq s, GameAction Identity s, Eq v) => StateEval s v -> s -> [Act s]
getBestActions eval state = [action | action <- validActions state, eval (runIdentity $ runAction state action) == score]
    where
        children = childStates state
        score = eval state

aiPlayer :: (Eq s, GameState s, GameAction Identity s, Eq v, MonadRandom m) => StateEval s v -> Player m s
aiPlayer eval state = fromList [(action, 1) | action <- actions]
    where actions = getBestActions eval state

-- given a GameEval that identifies the current player's turn and evaluates terminal states, IsTurn function determining whose turn it is, and an Eval that evaluates terminal states, returns an Eval that evaluates every state (using minimax criterion w.r.t. the player at the current state)
minimax :: (Eq s, GameState s, GameAction Identity s, Real v) => GameEval s v -> StateEval s v
minimax ge state
    | isTerminal state = (stateEval ge) state
    | (isTurn ge) state = maximum childScores
    | otherwise = minimum childScores
        where childScores = minimax ge <$> childStates state

minimaxAiPlayer :: (Eq s, GameState s, GameAction Identity s, Real v, MonadRandom m) => GameEval s v -> Player m s
minimaxAiPlayer = aiPlayer . minimax