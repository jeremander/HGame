{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Game where

import Control.Monad (void)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State (liftIO, MonadIO, State)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

-- maps from state type to action type
type family Act s :: *
-- maps from state type to information type
type family Info s :: *
-- maps from state type to outcome type
type family Outcome s :: *

class GameState s where
    -- checks whether the game is in a terminal state
    isTerminal :: s -> Bool
    -- gets the outcome of the game (assuming it is in a terminal state)
    outcome :: s -> Outcome s
    -- validates an action for the given state, wrapping the action in an Either
    validateAction :: s -> Act s -> Either String (Act s)
    validateAction _ action = Right action

type Player m s = s -> m (Act s)
-- type HumanPlayer s = Player IO s
type AIPlayer g s = Player (State g) s

class (Monad m, GameState s) => GameAction m s where
    -- performs an action on a state to get to the next state (may be nondeterministic)
    runAction :: s -> Act s -> m s
    -- reads an action from stdin, wrapped in Maybe
    readAction :: (MonadIO m, Read (Act s)) => s -> m (Maybe (Act s))
    readAction _ = liftIO $ readMaybe <$> getLine

class (GameState s) => GameInfo s where
    -- gets info available to the current player
    getPlayerInfo :: s -> Info s
    -- shows info available to the current player
    showPlayerInfo :: (Show (Info s)) => s -> IO ()
    showPlayerInfo = liftIO . print . getPlayerInfo

-- class (MonadIO m, GameState s, GameAction m s, Read (Act s), GameInfo s, Show (Info s)) => HumanPlayer m s where
-- shows player the current available state info, prompts for an action, and then reads it from stdin
humanPlayer :: (MonadIO m, GameState s, GameAction m s, Read (Act s), GameInfo s, Show (Info s)) => Player m s
humanPlayer state = do
    liftIO $ showPlayerInfo state
    liftIO $ putStr "> "
    mAction <- readAction state
    case mAction of
        Nothing -> tryAgain state "ERROR: invalid input"
        Just action' -> do
            case validateAction state action' of
                Left err -> tryAgain state err
                Right action'' -> return action''
    where
        tryAgain state err = do
            liftIO $ putStrLn err
            humanPlayer state

data Game m s = Game {
    -- initial state of the game
    initialState :: m s,
    -- gets the player whose turn it is, given the current state
    getPlayer :: s -> Player m s
}

-- given state, runs the current player's chosen action to get to the next state
playRound :: (Monad m, GameAction m s) => Game m s -> s -> m s
playRound game state = do
    let player = getPlayer game state
    action <- player state
    runAction state action

-- plays repeated rounds until the game is in a terminal state
runGame :: (Monad m, GameState s, GameAction m s) => Game m s -> m s
runGame game = initialState game >>= iterateUntilM isTerminal (playRound game)

runGameIO :: (MonadIO m, GameState s, Show s, Show (Outcome s), GameAction m s) => Game m s -> m ()
runGameIO game = do
    state <- runGame game
    liftIO $ print state
    liftIO $ print $ outcome state

class Turn t where
    -- advances to the next turn
    nextTurn :: t -> t
    -- index of the current player whose turn it is
    curPlayerIdx :: t -> Int
    -- string showing whose turn it is (1-up indexed)
    showTurn :: t -> String
    showTurn turns = "Player " ++ show (curPlayerIdx turns + 1) ++ " turn"

-- with a fixed number of players, stores this number and the index of the current player's turn
data TurnRotating = TurnRot !Int !Int

instance Turn TurnRotating where
    nextTurn (TurnRot n i) = TurnRot n ((i + 1) `mod` n)
    curPlayerIdx (TurnRot _ i) = i

instance Show TurnRotating where
    show = showTurn

-- alternating turns between two players
newtype Turn2P = Turn2P Bool
    deriving Enum

instance Turn Turn2P where
    nextTurn (Turn2P b) = Turn2P $ not b
    curPlayerIdx (Turn2P b) = fromEnum b

instance Show Turn2P where
    show = showTurn