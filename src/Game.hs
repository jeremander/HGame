{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game where

import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State (liftIO, MonadIO, State)
import Data.Char (toLower)
import Data.List (isPrefixOf)

-- type Player m s = s -> m s
-- type HumanPlayer s = Player IO s
-- type AIPlayer s g = Player (State g) s

class GameState s where
    isTerminal :: s -> Bool
    -- runGame :: (Monad m) => Player m s -> s -> m s
    -- runGame = iterateUntilM isTerminal
    -- playRound :: s -> IO s
    -- playRound state = do
    --     putStrLn $ show state
    --     putStr "> "
    --     line <- getLine
    --     return $ read line
    -- play :: s -> IO ()
    -- play initial = do
    --     runGame playRound initial
    --     return ()

-- maps from state type to action type
type family A s :: *

type Player m s = s -> m (A s)
type HumanPlayer s = Player IO s
type AIPlayer g s = Player (State g) s

class (Monad m, GameState s, A s ~ a) => GameAction m s a where
    runAction :: s -> a -> m s
    readAction :: (MonadIO m, Read a) => s -> m a
    readAction _ = liftIO $ read <$> getLine
-- class (Monad m, GameState s) => GameAction m s a where
    -- runAction :: s -> a -> m s
    -- readAction :: (MonadIO m, Read a) => m a
    -- readAction = liftIO $ read <$> getLine
    -- readAction player = liftIO $ player ()

class (Monad m, Show s, GameState s, GameAction m s a) => Game m s a where
    playRound :: Player m s -> s -> m s
    playRound player state = player state >>= runAction state
    playRoundIO :: (MonadIO m, Read a) => s -> m s
    playRoundIO state = do
        liftIO $ putStrLn $ show state
        liftIO $ putStr "> "
        action <- readAction state
        runAction state action
    runGame :: Player m s -> s -> m s
    runGame player = iterateUntilM isTerminal (playRound player)
    playIO :: (MonadIO m, Read a) => s -> m s
    playIO = runGame readAction
    -- TODO: fix


newtype Str = Str String
    deriving Show

instance Read Str where
    readsPrec _ s = [(Str s, "")]

instance GameState Str where
    isTerminal (Str s) = "q" `isPrefixOf` (toLower <$> s)

type instance A Str = Str

instance GameAction IO Str Str where
    runAction state action = return action

instance Game IO Str Str