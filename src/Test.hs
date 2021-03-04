{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test where

import Control.Monad.State
import Data.Proxy

-- maps states to actions
type family A s :: *

class (A s ~ a) => Action s a where
    readAction :: (Read a) => Proxy s -> IO a
    readAction _ = read <$> getLine

class (A s ~ a, Action s a) => Game s a where
    run :: (Read a) => s -> IO s
    run state = do
        putStr "> "
        action <- readAction (Proxy :: Proxy s)
        return state