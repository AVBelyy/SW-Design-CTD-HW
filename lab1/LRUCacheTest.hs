{-# LANGUAGE FlexibleContexts #-}

import           LRUCache

import           Control.Monad       (forM_, guard)
import           Control.Monad.State (runStateT)
import qualified Data.Map            as Map
import           Data.Maybe          (isNothing)
import qualified Data.Sequence       as Seq

test1 = put "k1" 1 >> put "k2" 2 >> put "k3" 3 >> put "k4" 4 >> get "k1"

test2 = put "k1" 1 >> put "k2" 2 >> put "k3" 3 >> put "k1" 2 >> get "k1"

test3 = forM_ (reverse [0 .. 100000]) (\i -> put ("k" ++ show i) i) >> get "k3"

emptyState = (Map.empty, Seq.empty)

main :: IO ()
main = do
    (res1, (_, vars1)) <- runStateT test1 emptyState
    guard (isNothing res1)
    guard (vars1 == Seq.fromList ["k4", "k3", "k2"])
    (res2, (_, vars2)) <- runStateT test2 emptyState
    guard (res2 == Just 2)
    guard (vars2 == Seq.fromList ["k1", "k3", "k2"])
    (res3, (_, vars3)) <- runStateT test3 emptyState
    guard (isNothing res3)
    guard (vars3 == Seq.fromList ["k0", "k1", "k2"])
