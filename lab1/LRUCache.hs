{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LRUCache
  ( get
  , put
  , LRUState(..)
  ) where

import           Control.Applicative (Alternative)
import           Control.Monad       (guard)
import qualified Control.Monad.State as State
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import qualified Data.Sequence       as Seq

cacheThreshold = 3 -- for testing purposes

type LRUState k v m = State.MonadState (Map.Map k v, Seq.Seq k) m

get
    :: (LRUState k v m, Alternative m, Ord k)
    => k -> m (Maybe v)
get k = do
    (map, seq) <- State.get
    let maybeMapVal = k `Map.lookup` map
    case maybeMapVal of
        Nothing -> return Nothing
        Just v -> do
            let maybeSeqPos = Seq.findIndexL (== k) seq
            guard (isJust maybeSeqPos)
            let Just seqPos = maybeSeqPos
            let seq' = k Seq.<| (seqPos `Seq.deleteAt` seq)
            guard (Seq.length seq == Seq.length seq')
            State.put (map, seq')
            return (Just v)

put
    :: (LRUState k v m, Alternative m, Ord k)
    => k -> v -> m ()
put k v = do
    (map, seq) <- State.get
    guard (Map.size map == Seq.length seq)
    case compare (Map.size map) cacheThreshold of
        LT -> State.put (map, seq)
        _ -> do
            let maybeKLru = (Seq.length seq - 1) `Seq.lookup` seq
            guard (isJust maybeKLru)
            let Just kLru = maybeKLru
            let map' = Map.delete kLru map
            let seq' = (Seq.length seq - 1) `Seq.deleteAt` seq
            guard (Map.size map' < cacheThreshold)
            guard (Seq.length seq' < cacheThreshold)
            State.put (map', seq')
    (map'', seq'') <- State.get
    State.put (Map.insert k v map'', k Seq.<| seq'')
