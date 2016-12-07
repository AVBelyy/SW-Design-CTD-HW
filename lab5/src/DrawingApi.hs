module DrawingApi (DrawingApi(..)) where

import           Control.Monad.State (State (..))

class DrawingApi s where
    setWindowSize :: Int -> Int -> State s ()
    setGraphSize  :: Int -> State s ()
    drawNode      :: Int -> State s Bool
    drawEdge      :: (Int, Int) -> State s Bool
    doEventLoop   :: s -> IO ()
