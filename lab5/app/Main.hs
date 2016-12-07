module Main where

import           Control.Monad       (void, when)
import           Control.Monad.State (State (..), execState)
import           Data.Default        (def)
import           DiagramsApi
import           DrawingApi
import           GlossApi
import           GraphLib
import           System.Environment  (getArgs)

-- Test case
testG1 = fromEdgeList 15 [(i, i+1) | i <- [1 .. 14]]
testG2 = fromConnectivityMatrix [ [ 0, 1, 1, 1, 1 ]
                                , [ 1, 0, 1, 1, 1 ]
                                , [ 1, 1, 0, 1, 1 ]
                                , [ 1, 1, 1, 0, 1 ]
                                , [ 1, 1, 1, 1, 0 ]
                                ]

execDrawing :: DrawingApi api => State api () -> api -> IO ()
execDrawing st def = void $ doEventLoop $ execState st def

drawGraph :: DrawingApi api => Int -> [(Int, Int)] -> State api ()
drawGraph n edges = do
        setWindowSize 700 500
        setGraphSize n
        mapM_ drawNode [1..n]
        mapM_ drawEdge edges

main :: IO ()
main = do
    apiMethod:_ <- getArgs
    let (n, es) = testG2
    when (apiMethod == "gloss")    $ execDrawing (drawGraph n es) (def :: GlossState)
    when (apiMethod == "diagrams") $ execDrawing (drawGraph n es) (def :: DiagramsState)
