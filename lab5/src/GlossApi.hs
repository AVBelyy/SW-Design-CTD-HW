{-# LANGUAGE RecordWildCards #-}

module GlossApi (GlossState) where

import           Control.Monad.State (get, modify)
import           Data.Default        (Default (..))
import           Data.Maybe          (maybe)
import           DrawingApi          (DrawingApi (..))
import           Graphics.Gloss

data GlossState = GS { screen :: (Int, Int)
                     , window :: Display
                     , n      :: Maybe Int
                     , pics   :: [Picture]
                     }

instance Default GlossState where
    def = GS (1366, 768) (InWindow "" (700, 500) (0, 0)) Nothing []

instance DrawingApi GlossState where
    setWindowSize w h = modify (\st ->
        let (x0, y0) = ((fst (screen st) - w) `div` 2, (snd (screen st) - h) `div` 2) in
            st { window = InWindow "Graph Visualization" (w, h) (x0, y0) } )
    setGraphSize n = modify (\st -> st { n = Just n } )
    drawNode k =
        get >>= \GS{..} -> maybe (return False) (\n' ->
            modify (\st -> st { pics = makeNode n' k : pics }) >>
            return True) n
    drawEdge (i, j) =
        get >>= \GS{..} -> maybe (return False) (\n' ->
            modify (\st -> st { pics = makeEdge n' i j : pics }) >>
            return True) n
    doEventLoop GS{..} = display window background (pictures pics)

background :: Color
background = white

makeNode :: Int -> Int -> Picture
makeNode n k = tr (nodePos n k) (color green (circleSolid scaleS)) where
    tr = uncurry translate
    scaleS =  400 / fromIntegral n

makeEdge :: Int -> Int -> Int -> Picture
makeEdge n i j = line [ nodePos n i
                      , nodePos n j
                      ]

nodePos :: Int -> Int -> (Float, Float)
nodePos n k = polar2 (n' / 2, 2 * pi * k' / n') where
                 n' = fromIntegral n
                 k' = fromIntegral k
                 polar2 (r, ϕ) = (r * scaleR * cos ϕ, r * scaleR * sin ϕ)
                 scaleR = 400 / n'
