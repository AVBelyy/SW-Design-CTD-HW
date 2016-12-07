{-# LANGUAGE RecordWildCards #-}

module DiagramsApi (DiagramsState) where

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.State             (get, modify)
import           Data.Default                    (Default (..))
import           Data.Maybe                      (maybe)
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude
import           DrawingApi                      (DrawingApi (..))
import qualified Graphics.UI.Gtk                 as GTK

data DiagramsState = DS { window :: (Int, Int)
                        , n      :: Maybe Int
                        , verts  :: [Diagram B]
                        , edges  :: [(Int, Int)]
                        }

instance Default DiagramsState where
    def = DS (700, 500) Nothing [] []

instance DrawingApi DiagramsState where
    setWindowSize w h = modify (\st -> st { window = (w, h) } )
    setGraphSize n = modify (\st -> st { n = Just n } )
    drawNode k =
        get >>= \DS{..} -> maybe (return False) (\n' ->
            modify (\st -> st { verts = makeNode n' k : verts }) >>
            return True) n
    drawEdge e =
        get >>= \DS{..} -> maybe (return False) (\n' ->
            modify (\st -> st { edges = e : edges }) >>
            return True) n
    doEventLoop DS{..} = do
        let (w', h') = window
        let (w, h) = (fromIntegral w', fromIntegral h')

        -- Ordinary GTK setup.
        GTK.initGUI
        win <- GTK.windowNew
        GTK.windowSetDefaultSize win w' h'
        GTK.windowSetPosition win GTK.WinPosCenter
        da <- GTK.drawingAreaNew
        win `GTK.containerAdd` da
        win `GTK.on` GTK.deleteEvent $ liftIO GTK.mainQuit >> return True

        -- Render the diagram on the drawing area and save the transformation.
        da `GTK.on` GTK.exposeEvent $ liftIO $ do
            dw <- GTK.widgetGetDrawWindow da
            let r = snd $ snd $ renderDiaT Cairo (CairoOptions "" (dims2D w h) PNG False)
                                                 (makeGraph (length verts) verts edges)
            GTK.renderWithDrawable dw r
            return True

        -- Run the Gtk main loop.
        GTK.widgetShowAll win
        GTK.mainGUI

makeGraph :: Int -> [Diagram B] -> [(Int, Int)] -> Diagram B
makeGraph n vs es = mconcat vs
                  # applyAll (map (uncurry (connectOutside' arrowOpts)) es)

makeNode :: Int -> Int -> Diagram B
makeNode n k = circle 1 # named k # fc green # moveTo nodePos where
    nodePos = polar2 (n' / 2, 2 * pi * k' / n')
    polar2 (r, ϕ) = p2 (r * cos ϕ, r * sin ϕ)
    n' = fromIntegral n
    k' = fromIntegral k

arrowOpts :: ArrowOpts Double
arrowOpts = with & gaps       .~ none
                 & headLength .~ none
