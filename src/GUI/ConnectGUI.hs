module GUI.ConnectGUI
        ( runGUI
        ) where

import           Game.Connect4
import qualified Game.Connect4                      as C4
import           Graphics.Gloss
import qualified Graphics.Gloss                     as G
import           Graphics.Gloss.Interface.Pure.Game
import           GUI.Rendering

runGUI :: IO ()
runGUI =
        G.play window
                background
                60
                initialGame
                renderGame
                eventHandler
                (const id)

window :: Display
window = InWindow "" (round screenWidth, round screenHeight) (200, 200)

background :: Color
background = makeColorI 40 42 54 255

eventHandler :: Event -> Game -> Game
eventHandler (EventKey (MouseButton LeftButton) Up _ (x, _)) g
        | clickedCell `elem` availableColumns (gBoard g) = C4.play clickedCell g
        | otherwise = g
  where
    clickedCell = ceiling $ ((x + screenWidth / 2) / cellWidth) - 1
eventHandler (EventKey (Char 'r') Up _ _) _ = initialGame
eventHandler _ g = g
