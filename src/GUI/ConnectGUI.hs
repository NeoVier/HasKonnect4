module GUI.ConnectGUI
        ( runGUI
        ) where

import           Graphics.Gloss

runGUI :: IO ()
runGUI = display window background blank

window :: Display
window = InWindow "" (600, 600) (200, 200)

background :: Color
background = makeColorI 40 42 54 0
