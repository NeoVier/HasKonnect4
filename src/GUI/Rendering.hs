module GUI.Rendering
        ( renderGame
        , screenWidth
        , screenHeight
        , cellWidth
        , cellHeight
        ) where

import           Data.Maybe     (isNothing)
import           Game.Connect4
import           Graphics.Gloss

screenWidth :: Float
screenWidth = 600

screenHeight :: Float
screenHeight = 600

cellWidth :: Float
cellWidth = screenWidth / fromIntegral defaultWidth

cellHeight :: Float
cellHeight = screenHeight / fromIntegral defaultHeight

foreground :: Color
foreground = makeColorI 0 0 255 255

renderGame :: Game -> Picture
renderGame g =
        translate (-screenWidth / 2) (-screenHeight / 2) $
        pictures $ dividers g : renderBoard (gBoard g)

renderCell :: Maybe Player -> (Int, Int) -> Picture
renderCell Nothing  = const blank
renderCell (Just p) = color (colorPlayer p) . renderCircle

colorPlayer :: Player -> Color
colorPlayer Red    = makeColorI 255 0 0 255
colorPlayer Yellow = makeColorI 255 255 0 255

renderCircle :: (Int, Int) -> Picture
renderCircle (line, col) =
        translate
                (fromIntegral col * cellWidth + (cellWidth / 2))
                (fromIntegral (defaultHeight - line - 1) * cellHeight +
                 (cellHeight / 2)) $
        circleSolid diameter
  where
    diameter = diameterParam - 0.1 * diameterParam
    diameterParam = min (cellWidth / 2) (cellHeight / 2)

renderBoard :: Board -> [Picture]
renderBoard b =
        concat $
        zipWith (\line lineNum ->
                         zipWith
                                 (\player colNum ->
                                          renderCell player (lineNum, colNum))
                                 line
                                 [0 .. length (b !! lineNum)])
                b
                [0 .. length b]

dividers :: Game -> Picture
dividers Game {gWinner = w} =
        color (maybe foreground colorPlayer w) $
        pictures [horizontalDividers, verticalDividers]

verticalDividers :: Picture
verticalDividers =
        pictures $
        map (\x -> line [(0, x * cellHeight), (screenWidth, x * cellHeight)])
                [0,1 .. fromIntegral defaultHeight]

horizontalDividers :: Picture
horizontalDividers =
        pictures $
        map (\x -> line [(x * cellWidth, 0), (x * cellWidth, screenHeight)])
                [0,1 .. fromIntegral defaultWidth]
