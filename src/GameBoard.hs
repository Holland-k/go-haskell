module GameBoard where

import Graphics.Gloss.Interface.Pure.Game
import Data.Array

type Board = Array (Int, Int) Cell

type Cell = Maybe Player
data Player = PlayerW | PlayerB deriving (Eq, Show)

n :: Int
n = 9

backgroundColor = makeColorI 230 161 99 255
playerBColor = makeColorI 0 0 0 255
playerWColor = makeColorI 255 255 255 255

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

snapPictureToCell picture (row, column) = translate x y picture
  where x = fromIntegral column * cellWidth + cellWidth * 0.5
        y = fromIntegral row * cellHeight + cellHeight * 0.5

boardGrid :: Picture
boardGrid =
  pictures $
  concatMap (\i -> [ line [ (i*cellWidth, 0.0),
                            (i*cellWidth, fromIntegral screenHeight)]
                   , line [ (0.0, i * cellHeight),
                            (fromIntegral screenWidth, i * cellHeight)]
                   ])
  [0.0 .. fromIntegral n]

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
  pictures $
  map (snapPictureToCell cellPicture . fst) $
  filter (\(_, e) -> e == cell) $
  assocs board

pPiece :: Picture
pPiece = circleSolid (cellWidth * 0.3)

wCellsOfBoard :: Board -> Picture
wCellsOfBoard board = cellsOfBoard board (Just PlayerW) pPiece

bCellsOfBoard :: Board -> Picture
bCellsOfBoard board = cellsOfBoard board (Just PlayerB) pPiece

boardAsRunningPicture board =
  pictures [ color playerWColor $ wCellsOfBoard board,
             color playerBColor $ bCellsOfBoard board,
             boardGrid
           ]

boardAsPicture board =
  pictures [ wCellsOfBoard board,
             bCellsOfBoard board,
             boardGrid
           ]
  
boardAsGameOverPicture winner board =
  color backgroundColor (boardAsPicture board)

screenHeight :: Int
screenHeight = 640

screenWidth :: Int
screenWidth = 640
