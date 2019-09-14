import Data.List
import Data.Array
--import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game

{- 
data Space = Space PosX PosY Symbol deriving (Show, Eq)
type Board = [[Space]]
data Symbol = B | W | N deriving (Show, Eq)

type PosX = Int 
type PosY = Int 

data GameState = Game { board :: Board , activePlayer :: Symbol}

initialGameState :: GameState
initialGameState = Game (gameInit 9) W

{- Pretty print the board -}
filler :: Int -> String
filler len = " +" ++ concat (replicate (len-1) "-----+") ++ "-----+ \n"

prRow :: [Space] -> String
prRow [] = " | \n"
prRow ((Space x y N) : xs) = " | " ++ show y ++ "," ++ show x ++ prRow xs
prRow ((Space _ _ s) : xs) = " |  " ++ show s ++ " " ++ prRow xs
 
printBoard :: Board -> String
printBoard b = (filler len) ++ 
    (concat $ intersperse (filler len) (prRow <$> b)) ++ (filler len)
    where len = length b

showBoard :: Board -> IO()
showBoard b = putStrLn $ printBoard b

{- Initialize the board -}
createSpace :: Int -> Int -> Space
createSpace x y = Space x y N

gameInit :: Int -> Board
gameInit n = fmap (\z -> row z) [0..n]
    where row x = fmap (\y -> createSpace x y) [0..n]

updateSpace :: Space -> Symbol -> Space
updateSpace (Space x y _) p = Space x y p 

{- Game mechanics -}
place :: PosX -> PosY -> GameState
place x y = undefined {- do
    (b, p) <- get 
    case p of 
        B -> put (updateSpace x y p, W)
        W -> put (updateSpace x y p, B)
        _ -> put (b, p)
    return GameState b p
-}
-}
data Game = Game { gameBoard :: Board,
                   gamePlayer :: Player,
                   gameState :: State } deriving (Eq, Show)
type Board = Array (Int, Int) Cell
data Player = PlayerW | PlayerB deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)
type Cell = Maybe Player 

n :: Int
n = 9

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
  pictures [wCellsOfBoard board,
            bCellsOfBoard board,
            boardGrid]
  
boardAsGameOverPicture winner board =
  color backgroundColor (boardAsPicture board)

screenHeight :: Int
screenHeight = 640

screenWidth :: Int
screenWidth = 640
 
initialGame = Game
              {gameBoard = array indexRange $
                           zip
                           (range indexRange)
                           (cycle [Nothing]),
              gamePlayer = PlayerW,
              gameState = Running}
                  where indexRange = ((0,0), (n-1, n-1))

gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                     (fromIntegral screenHeight * (-0.5))
                     frame
    where frame = case gameState game of
              Running -> boardAsRunningPicture (gameBoard game)
              GameOver winner -> boardAsPicture {-winner-} (gameBoard game)

switchPlayer game =
  case gamePlayer game of
    PlayerB -> game { gamePlayer = PlayerW }
    playerW -> game { gamePlayer = PlayerB }

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cord
  | isCordCorrect cord && board ! cord == Nothing =
    switchPlayer $ game { gameBoard = board // [(cord, Just player)]}
  | otherwise = game
    where isCordCorrect = inRange ((0,0) , (n-1,n-1))
          board = gameBoard game
          player = gamePlayer game
mousePosAsCell :: (Float, Float) -> (Int, Int)
mousePosAsCell (x,y) =
  (floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight),
   floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth))

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
  case gameState game of
    Running -> playerTurn game $ mousePosAsCell mousePos
    GameOver _ -> initialGame
transformGame _ game = game

window = InWindow "Functional" (screenWidth, screenHeight) (100,100)

backgroundColor = makeColorI 230 161 99 255
playerBColor = makeColorI 0 0 0 255
playerWColor = makeColorI 255 255 255 255

main :: IO()
main = play
  window
  backgroundColor
  30
  initialGame
  gameAsPicture
  transformGame
  (const id)
