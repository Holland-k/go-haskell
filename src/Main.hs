import Data.List
import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import GameBoard

data Game = Game { gameBoard :: Board,
                   gamePlayer :: Player,
                   gameState :: State } deriving (Eq, Show)

data State = Running | GameOver (Maybe Player) deriving (Eq, Show)
 
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

main :: IO()
main = play
  window
  backgroundColor
  30
  initialGame
  gameAsPicture
  transformGame
  (const id)
