import Data.List
import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game

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
screenHeight :: Int
screenHeight = 640
screenWidth :: Int
screenWidth = 480
initialGame = undefined
gameAsPicture = undefined
transformGame = undefined
window = InWindow "Functional" (screenWidth, screenHeight) (100,100)
backgroundColor = makeColor 0 0 0 255

main :: IO()
main = play
  window
  backgroundColor
  30
  initialGame
  gameAsPicture
  transformGame
  (const id)
