import Data.List

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
    (concat $ intersperse (filler len) (prRow <$> b)) ++ 
    (filler len)
    where len = length b

showBoard :: Board -> IO()
showBoard b = putStrLn $ printBoard b

{- Initialize the board -}
createSpace :: Int -> Int -> Space
createSpace x y = Space x y N

gameInit :: Int -> Board
gameInit n = fmap (\z -> row z) [0..n]
    where row x = fmap (\y -> createSpace x y) [0..n]

{- Game mechanics -}
place :: GameState -> PosX -> PosY -> Maybe GameState
place g x y = undefined

main :: IO()
main = do putStrLn "go"
