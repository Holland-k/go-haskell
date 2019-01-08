import Data.List

data Space = Space PosX PosY Symbol
type Board = [[Space]]
data Symbol = B | W | N deriving (Show, Eq)

type PosX = Int 
type PosY = Int 

{- Pretty print the board -}
filler :: Int -> String
filler len = " +" ++ concat (replicate (len-1) "-----+") ++ "-----+ \n"

prRow :: [Space] -> String
prRow [] = " | \n"
prRow ((Space x y N) : xs) = " | " ++ show x ++ "," ++ show y ++ prRow xs
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

game_init :: Int -> Board
game_init n = fmap (\z -> row z) [0..n]
    where row x = fmap (\y -> createSpace x y) [0..n]

{- Game mechanics -}
place :: PosX -> PosY -> Maybe Space
place x y = undefined

main :: IO()
main = do putStrLn "go"
