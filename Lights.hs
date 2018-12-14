module Lights where
import System.Random
import Data.List
import Control.Monad

data Color = Red | Green | Blue | Yellow | Orange | Purple | Black deriving (Show, Enum, Bounded)

instance Eq Color where
  (==) Red Red = True
  (==) Green Green = True
  (==) Blue Blue = True
  (==) Yellow Yellow = True
  (==) Orange Orange = True
  (==) Purple Purple = True
  (==) Black Black = True
  _ == _ = False 

instance Random Color where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

iniGame :: Int -> IO ()
iniGame run = do
  let g = mkStdGen run
  let ans = [(randoms g :: [Color]) !! 0, (randoms g :: [Color]) !! 1, (randoms g :: [Color]) !! 2, (randoms g :: [Color]) !! 3, (randoms g :: [Color]) !! 4]
  putStrLn "New game started."
  runGame ans run

runGame :: [Color] -> Int -> IO ()
runGame ans run = do
  putStrLn "Please enter your guess: (separate each color with space)"
  line <- getLine
  let listW = (words line :: [String])
  let listC = (map strCol listW :: [Color])
--  print ans
  if length listC /= 5 then do
         putStrLn "Please choose 5 colors and separate them with space."
         runGame ans run
  else if any (Black ==) listC then do
         putStrLn "Please choose your colors from Red, Green, Blue, Yellow, Orange and Purple with the exact spelling."
         runGame ans run
  else if numCorr ans listC 0 0 == 5 then do
           putStrLn "Congratulations, you won!"
           putStrLn "Initializing new game."
           let run1 = run * 2
           iniGame run1
      else do
           let nc = numCorr ans listC 0 0
           let nmp = numMP ans listC 0 0
           putStrLn $ "Number of correct colors: " ++ (show nc)
           putStrLn $ "Number of miss-passed colors: " ++ (show nmp)
           runGame ans run


numMP :: [Color] -> [Color] -> Int -> Int -> Int
numMP ans listC ind num =
  if ind > 4 then do
    num
  else if (listC !! ind `elem` ans) && (listC !! ind /= ans !! ind)
         then do
           let ind1 = ind +1
           let num1 = num + 1
           numMP ans listC ind1 num1 
       else do
         let ind1 = ind +1
         numMP ans listC ind1 num
  

numCorr :: [Color] -> [Color] -> Int -> Int -> Int
numCorr ans listC ind num = 
  if ind > 4 then do
    num
  else if ans !! ind == listC !! ind
         then do
           let ind1 = ind +1
           let num1 = num + 1
           numCorr ans listC ind1 num1
       else do 
         let ind1 = ind +1
         numCorr ans listC ind1 num


strCol :: String -> Color
strCol str
  | str == "Red" = Red
  | str == "Green" = Green
  | str == "Blue" = Blue
  | str == "Yellow" = Yellow
  | str == "Orange" = Orange
  | str == "Purple" = Purple
  | otherwise = Black

main :: IO ()
main = do
  putStrLn "Welcome to 'Lights'"
  putStrLn "Guess the color for each light in a sequence of 5 lights."
  putStrLn "Lights are of color Red, Green, Blue, Yellow, Orange or Purple."
  putStrLn "Duplicates are allowed." 
  iniGame 100

