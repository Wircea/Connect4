--data
type Turn = Int
type RowInput = Int

--width = 7
--height = 6
board = replicate 6 $ replicate 7 (0::Int) --set board to 0

data CurrentBoard = Board [[Int]] Int Int deriving Show --order is HEIGHT then WIDTH
data C4Command = Place Int | WhoAmI | Surrener deriving (Show, Read, Eq)


----implementation
--table printing
getTableSymbol :: Int -> [Char] --Legend
getTableSymbol 0 = "[]"
getTableSymbol 1 = "██"
getTableSymbol 2 = "▒▒"

getTableValue :: CurrentBoard -> Int -> [Char] --determine what's printed
getTableValue (Board b h w) x = getTableSymbol (b!!  (x `div` w)!! (x `mod` w))

tableFooter :: Int -> Int -> [Char]
tableFooter w x | x == w-1 = if x<10 then "   " ++ show x else "  " ++ show x
tableFooter w x | x == 0 = (replicate (w*4) '-') ++ "\n" ++ show x ++ tableFooter w (x+1)
tableFooter w x | x < 11 = "   " ++ show x ++ tableFooter w (x+1)
tableFooter w x = "  " ++ show x ++ tableFooter w (x+1)

parseTable :: CurrentBoard -> Int -> [Char] --formatting by row
parseTable (Board b h w) x | x==(w*h-1) = getTableValue (Board b h w) x ++ "\n" ++ tableFooter w 0
parseTable (Board b h w) x | ((x `mod` w) == 0 && x/=0) = "\n\n" ++ getTableValue (Board b h w) x ++ "  " ++ parseTable (Board b h w) (x+1)
parseTable (Board b h w) x = getTableValue (Board b h w) x ++ "  " ++ parseTable (Board b h w) (x+1)

showTable :: CurrentBoard -> [Char] --root
showTable (Board b h w) = parseTable (Board b h w) 0

--check if empty
getTableNumericValue :: Int -> Int -> Int
getTableNumericValue y x = board!!y !!x

pieceExists :: Int->Int->Bool
pieceExists y x =  if(getTableNumericValue y x /= 0) then True else False

--changeIntoToken :: Int -> Int -> Int -> IO()
--changeIntoToken y x b = do
                        --board = replicate height $ replicate width (1::Int)
--                        putStrLn (showTable)


execCommand :: CurrentBoard -> Int  -> C4Command -> IO()
execCommand (Board b h w) y comm | comm == WhoAmI  = do 
                                                     putStrLn ("You are Player" ++ (show y))
                                                     playerTurn (Board b h w) y
                                                 
playerTurn :: CurrentBoard -> Int -> IO()
playerTurn (Board b h w) y = do
                          putStrLn (">")
                          input1 <- getLine
                          let comm = (read input1 :: C4Command) 
                          execCommand (Board b h w) y comm


startGame:: CurrentBoard -> IO()
startGame (Board b h w) = do
                          putStrLn (showTable (Board b h w))
                          playerTurn (Board b h w) 1
----debug functions

changeBoardSize :: Int -> Int -> IO()
changeBoardSize a b | a>=4 && b>=4 = do
                       let w = b 
                       let h = a
                       let b = replicate h $ replicate w (0::Int)
                       putStr "\ESC[2J"
                       putStr ("\nTable dimensions set to [" ++ show h ++ "x" ++ show w ++ "].\n\n")
                       startGame(Board b h w)
                       return ()
changeBoardSize a b = changeBoardSize 6 7

main = do
       startGame (Board board 6 7)
       
       