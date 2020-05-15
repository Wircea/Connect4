--data
type Turn = Int
type RowInput = Int

--width = 7
--height = 6
--board = replicate 6 $ replicate 7 (0::Int) --set board to 0
board = replicate (6*7) (0::Int)

data CurrentBoard = Board [Int] Int Int deriving Show --order is HEIGHT then WIDTH
data C4Command = Place Int | WhoAmI | Surrener deriving (Show, Read, Eq)
data GameOption = StartGame | TableSize Int Int | Exit deriving (Show, Read, Eq)


----implementation
--table printing
getTableSymbol :: Int -> [Char] --Legend
getTableSymbol 0 = "[]"
getTableSymbol 1 = "\x1b[36m██\x1b[0m"
getTableSymbol 2 = "\x1b[31m██\x1b[0m"

getTableValue :: CurrentBoard -> Int -> [Char] --determine what's printed
getTableValue (Board b h w) x = getTableSymbol (b !!  x)

tableFooter :: Int -> Int -> [Char]
tableFooter w x | x == w-1 = if x<10 then "   " ++ show x else "  " ++ show x
tableFooter w x | x == 0 = (replicate (w*4) '-') ++ "\n" ++ show x ++ tableFooter w (x+1)
tableFooter w x | x < 11 = "   " ++ show x ++ tableFooter w (x+1) -- handler for printing one digit numbers
tableFooter w x = "  " ++ show x ++ tableFooter w (x+1)

parseTable :: CurrentBoard -> Int -> [Char] --formatting by row
parseTable (Board b h w) x | x==(w*h-1) = getTableValue (Board b h w) x ++ "\n" ++ tableFooter w 0
parseTable (Board b h w) x | ((x `mod` w) == 0 && x/=0) = "\n\n" ++ getTableValue (Board b h w) x ++ "  " ++ parseTable (Board b h w) (x+1)
parseTable (Board b h w) x = getTableValue (Board b h w) x ++ "  " ++ parseTable (Board b h w) (x+1)

showTable :: CurrentBoard -> [Char] --root
showTable (Board b h w) = parseTable (Board b h w) 0

--check if empty
getTableNumericValue :: CurrentBoard -> Int -> Int 
getTableNumericValue (Board b h w) a = b!! a

pieceExists :: CurrentBoard -> Int -> Bool
pieceExists (Board b h w) a =  if(getTableNumericValue (Board b h w) a /= 0) then True else False

--win conditions


checkDOWN :: CurrentBoard -> Int -> Int -> Int -- board, player, position
checkDOWN (Board b h w) p a | ((b!!a) == p) = if( (a + w) < h*w) then (1 + checkDOWN (Board b h w) p (a + w)) else 0
checkDOWN (Board b h w) p a = 0

checkWin :: CurrentBoard -> Int -> Int -> Bool -- board, player, position
checkWin (Board b h w) p a = if ((1 + (checkDOWN (Board b h w) p a)) >=4 ) then True --vertical case. going UP does not make sense as the check will always be downwards 
                             else False

--table altering
alterTable :: CurrentBoard -> Int -> Int -> Int -> [Int] -- board, player, pos of placement, current pos
alterTable (Board b h w) p x a | a > h*w-1 = []
alterTable (Board b h w) p x a | a == x = [p] ++ alterTable (Board b h w) p x (a+1)
alterTable (Board b h w) p x a = [b!! (a)] ++ alterTable (Board b h w) p x (a+1)

newTable :: CurrentBoard -> Int -> Int -> CurrentBoard
newTable (Board b h w) p x = (Board (alterTable (Board b h w) p x 0 ) h w)

--game flow
nextTurn :: Int -> Int
nextTurn 1 = 2
nextTurn 2 = 1

placementProcess :: CurrentBoard -> Int -> Int -> IO() -- board, player, position
placementProcess (Board b h w) p x | x < 0 = do 
                                              putStrLn ("Invalid move")
                                              playerTurn (Board b h w) p
placementProcess (Board b h w) p x = if (pieceExists (Board b h w) x == True) then  placementProcess (Board b h w) p (x-w) else do
                                                                                                                                 putStrLn ("\ESC[2J")
                                                                                                                                 let newBoard = newTable (Board b h w) p x
                                                                                                                                 putStrLn (showTable (newBoard))
                                                                                                                                 
                                                                                                                                 if(checkWin (newBoard) p x == True ) then putStrLn ("Player" ++ show p ++ "(" ++ (getTableSymbol p) ++ ") wins" )
                                                                                                                                 else playerTurn newBoard (nextTurn p)

execCommand :: CurrentBoard -> Int  -> C4Command -> IO()
execCommand (Board b h w) y (WhoAmI)  = do 
                                                     putStrLn ("You are Player" ++ (show y) ++ "(" ++ (getTableSymbol y) ++ ")")
                                                     playerTurn (Board b h w) y
execCommand (Board b h w) y (Place x) =if (x<0 || x>=w) then playerTurn (Board b h w) y else
                                       do
                                       placementProcess (Board b h w) y (w*(h-1)+x)
                                       
                                                 
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

optionCheck:: CurrentBoard -> GameOption -> IO()
optionCheck (Board b h w) StartGame = do 
                                       putStrLn "\ESC[2J"
                                       startGame(Board b h w)
optionCheck (Board b h w) (TableSize y x) = do
                                             changeBoardSize y x
optionCheck (Board b h w) (Exit) = do
                                    exitProg


preGame:: CurrentBoard -> IO()
preGame (Board b h w) = do
                         putStrLn (showTable (Board b h w))
                         putStrLn "\nCommands: \n\n\t-StartGame \n\t-TableSize height width\n\t-Exit" 
                         input1 <- getLine
                         let comm = (read input1 :: GameOption)
                         optionCheck (Board b h w) comm

----debug functions

changeBoardSize :: Int -> Int -> IO()
changeBoardSize a b | a>=4 && b>=4 = do
                       let w = b 
                       let h = a
                       let b = replicate (h * w) (0::Int)
                       putStr "\ESC[2J"
                       putStr ("\nTable dimensions set to [" ++ show h ++ "x" ++ show w ++ "].\n\n")
                       preGame(Board b h w)
                       return ()
changeBoardSize a b = changeBoardSize 6 7

exitProg :: IO()
exitProg = putStr "Exiting...\n"

main = do
       putStrLn "\ESC[2J"
       preGame (Board board 6 7)
       putStr "Successfully quit.\n"
       