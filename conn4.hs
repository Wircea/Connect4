--data
type Turn = Int
type RowInput = Int

--width = 7
--height = 6
--board = replicate 6 $ replicate 7 (0::Int) --set board to 0
board = replicate (6*7) (0::Int)

data CurrentBoard = Board [Int] Int Int Int deriving Show --order is HEIGHT then WIDTH, followed by filled squares
data C4Command = Place Int | WhoAmI | Surrener deriving (Show, Read, Eq)
data GameOption = StartGame | TableSize Int Int | Exit deriving (Show, Read, Eq)


----implementation
--table printing
getTableSymbol :: Int -> [Char] --Legend
getTableSymbol 0 = "[]"
getTableSymbol 1 = "\x1b[36m██\x1b[0m"
getTableSymbol 2 = "\x1b[31m██\x1b[0m"

getTableValue :: CurrentBoard -> Int -> [Char] --determine what's printed
getTableValue (Board b h w f) x = getTableSymbol (b !!  x)

tableFooter :: Int -> Int -> [Char]
tableFooter w x | x == w-1 = if x<10 then "   " ++ show x else "  " ++ show x
tableFooter w x | x == 0 = (replicate (w*4) '-') ++ "\n" ++ show x ++ tableFooter w (x+1)
tableFooter w x | x < 11 = "   " ++ show x ++ tableFooter w (x+1) -- handler for printing one digit numbers
tableFooter w x = "  " ++ show x ++ tableFooter w (x+1)

parseTable :: CurrentBoard -> Int -> [Char] --formatting by row
parseTable (Board b h w f) x | x==(w*h-1) = getTableValue (Board b h w f) x ++ "\n" ++ tableFooter w 0
parseTable (Board b h w f) x | ((x `mod` w) == 0 && x/=0) = "\n\n" ++ getTableValue (Board b h w f) x ++ "  " ++ parseTable (Board b h w f) (x+1)
parseTable (Board b h w f) x = getTableValue (Board b h w f) x ++ "  " ++ parseTable (Board b h w f) (x+1)

showTable :: CurrentBoard -> [Char] --root
showTable (Board b h w f) = parseTable (Board b h w f) 0

--check if empty
getTableNumericValue :: CurrentBoard -> Int -> Int 
getTableNumericValue (Board b h w f) a = b!! a

pieceExists :: CurrentBoard -> Int -> Bool
pieceExists (Board b h w f) a =  if(getTableNumericValue (Board b h w f) a /= 0) then True else False

--win conditions
------case vertical
checkDOWN :: CurrentBoard -> Int -> Int -> Int -- board, player, position
checkDOWN (Board b h w f) p a | ((b!!a) == p) = if( (a + w) < h*w) then (1 + checkDOWN (Board b h w f) p (a + w)) else 1
checkDOWN (Board b h w f) p a = 0
------case horizontal
checkLEFT :: CurrentBoard -> Int -> Int -> Int -- board, player, position
checkLEFT (Board b h w f) p a | ((b!!a) == p) = if( (a - 1 ) >=0 && ( (a `div` w) == ((a - 1) `div` w)) ) then (1 + checkLEFT (Board b h w f) p (a - 1)) else 1
checkLEFT (Board b h w f) p a = 0

checkRIGHT :: CurrentBoard -> Int -> Int -> Int -- board, player, position
checkRIGHT (Board b h w f) p a | ((b!!a) == p) = if( (a + 1 ) < w*h && ( (a `div` w) == ((a + 1) `div` w)) ) then (1 + checkRIGHT (Board b h w f) p (a + 1)) else 1
checkRIGHT (Board b h w f) p a = 0
------case \
checkUPLEFT :: CurrentBoard -> Int -> Int -> Int -- board, player, position
checkUPLEFT (Board b h w f) p a | ((b!!a) == p) = if( (a - w - 1 ) >=0 && ( (a `div` w) == (((a - w - 1) `div` w)+1)) ) then (1 + checkUPLEFT (Board b h w f) p (a - w- 1)) else 1
checkUPLEFT (Board b h w f) p a = 0

checkDOWNRIGHT :: CurrentBoard -> Int -> Int -> Int -- board, player, position
checkDOWNRIGHT (Board b h w f) p a | ((b!!a) == p) = if( (a + w + 1 ) < w*h && ( (a `div` w) == (((a + w + 1) `div` w)-1)) ) then (1 + checkDOWNRIGHT (Board b h w f) p (a + w + 1)) else 1
checkDOWNRIGHT (Board b h w f) p a = 0
------case /
checkDOWNLEFT :: CurrentBoard -> Int -> Int -> Int -- board, player, position
checkDOWNLEFT (Board b h w f) p a | ((b!!a) == p) = if( (a + w - 1 ) < w*h && ( (a `div` w) == (((a + w - 1) `div` w)-1)) ) then (1 + checkDOWNLEFT (Board b h w f) p (a + w- 1)) else 1
checkDOWNLEFT (Board b h w f) p a = 0

checkUPRIGHT :: CurrentBoard -> Int -> Int -> Int -- board, player, position
checkUPRIGHT (Board b h w f) p a | ((b!!a) == p) = if( (a - w + 1 ) >= 0 && ( (a `div` w) == (((a - w + 1) `div` w)+1)) ) then (1 + checkUPRIGHT (Board b h w f) p (a - w + 1)) else 1
checkUPRIGHT (Board b h w f) p a = 0

checkWin :: CurrentBoard -> Int -> Int -> Bool -- board, player, position
checkWin (Board b h w f) p a = if (( (checkDOWN (Board b h w f) p a)) >=4 ) then True --vertical case. going UP does not make sense as the check will always be downwards 
                             else if ( - 1 + (checkLEFT(Board b h w f) p a) + (checkRIGHT(Board b h w f) p a) >=4 ) then True --the starting point will be counted twice. same below 
                             else if ( - 1 + (checkUPLEFT (Board b h w f) p a) + (checkDOWNRIGHT(Board b h w f) p a) >=4 ) then True
                             else if ( - 1 + (checkDOWNLEFT (Board b h w f) p a) + (checkUPRIGHT(Board b h w f) p a) >=4 ) then True
                             else False
--table altering
alterTable :: CurrentBoard -> Int -> Int -> Int -> [Int] -- board, player, pos of placement, current pos
alterTable (Board b h w f) p x a | a > h*w-1 = []
alterTable (Board b h w f) p x a | a == x = [p] ++ alterTable (Board b h w f) p x (a+1)
alterTable (Board b h w f) p x a = [b!! (a)] ++ alterTable (Board b h w f) p x (a+1)

newTable :: CurrentBoard -> Int -> Int -> CurrentBoard
newTable (Board b h w f) p x = (Board (alterTable (Board b h w f) p x 0 ) h w f)

--game flow
nextTurn :: Int -> Int
nextTurn 1 = 2
nextTurn 2 = 1

placementProcess :: CurrentBoard -> Int -> Int -> IO() -- board, player, position
placementProcess (Board b h w f) p x | x < 0 = do 
                                              putStrLn ("Invalid move")
                                              playerTurn (Board b h w f) p
placementProcess (Board b h w f) p x = if (pieceExists (Board b h w f) x == True) then  placementProcess (Board b h w f) p (x-w) else do
                                                                                                                                 putStrLn ("\ESC[2J")
                                                                                                                                 let newBoard = newTable (Board b h w (f+1)) p x
                                                                                                                                 putStrLn (showTable (newBoard))
                                                                                                                                 
                                                                                                                                 if(checkWin (newBoard) p x == True ) then putStrLn ("Player" ++ show p ++ "(" ++ (getTableSymbol p) ++ ") wins" )
                                                                                                                                 else if(f+1 >= w*h ) then putStrLn("The players have tied")
                                                                                                                                 else playerTurn newBoard (nextTurn p)

execCommand :: CurrentBoard -> Int  -> C4Command -> IO()
execCommand (Board b h w f) y (WhoAmI)  = do 
                                                     putStrLn ("You are Player" ++ (show y) ++ "(" ++ (getTableSymbol y) ++ ")")
                                                     playerTurn (Board b h w f) y
execCommand (Board b h w f) y (Place x) =if (x<0 || x>=w) then playerTurn (Board b h w f) y else
                                       do
                                       placementProcess (Board b h w f) y (w*(h-1)+x)
                                       
                                                 
playerTurn :: CurrentBoard -> Int -> IO()
playerTurn (Board b h w f) y = do
                          putStrLn (">")
                          input1 <- getLine
                          let comm = (read input1 :: C4Command) 
                          execCommand (Board b h w f) y comm


startGame:: CurrentBoard -> IO()
startGame (Board b h w f) = do
                          putStrLn (showTable (Board b h w f))
                          playerTurn (Board b h w f) 1

optionCheck:: CurrentBoard -> GameOption -> IO()
optionCheck (Board b h w f) StartGame = do 
                                       putStrLn "\ESC[2J"
                                       startGame(Board b h w f)
optionCheck (Board b h w f) (TableSize y x) = do
                                             changeBoardSize y x
optionCheck (Board b h w f) (Exit) = do
                                    exitProg


preGame:: CurrentBoard -> IO()
preGame (Board b h w f) = do
                         putStrLn (showTable (Board b h w f))
                         putStrLn "\nCommands: \n\n\t-StartGame \n\t-TableSize height width\n\t-Exit" 
                         input1 <- getLine
                         let comm = (read input1 :: GameOption)
                         optionCheck (Board b h w f) comm

----debug functions

changeBoardSize :: Int -> Int -> IO()
changeBoardSize a b | a>=4 && b>=4 = do
                       let w = b 
                       let h = a
                       let b = replicate (h * w) (0::Int)
                       putStr "\ESC[2J"
                       putStr ("\nTable dimensions set to [" ++ show h ++ "x" ++ show w ++ "].\n\n")
                       preGame(Board b h w 0)
                       return ()
changeBoardSize a b = changeBoardSize 6 7

exitProg :: IO()
exitProg = putStr "Exiting...\n"

main = do
       putStrLn "\ESC[2J"
       preGame (Board board 6 7 0)
       putStr "Successfully quit.\n"
       