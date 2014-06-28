import StringUtil
import Control.Monad
import Text.Printf

-- parse {{{
fields :: String -> [String]
fields line = split ',' line

main = do
    str <- getContents
    run $ map struct $ map fields $ lines str
-- }}}

-- run {{{
run :: [Card] -> IO ()
run [] = putStr ""
run xx@(x:xs) = 
    let cr = CardResult {uid = 0 , lsum = 0, rsum = 0}
    in compareLines cr xx
-- }}}

-- struct {{{
data Card = Card {
    mid :: Int,
    transType :: Int,
    amount :: Double
} deriving Show

data CardResult = CardResult {
    uid :: Int,
    lsum :: Double,
    rsum :: Double
} deriving Show

struct :: [String] -> Card
struct [] = Card { mid=0, transType=0, amount=0 }
struct (x:xs) = Card { mid=mid', transType=transType', amount=amount' }
    where mid' = read x
          transType' = read (xs!!1)
          amount' = read (xs!!2)
-- }}}

-- compare {{{
compareLines :: CardResult -> [Card] -> IO ()
compareLines x [] = do
    compareLineLast x
compareLines x (y:ys) = do
    cr <- compareLine x y
    compareLines cr ys

compareLine :: CardResult -> Card -> IO CardResult
compareLine x y = do
    if uid x == mid y
        then if transType y == 100
                then let sum' = lsum x + amount y
                     in if sum' > rsum x
                        then return x { lsum = sum', rsum = sum'}
                        else return x { lsum = sum' }
                else return x { lsum = 0 }
        else do
            printCardResult x
            if transType y == 100
                then return CardResult { uid = mid y, lsum = amount y, rsum = amount y }
                else return CardResult { uid = mid y, lsum = 0, rsum = 0 }

printCardResult :: CardResult -> IO ()
printCardResult cr = do
    if rsum cr > 0
        then putStrLn $ show (uid cr) ++ "," ++  (printf "%.2f" (rsum cr))
        else putStr ""


compareLineLast :: CardResult -> IO ()
compareLineLast = printCardResult

-- }}}
