import Numeric
import Data.List
import System.IO
import System.Environment
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy.Char8 as L

transHex :: String -> [String]
transHex = reverse . foldr transHex' [[]]
transHex' x z@(y:ys) = 
    if length y == 2 then
        [x] : z
        else ((x:y):ys)
transHex' x [] = [[x]]

hex2Num = fst . (!! 0) . readHex

main = do
    args <- getArgs
    let rFileName = args !! 0
    let rawNum = read $ args !! 1

    -- let findMe = B.pack [0x53, 0x43]
    let findMe = B.pack $ map hex2Num $ transHex $ showHex rawNum ""

    handle <- openBinaryFile rFileName ReadMode
    content <- B.hGetContents handle
    let posList = B.findSubstrings findMe content
    hClose handle
    mapM_ putStrLn $ map (flip showHex "") posList
