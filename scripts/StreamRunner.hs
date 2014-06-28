import StringUtil
import Control.Monad

-- parse {{{
fields :: String -> [String]
fields line = split '`' line

main = do
    str <- getContents
    run $ map fields $ lines str
-- }}}

-- run {{{
run :: [[String]] -> IO ()
run m = compareLines [] m

compareLines :: [String] -> [[String]] -> IO ()
compareLines [] [] = do
    putStr ""
compareLines [] (y:ys) = do
    compareLines y ys
compareLines x (y:ys) = do
    compareLine x y
    compareLines y ys
compareLines x [] = do
    compareLine x []
-- }}}

-- compare {{{
compareLine :: [String] -> [String] -> IO ()
compareLine xx@(x:xs) y = do
    if compareLine' xx y
        then putStrLn x
        else putStr ""

compareLine' :: [String] -> [String] -> Bool
compareLine' [] _ = False
compareLine' x@(x':xs') [] =
    if head xs' == "1" 
        then True
        else False
compareLine' x@(x':xs') y@(y':ys') =
    if x' == y'
        then False
        else if head xs' == "1"
            then True
            else False
-- }}}
