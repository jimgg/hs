module Helpers.StringUtil
where

split :: Eq a => a -> [a] -> [[a]]

split _ [] = [[]]
split delim str = 
    let (before, remainder) = span (/= delim) str
    in
    before : case remainder of
        [] -> []
        x -> split delim (tail x)

