import Char

urldecode :: String -> String
urldecode xs =
    let len = length xs
    in hs_url_decode xs len

hs_url_decode :: String -> Int -> String
hs_url_decode [] _ = []
hs_url_decode (x:xs) len =
    let len' = len - 1
    in if x == '+'
        then ' ' : hs_url_decode xs len'
        else if x == '%' && len' >= 2 && isHexDigit (head xs) && isHexDigit (head (tail xs))
            then chr (hs_htoi xs) : hs_url_decode (drop 2 xs) (len' - 2)
            else x : hs_url_decode xs len'

hs_htoi :: String -> Int
hs_htoi (x:y:xs) =
    let c1 = strtolower x
        c2 = strtolower y
        value1 = xdigittonum c1 * 16
        value2 = xdigittonum c2
    in
        value1 + value2
    where strtolower c = if isUpper c then toLower c else c
          xdigittonum c = if c >= '0' && c <= '9'
                            then ord c - ord '0'
                            else ord c - ord 'a' + 10


main = do
    args <- getArgs
    let url = args !! 0
    let (pre, mid, post, match) = url =~ "url=([^&]+)&" :: (String,String,String,[String])
    case match of
        [] -> putStrLn "no match!"
        [xs] -> putStrLn $ urldecode xs
