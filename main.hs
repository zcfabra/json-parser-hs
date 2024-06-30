module Main where
import Data.Char (isDigit)

data Json = 
    JsonString String 
    | JsonNumber Float 
    | JsonArray  [Json]
    | JsonObject  [(String, Json)]
    | JsonNull 
    | JsonBool Bool 
    deriving (Show, Eq)

parseBool remaining boolVal = 
    Left (JsonBool boolVal, remaining)
parseNull t = Left (JsonNull, t)
parseStr s =
    let 
        aux str acc = 
            case str of
            ('"': t) -> Left (acc, t)
            (x:t) ->  aux t (x : acc)
            [] -> Right "No Closing"
    in case aux s [] of
        Left (res, rem) -> Left (JsonString $ reverse res, rem)
        Right e -> Right e


stringToFloat str = read str :: Float

parseNum c = 
    let 
        aux c' acc isPastDecimal = 
            case c' of
            (h : t) | isDigit h -> aux t (h:acc) isPastDecimal
            (h : t) | h == '.' -> 
                if isPastDecimal
                then Right "Cannot have more than one decimal in a float"
                else aux t (h : acc) True 
            (h : _) | h == ',' || h == '}' || h == ']' -> Left (acc, c')
            [] -> Left (acc, [])
            _ -> Right "Invalid Number"
    in
        case aux c [] False of
        Left (res, rem) -> 
            Left (JsonNumber $ (stringToFloat . reverse) res, rem)
        Right e -> Right e

parseArr c =
    let 
        aux c acc =
            case c of
                (']':t) -> Left (acc, t)
                x -> case parse x of
                    Left (res, rem)-> 
                        case rem of 
                            (',':t) -> aux t (res : acc) 
                            (']':t) -> Left (res : acc, t)
                    Right e -> Right e
        in case aux c [] of
        Left (res, rem) -> Left(JsonArray $ reverse res, rem)
        Right e -> Right e
parseObject o = 
    let 
        aux c acc = 
            case c of
            [] -> Right "Unterminated Object"
            '}' : t -> Left (acc, t)
            x -> case parse x of
                Left (JsonString k, rem) -> 
                    case rem of 
                        (':': t) -> 
                            case parse t of 
                                Left (v, ',':'}':t) -> Right "Cannot end object with `,`"
                                Left (v, ',':t) -> aux t ((k,v) :acc)
                                Left (v, '}':t) -> Left ((k,v):acc, t)
                                Left (v, _) -> Right "Invalid Object Formatting"
                Right e -> Right e
                _ -> Right "Need a string as key to Json Object"
    in case aux o [] of
        Left (res, rem) -> Left (JsonObject (reverse res), rem)
        Right e -> Right e

parse :: [Char] -> Either (Json, [Char]) String
parse c = 
    case c of 
        ('"' : t) -> parseStr t
        ('[' : t) -> parseArr t
        ('{' : t) -> parseObject t
        ('n':'u':'l':'l' : t) -> parseNull t
        ('t':'r':'u':'e' : t) -> parseBool t True
        ('f':'a':'l':'s':'e' : t) -> parseBool t False 
        (h : _) | isDigit h || h == '.' -> parseNum c
        c -> Right ("Ouch " ++ show c)


main :: IO ()
main = do
    let parsed = parse $ filter (/= ' ') "{\"elements\":[90.0, \"hi\"], \"thing\": null, \"isHappy\": false}"
        in case parsed of
            Left (res, rem) -> print res
            Right e -> print e 
