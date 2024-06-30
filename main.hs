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

parseBool remaining boolVal = Right (JsonBool boolVal, remaining)
parseNull t = Right (JsonNull, t)
parseStr s =
    let 
        aux str acc = 
            case str of
            ('"': t) -> Right (acc, t)
            (x:t) ->  aux t (x : acc)
            [] -> Left "No Closing"
    in case aux s [] of
        Right (res, rem) -> Right (JsonString $ reverse res, rem)
        Left e -> Left e


stringToFloat str = read str :: Float

parseNum c = 
    let 
        aux c' acc isPastDecimal = 
            case c' of
            (h : t) | isDigit h -> aux t (h:acc) isPastDecimal
            (h : t) | h == '.' -> 
                if isPastDecimal
                then Left "Cannot have more than one decimal in a float"
                else aux t (h : acc) True 
            (h : _) | h == ',' || h == '}' || h == ']' -> Right (acc, c')
            [] -> Right (acc, [])
            _ -> Left "Invalid Number"
    in
        case aux c [] False of
        Right (res, rem) -> 
            Right (JsonNumber $ (stringToFloat . reverse) res, rem)
        Left e -> Left e

parseArr c =
    let 
        aux c acc =
            case c of
                (']':t) -> Right (acc, t)
                x -> case parse x of
                    Right (res, rem)-> 
                        case rem of 
                            (',':t) -> aux t (res : acc) 
                            (']':t) -> Right (res : acc, t)
                    Left e -> Left e
        in case aux c [] of
        Right (res, rem) -> Right(JsonArray $ reverse res, rem)
        Left e -> Left e
parseObject o = 
    let 
        aux c acc = 
            case c of
            [] -> Left "Unterminated Object"
            '}' : t -> Right (acc, t)
            x -> case parse x of
                Right (JsonString k, rem) -> 
                    case rem of 
                        (':': t) -> 
                            case parse t of 
                                Right (v, ',':'}':t) -> Left "Cannot end object with `,`"
                                Right (v, ',':t) -> aux t ((k,v) :acc)
                                Right (v, '}':t) -> Right ((k,v):acc, t)
                                Right (v, _) -> Left "Invalid Object Formatting"
                                Left e -> Left e
                Left e -> Left e
                _ -> Left "Need a string as key to Json Object"
    in case aux o [] of
        Right (res, rem) -> Right (JsonObject (reverse res), rem)
        Left e -> Left e

parse :: [Char] -> Either String (Json, [Char]) 
parse c = 
    case c of 
        ('"' : t) -> parseStr t
        ('[' : t) -> parseArr t
        ('{' : t) -> parseObject t
        ('n':'u':'l':'l' : t) -> parseNull t
        ('t':'r':'u':'e' : t) -> parseBool t True
        ('f':'a':'l':'s':'e' : t) -> parseBool t False 
        (h : _) | isDigit h || h == '.' -> parseNum c
        c -> Left ("Ouch " ++ show c)


main :: IO ()
main = do
    let parsed = parse $ filter (/= ' ') "{\"elements\":[90.0, \"hi\"], \"thing\": null, \"isHappy\": false}"
        in case parsed of
            Right (res, rem) -> print res
            Left e -> print e 
