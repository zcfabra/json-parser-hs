module Main where
import Data.Char (isDigit)
import Data.Monoid
import Data.Foldable

data Json = 
    JsonString String 
    | JsonNumber Float 
    | JsonArray  [Json]
    | JsonObject  [(String, Json)]
    | JsonNull 
    | JsonBool Bool 
    deriving (Show, Eq)


{-
    We want to apply a bunch of (Char -> Bool) functions 
    So, we need an operation that takes one:
        (Char -> Bool) and another (Char -> Bool)
    And merges them into a single predicate 

    f :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
    This is basically a binary op on the same type of (Char -> Bool)
    Which makes it suitable to consider as a monoid

    The `->` in a function type is actually itself a type which is parameterized
    by an Input and an Output type

    (a -> b) is monoid only if the result type is a monoid
    Monoid b => Monoid (a -> b)

    In our case of (Char -> Bool), Bool does not satisfy Monoid

    But, Any and All are 2 Boolean 'wrappers' which do satisfy Monoid

    If we turn (Char -> Bool) into (Char -> Any), the function will then satisfy 
    Monoid

    Compose Any . (Char -> Bool)

    Can then use mappend to combine 2 Char -> Any operations
    But, this only works w/ 2 Monoid fns, want arbitrarily many

    We basically want to be able to fold together a list of predicates of type
    Char -> Any 

    fold :: (Foldable t, Monoid m) => t m -> m

    List is Foldable, so fold can take a list of Char -> Any Monoids and collapse
    them to a monoid
-} 

isValidTerminated = getAny . foldMap  (Any .) [( == ','), (=='}'), (==']')]

parseBool remaining boolVal = 
    case remaining of 
        (h:t) | isValidTerminated h  -> Right (JsonBool boolVal, remaining)
        x  -> 
            Left $ 
            "Invalid character following bool: "  
            ++ show boolVal 
            ++ " - `"
            ++ show x 
            ++ "`"

parseNull remaining = 
    case remaining of 
        (h:t) | isValidTerminated h -> Right (JsonNull, remaining)
        x -> Left $ "Invalid values following null: `" ++ show x ++ "`"
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

    let parsed = parse $ filter (\el -> el /= ' ' &&  el /= '\n') "{\"parentList\":[{\"elements\":[90.0, \"hi\"], \"thing\": null, \"isHappy\": false}]}"
        in case parsed of
            Right (res, rem) -> print res
            Left e -> print e 
