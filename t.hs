import           Data.List

data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero
  deriving (Show,Eq)

-- newtype Number = Number [Digit]
type Number = [Digit]

digits = [Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine]
sdigits = "0123456789"

digitToChar :: Digit -> Char
digitToChar d = case lookup d $ zip digits sdigits of
                  Nothing -> error "Impossible"
                  (Just c) -> c

charToDigit :: Char -> Maybe Digit
charToDigit c = lookup c $ zip sdigits digits

strToNumber :: String -> [Digit]
strToNumber n = case traverse charToDigit n of
           Nothing -> []
           (Just x) -> reverse x

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop n $ cycle xs) xs

incd :: Digit -> Digit
incd Zero = One
incd One = Two
incd Two = Three
incd Three = Four
incd Four = Five
incd Five = Six
incd Six = Seven
incd Eight = Nine
incd Nine = Zero

decd :: Digit -> Digit
decd One = Zero
decd Two = One
decd Three = Two
decd Four = Three
decd Five = Four
decd Six = Five
decd Seven = Six
decd Eight = Seven
decd Nine = Eight
decd Zero = Nine

inc :: Number -> Number
inc [] = One : []
inc (digit:number) =  let d' = incd digit in
                        case d' of
                          Zero -> Zero:(inc number)
                          _ -> d':number

add :: Number -> Number -> Number
add n n' = case n of
  [Zero] -> n'
  _ -> inc $ (dec n) `add` n'

dec :: Number -> Number
dec [Zero] = [Zero]
dec [Zero,One] = [Nine]
dec (d : number) = let d' = decd d in
                     case d' of
                       Nine -> d' : dec number
                       _ -> d' : number

mul :: Number -> Number -> Number
mul n n' = case n of
  [Zero] -> [Zero]
  [One] -> n'
  _ ->  n' `add`  mul (dec n) n'

numberToStr :: Number -> String
numberToStr = reverse. map digitToChar

