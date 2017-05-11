module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

-- The parser m -# n accepts the same input as m # n, but 
-- returns just the result from the n parser. 
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- The parser m #- n accepts the same input as m # n, but 
-- returns the result from the m parser.
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- spaces accepts any number of whitespace characters 
-- as defined by the Prelude function isSpace.
spaces :: Parser String
spaces = iter $ char ? isSpace

token :: Parser a -> Parser a
token m = m #- spaces

-- letter is a parser for a letter as defined by the 
-- Prelude function isAlpha.
letter :: Parser Char
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- The parser chars n accepts n characters.
chars :: Int -> Parser String
chars n
  | n == 0 = return []
  | otherwise = char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- The parser require w accepts the same string input 
-- as accept w but reports the missing string using 
-- err in case of failure.
require :: String -> Parser String
require w  = (accept w ! err ("Program error: expecting " ++ w))

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

