module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show

program = iter Statement.parse >-> buildProgram
buildProgram = Program

instance Parse T where
  parse = program
  toString = shw
             
exec (Program p) = Statement.exec p Dictionary.empty

shw :: T -> [Char]
shw (Program []) = " "
shw (Program (stmt:stmts)) = (Statement.toString stmt) ++ (shw $ Program stmts) 