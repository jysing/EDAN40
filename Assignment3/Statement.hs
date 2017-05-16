module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip;" >-> buildSkip
buildSkip s = Skip

begin = accept "begin" -# iter Expr.parse #- require "end" >-> buildBegin 
buildBegin = Begin

ifStmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf 
buildIf ((e, s1), s2) = If e s1 s2

whileStmt = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readStmt = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read 

writeStmt = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write  

commentStmt = accept "--" -# comment #- require "\n" >-> buildComment
buildComment s = Comment s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment v e : stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Begin stmts:stmtss) dict input = exec (stmts++stmtss) dict input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond stmt:stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (stmt:(While cond stmt:stmts)) dict input
    else exec stmts dict input
exec (Read e : stmts) dict input = exec stmts (Dictionary.insert (e, head input) dict) $ tail input
exec (Write e : stmts) dict input = (Expr.value e dict) : exec stmts dict input
exec (Comment str : stmts) dict input = exec stmts dict input

shw :: Statement -> String
shw (Assignment s e) = s ++ " := " ++ Expr.toString e ++ ";\n"
shw (Skip) = "skip;\n"
shw (Begin stmts) = "begin\n" ++ concat (map shw stmts) ++ "end\n"
shw (If cond stmt1 stmt2) = "if " ++ Expr.toString cond ++ " then\n" ++ shw stmt1 ++ "else\n" ++ shw stmt2
shw (While cond stmts) = "while " ++ Expr.toString cond ++ " do\n" ++ shw stmts
shw (Read str) = "read " ++ str ++ ";\n"
shw (Write e) = "write " ++ Expr.toString e ++ ";\n"
shw (Comment str) = "-- " ++ str ++ "\n"

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifStmt ! whileStmt ! readStmt ! writeStmt ! commentStmt
  toString = shw
