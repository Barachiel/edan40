module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    While Expr.T Statement |
    Begin [Statement] |
    Read String |
    Write Expr.T |
    Comment [String]
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e
ifElse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfElse
buildIfElse ((e, s1), s2) = If e s1 s2
skip = accept "skip" # require ";" >-> buildSkip
buildSkip (st1, st2) = Skip
while = accept "while" -#  Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s
begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin (sList) = Begin sList
readS = accept "read" -# word #- require ";" >-> buildRead
buildRead w = Read w
writeS = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e
comment = accept "--" -# iter word #- newLine >-> buildComment
buildComment (stList) = Comment stList


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment v e: stmts) dict input =
    exec stmts newDict input
    where newDict = Dictionary.insert (v, Expr.value e dict) dict
exec (Read w: stmts) dict (i:inputs) =
    exec stmts newDict inputs
    where newDict = Dictionary.insert (w, i) dict
exec (Begin sList: stmts) dict input =
    exec (sList ++ stmts) dict input
exec ((While e stmt): stmts) dict input =
    if (Expr.value e dict) > 0
    then exec (stmt:(While e stmt):stmts) dict input
    else exec stmts dict input
exec (Skip : stmts) dict input =
    exec stmts dict input
exec (Write e: stmts) dict input =
    (Expr.value e dict) : exec stmts dict input
exec (Comment st2: stmts) dict input =
    exec stmts dict input

indent :: Int -> String
indent ind = concat (replicate ind " ")

--Used by toString to prettyPrint the code
shw :: Int -> Statement -> String
shw ind (Assignment v e) = (indent ind) ++ v ++ " := " ++ (Expr.toString e) ++ ";\n"
shw ind (If e s1 s2) = (indent ind) ++ "if " ++ (Expr.toString e) ++ " then\n" ++ (shw (ind+3) s1) ++ (indent ind) ++ "else\n" ++ (shw (ind+3) s2)
shw ind Skip = (indent ind) ++ "skip;\n"
shw ind (While e s) = (indent ind) ++ "while " ++ (Expr.toString e) ++ " do\n" ++ (shw (ind+3) s)
shw ind (Begin sList) = (indent ind) ++ "begin\n" ++ concat (map (shw (ind + 3)) sList) ++ "end\n"
shw ind (Read w) = (indent ind) ++ "read " ++ w ++ ";\n"
shw ind (Write e) = (indent ind) ++ "write " ++ (Expr.toString e) ++ ";\n"
shw ind (Comment stList) = (indent ind) ++ "--" ++ concat (mapExc insertSpace stList) ++ "\\n" ++ "\n"

insertSpace :: String -> String
insertSpace [] = " "
insertSpace (x:xs) = x:(insertSpace xs)

--map but excludes last element
mapExc :: (a -> a) -> [a] -> [a]
mapExc f [] = []
mapExc f [x] = [x]
mapExc f (x:xs) = f x : mapExc f xs

instance Parse Statement where
  parse = assignment ! ifElse ! skip ! while ! begin ! readS ! writeS ! comment
  toString = shw 0
