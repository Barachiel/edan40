module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T]

--Generate a Program from the iterated statements
--map toString on statements and concat the result
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) = concatMap Statement.toString stmts

--Calls Statement.exec for each statement contained in T
exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input
