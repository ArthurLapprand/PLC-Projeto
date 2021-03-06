module Value (Value (..)) where
import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
	  | GlobalVar
	  | Double Double
	  | Break (Maybe Id)
	  | Function Id [Id] [Statement]
	  | Return Value
	  | List [Value]
	  | Empty
	deriving (Eq)
	

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "True"
  show (Bool False) = "False"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = " Undefined"
  show (Function (Id id) args commands) = " Func: " ++ id ++ (showArgs args)
  show (Return v) = show v
  show GlobalVar = "Undefined variable"
  show (Double d) = show d
  show (List l) = showListContents l
  show (Break b) = show b
  show Empty = " Empty";
  
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showArgs [] = ""
showArgs (Id c :cs) = (show c) ++ ";" ++ showArgs cs

showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
