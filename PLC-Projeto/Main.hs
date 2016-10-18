import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

import Data.Bits
--
-- Evaluate functions
--

--EXPRESSIONS
evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (NumLit num) = return $ Double num
evalExpr env (StringLit string) = return $ String string
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (ArrayLit []) = return $ List []
evalExpr env (ArrayLit l) = evalList env l (List [])
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (ListExpr []) = return Nil
evalExpr env (ListExpr [x]) = do 
	evalExpr env x
evalExpr env (ListExpr (x:xs)) = do 
	evalExpr env x >> evalExpr env (ListExpr xs)


evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    proc <-stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    if (proc == GlobalVar) then setGlobalVar var e else setVar var e

evalExpr env (AssignExpr OpAssign (LBracket exp1 exp2) expr ) = do
	case exp1 of
		VarRef (Id id) -> do 
			variable <- stateLookup env id
			posDot <- evalExpr env exp2
			e <- evalExpr env expr
			case variable of 
				List l -> do
					newList <- createList (List l) (List []) (posDot) (e)
					setVar id newList
				_ -> error $ "Not a List"

evalExpr env (UnaryAssignExpr inc (LVar var)) = do
	case inc of 
		PrefixInc -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
		PrefixDec -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))
		PostfixInc -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
		PostfixDec -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))

evalExpr env (BracketRef expr1 expr2) = do 
	evaluatedExpr1 <- evalExpr env expr1
	evaluatedExpr2 <- evalExpr env expr2
	findElementAt env evaluatedExpr1 evaluatedExpr2

evalExpr env (FuncExpr maybeId id stmt) = do
    case maybeId of
        Nothing -> return $ Function (Id "fun") id stmt
        Just v -> return $ Function v id stmt

evalExpr env (CondExpr expr1 expr2 expr3) = do
	evaluatedExpr <- evalExpr env expr1
	case evaluatedExpr of
		Bool True -> evalExpr env expr2
		Bool False -> evalExpr env expr3

evalExpr env (DotRef expr (Id id)) = do 
	firstId <- evalExpr env expr
--	case firstId of 
--		(Function name arguments statements) -> do 
--			listReturns <- searchForReturn (BlockStmt statements) (ListExpr [])
	case firstId of 
		List l -> do 
			case id of 
				"head" -> headOp env (List l)
				"tail" -> tailOp env (List l)
				_ -> error $ "Nao existe funcao"

evalExpr env (CallExpr name params) = do
    case name of
        DotRef expr (Id id) -> do
            list <- evalExpr env expr
            case list of
                List l -> do
                    case id of
                        "concat" -> concatOp env l params
                        "head" -> headOpExp env params
                        "tail" -> tailOpExp env params
                        "equals" -> equalsOp env l params
        _ -> do
            evaluedName <- evalExpr env name
            case evaluedName of
                (Function ids args stmt) -> do
                    pushScope
                    compareArgs env args params
                    ret <- evalStmt env (BlockStmt stmt)
                    popScope
                    case ret of
                        Return r -> return r
                        Break b -> error $ "break"
                        _ -> return Nil

--compareIdFunction :: StateT -> Value -> Expression -> StateTransformer Value
--compareIdFunction env id (ListExpr []) = erro $ "Nao existe essa funcao"
--compareIdFucntion env id (ListExpr (n:xs)) = do 
--	posFunc <- evalExpr env n
--	case posFunc of 
--		(Function name args stmts) -> do 
--			if(id == name) then 


--searchForReturn :: Statement -> Expression
--searchForReturn (BlockStmt []) (ListExpr l) = ListExpr l 
--searchForReturn (BlockStmt (s:sts)) (ListExpr l) = case s of 
--	(ReturnStmt r) -> case r of 
--		Nothing -> searchForReturn (BlockStmt sts) (ListExpr l)
--		Just v -> searchForReturn (BlockStmt sts) (ListExpr l ++ [v])
--	_ -> searchForReturn (BlockStmt sts) (ListExpr l)

--List RELATED STUFFS

evalList :: StateT ->[Expression] ->Value ->StateTransformer Value
evalList env [] (List l) = return $ List l
evalList env (x:xs) (List l) = do
	evaluatedExpr <- evalExpr env x
	evalList env xs (List (l++[evaluatedExpr]))

createList :: Value -> Value -> Value -> Value -> StateTransformer Value
createList (List x) (List []) (Int 0) e = return (List (x ++ [e]))
createList (List x) (List []) (Int n) e = createList (List (x ++ [Empty])) (List []) (Int (n-1)) e
createList (List x) (List (l:ls)) (Int 0) e = return (List (x ++ [e] ++ ls))
createList (List x) (List (l:ls)) (Int n) e = createList (List (x ++ [l])) (List ls) (Int (n-1)) e

findElementAt :: StateT -> Value -> Value -> StateTransformer Value
findElementAt env (List []) (Int n) = return $ Nil
findElementAt env (List (x:xs)) (Int 0) = return x
findElementAt env (List (x:xs)) (Int n) = do 
	findElementAt env (List xs) (Int (n-1))

--STATEMENTS
evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (IfStmt exp st1 st2) = do
	e <- evalExpr env exp 
	case e of 
		Bool True -> evalStmt env st1
		Bool False -> evalStmt env st2

evalStmt env (IfSingleStmt exp st1) = do
      e <- evalExpr env exp
      case e of
      	Bool True -> evalStmt env st1
      	Bool False -> return Nil

evalStmt env (ForStmt init test inc stmt) = do
	case init of 
		NoInit -> return Nil
		VarInit vars -> evalStmt env (VarDeclStmt vars)
		ExprInit expr -> evalExpr env expr
	case test of 
		Nothing -> do
			evaluatedStmt <- evalStmt env stmt
			case evaluatedStmt of
				Break b -> return Nil
				Return r -> return $ (Return r)
				_ -> do
					case inc of 
						Nothing -> evalStmt env (ForStmt NoInit test inc stmt)
						Just incV -> evalExpr env incV >> evalStmt env (ForStmt NoInit test inc stmt)
		Just bool -> do 
			evaluatedExpr <- evalExpr env bool
			case evaluatedExpr of
				Bool True -> do
					stmtEvaluated <- evalStmt env stmt
					case stmtEvaluated of
						Break d -> return Nil
						Return l -> return $ (Return l)
						_ -> do
							case inc of
								Nothing -> evalStmt env (ForStmt NoInit test inc stmt)
								Just incW -> evalExpr env incW >> evalStmt env (ForStmt NoInit test inc stmt)
				Bool False -> return Nil

evalStmt env (WhileStmt exp stmt) = do
	evaluatedExpr <- evalExpr env exp
	case evaluatedExpr of
		Bool True -> do 
			e <- evalStmt env stmt 
			case e of
				Break b -> return Nil
                Return r -> return (Return r) 
                _ -> evalStmt env (WhileStmt exp stmt)
		Bool False -> return Nil

evalStmt env (DoWhileStmt stmt expr) = do
    evaluetedExpr <- evalExpr env expr
    evaluetedStmt <- evalStmt env stmt
    case evaluetedExpr of
        Bool True -> do
            case evaluetedStmt of
                Break b -> return Nil
                Return r -> return (Return r)
                _ -> evalStmt env (DoWhileStmt stmt expr)
        Bool False -> return Nil

evalStmt env (SwitchStmt expr cases) = do 
	evaluatedExpr <- evalExpr env expr
	compareCases env evaluatedExpr cases

evalStmt env (BreakStmt id) = do
	case id of
		Nothing -> return (Break Nothing)
		Just idM -> return (Break $ Just idM)

evalStmt env (ReturnStmt expr) = do
	case expr of
		Nothing -> return (Return Nil)
		Just exprM -> do
			evaluatedExpr <- evalExpr env exprM
			return (Return evaluatedExpr)

   
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (x:xs)) = do 
	evalStatement <- evalStmt env x
	case evalStatement of 
		Return r -> return (Return r)
		Break b -> return (Break b)
		_ -> evalStmt env (BlockStmt xs)

evalStmt env (FunctionStmt (Id name) args stmts) = setLocalVar name (Function (Id name) args stmts)

compareCases :: StateT -> Value -> [CaseClause] ->StateTransformer Value
compareCases env evaluatedExpr _ = return Nil
compareCases env evaluatedExpr (c:cs) = do 
	case c of 
		CaseClause exp stmts -> do 
			evaluatedExpr2 <- evalExpr env exp
			if(evaluatedExpr2 == evaluatedExpr) then
				evalStmt env (BlockStmt stmts)
			else compareCases env evaluatedExpr (c:cs)
		CaseDefault stmts2 -> do
			evalStmt env (BlockStmt stmts2)

--evalBlock :: StateT -> Statement -> StateTransformer Value
--evalBlock env (BlockStmt []) = return Nil
--evalBlock env (BlockStmt (x:xs)) = do
	--evalStmt env x >> evalBlock env (BlockStmt xs)

--FUNCTION RELATED STUFF

compareArgs:: StateT ->[Id]->[Expression]->StateTransformer Value
compareArgs env [] [] = return Nil
compareArgs env (Id arg:args) (param:params) = do
    evaluatedParam <- evalExpr env param
    setLocalVar arg evaluatedParam
    compareArgs env args params
compareArgs env _ _ = error $ "Number of params mismatch" 

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2
infixOp env OpBXor  (Bool v1) (Bool v2) = return $ Bool $ xor v1 v2
infixOp env OpLShift (Int v1) (Int v2) = return $ Int $ shiftL v1 v2
infixOp env OpSpRShift  (Int v1) (Int v2) = return $ Int $ shiftR v1 v2

prefixOp :: StateT -> PrefixOp -> Value -> StateTransformer Value
prefixOp env PrefixLNot (Bool v1) = return $ Bool $ not v1
prefixOp env PrefixBNot (Bool v1) = return $ Bool $ not v1
prefixOp env PrefixMinus (Int v1) = return $ Int $ -v1
prefixOp env PrefixMinus (Double v1) = return $ Double $ -v1


--
-- Environment and auxiliary functions
--

environment :: StateT
environment = [Map.empty]

--Head /Tail/Concat

headOp :: StateT -> Value -> StateTransformer Value
headOp env (List []) = return Nil
headOp env (List (x:xs)) = return x

tailOp :: StateT -> Value -> StateTransformer Value
tailOp env (List []) = return Nil
tailOp env (List [x]) = return x
tailOp env (List (x:xs)) = return (List xs)

headOpExp :: StateT -> [Expression] -> StateTransformer Value
headOpExp env [] = return Nil
headOpExp env (x:xs) = evalExpr env x

tailOpExp :: StateT -> [Expression] -> StateTransformer Value
tailOpExp env [] = return Nil
tailOpExp env [x] = evalExpr env x
tailOpExp env (x:xs) = evalExpr env (ListExpr xs)

concatOp :: StateT ->[Value] ->[Expression] ->StateTransformer Value
concatOp env l [] = return (List l)
concatOp env l (param:params) = do 
	evaluatedParam <- evalExpr env param
	case evaluatedParam of
		(List l2) -> concatOp env (l++l2) params
		v-> concatOp env (l++[v]) params

equalsOp :: StateT -> [Value] -> [Expression] -> StateTransformer Value
equalsOp env l [] = return $ Bool True
equalsOp env l (param:params) = do
	evalParam <- evalExpr env param
	case evalParam of
		(List l2) -> do
			if (compareList l l2) then (equalsOp env l params) else return $ Bool False

compareList :: [Value] -> [Value] -> Bool
compareList [] [] = True
compareList x [] = False
compareList [] y = False
compareList (x:xs) (y:ys) = (x == y) && (compareList xs ys) 

compareValue :: StateT -> Value -> Value -> StateTransformer Value
compareValue env v1 v2 = return $Bool (v1 == v2) 


stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case scopeLookup s var of
        Nothing -> (GlobalVar,s)
        Just val -> (val, s)

--varDeclList :: StateT -> [VarDecl] -> StateTransformer Value
--varDeclList env [] = return Nil
--varDeclList env var = foldl1 (>>) map (varDecl env) var 
		
varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setLocalVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setLocalVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, setVarScopes var val s)

setLocalVar ::String -> Value -> StateTransformer Value
setLocalVar var val = ST $ \s -> (val, (insert var val (head s)):(tail s))

setGlobalVar :: String ->Value ->StateTransformer Value
setGlobalVar var val = ST $ \s -> (val, setGlobalAux var val s)

setGlobalAux :: String -> Value -> StateT -> StateT
setGlobalAux var val (s:scopes) = if(scopes == [])
                                   then (insert var val s):scopes
									else s:(setGlobalAux var val scopes)

setVarScopes :: String -> Value -> StateT -> StateT
setVarScopes var val st = case (Map.lookup var (head st)) of
	Nothing -> (head st):(setVarScopes var val (tail st))
	Just v -> (insert var val (head st)):(tail st)
setVarScopes var val _ = error $ "Variavel nao existe"

--ESCOPO- VARIAVEIS LOCAIS E GLOBAIS
scopeLookup :: StateT -> String -> Maybe Value
scopeLookup [] var = Nothing
scopeLookup (scope:scopes) var = case Map.lookup var scope of 
                                 Nothing -> scopeLookup scopes var 
                                 Just val-> Just val
pushScope:: StateTransformer Value
pushScope = ST $ \s -> (Nil, (Map.empty):s)

popScope:: StateTransformer Value
popScope = ST $	\s -> (Nil,(tail s))							 
--
-- Types and boilerplate
--

type StateT = [Map String Value] 
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, []) = ""
showResult (val, (s:scopes)) =
    show val ++ "\n" ++ show (toList $ union s (Map.empty)) ++ "\n" ++ showResult (val, scopes)

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [Map.empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
