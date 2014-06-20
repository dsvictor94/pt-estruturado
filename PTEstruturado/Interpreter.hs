module PTEstruturado.Interpreter
( run
, runfile
) where

import PTEstruturado.Data
import PTEstruturado.Parse
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Control.Monad

import Data.List (find)
import Data.Maybe

data AssignedVar = AssignedVar Variavel Value

instance Show AssignedVar where
  show (AssignedVar var value) =
    "'var " ++ (nome var) ++ " (" ++ (show $ tipo var) ++ ") = " ++ (show value) ++ "'"

data Value = Int  Integer
           | Frac Double
           | Logi Bool
           | Unassigned

instance Show Value where
  show (Logi True)  = "verdadeiro"
  show (Logi False) = "falso"
  
  show (Int  x) = show x
  show (Frac x) = show x
  
  show (Unassigned) = "(unassigned)"
    
             
type ProgramState = [AssignedVar]

type Interpreter a = StateT ProgramState IO a


assign::String -> Value -> Interpreter ()
assign name val = do
  vars <- get 
  case val of 
       Int      _ -> put $ update [] Inteiro vars
       Frac     _ -> put $ update [] Fracionario vars
       Logi     _ -> put $ update [] Logico vars
       Unassigned -> error $ "can't assign (unassigned) to "++show name

  where
    update _ _ [] = error $ "can't assign to undefined variable "++show name
    update olds t ((AssignedVar var value):xs) = 
      case var of
           (Variavel nome tipo)
             | nome == name && tipo /= t ->
               error $ "can't assign '" ++ show val ++ " (" ++ show t ++ ")'"
                     ++" to variable '"++ name ++ " (" ++ show tipo ++ ")'"
             | nome == name && tipo == t ->
               (AssignedVar var val):olds ++ xs
             | otherwise ->
               update ((AssignedVar var value):olds) t xs

access::String -> Interpreter Value 
access name = do
  vars <- get 
  getValue $ fromJust $ find predicate vars 
  where
    getValue avar = case avar of
                         (AssignedVar _ Unassigned) 
                           -> error $ "tried access unassigned var " ++ (show name) 
                         (AssignedVar _ value) -> return value                  
    predicate (AssignedVar v _) = 
        nome v == name

makeProgramState :: [Variavel] -> ProgramState
makeProgramState vars =
  map (\x -> AssignedVar x Unassigned) vars
  
interpreterStmt :: Instr -> Interpreter ()
interpreterStmt (Seq [])     = return ()
interpreterStmt (Seq (x:xs)) = 
   interpreterStmt x >> interpreterStmt (Seq xs)
   
interpreterStmt (Atrib name expr) = 
  interpreterExpr expr >>= assign name 
interpreterStmt (Escreva expr) = do
  val <- interpreterExpr expr
  liftIO $ print $ val
  
interpreterExpr :: Expr -> Interpreter Value  
interpreterExpr (Arit expr) = interpreterArit expr
interpreterExpr (Logica expr) = interpreterLogica expr

interpreterLogica:: ExpLogica -> Interpreter Value
interpreterLogica (VarLogica name) = access name
interpreterLogica (ConsLogica v) = return $ Logi v
interpreterLogica (Negacao expr) = do
  (Logi v) <- interpreterLogica expr
  return $ Logi $ not v
interpreterLogica (LogicoBin E rExpr lExpr) =do 
  (Logi r) <- interpreterLogica rExpr
  (Logi l) <- interpreterLogica lExpr
  return $ Logi (r && l)
interpreterLogica (LogicoBin Ou rExpr lExpr) =do 
    (Logi r) <- interpreterLogica rExpr
    (Logi l) <- interpreterLogica lExpr
    return $ Logi (r || l)
--  | RelacianalBin OpRelacional ExpArit ExpArit

interpreterArit:: ExpArit -> Interpreter Value
interpreterArit (VarArit name) = access name
interpreterArit (ConsArit (Left x)) = return $ Int x 
interpreterArit (ConsArit (Right x)) = return $ Frac x
interpreterArit (Neg expr) = do
  val <- interpreterArit expr
  case val of
       Int  x -> return $ Int  (-x)
       Frac x -> return $ Frac (-x)

interpreterArit (AritBin Divi lExpr rExpr) = do
  l <- interpreterArit lExpr
  r <- interpreterArit rExpr
  case (l,r) of
       (Int  l, Int  r) -> return $ Frac $ (/) (fromIntegral l) (fromIntegral r)
       (Int  l, Frac r) -> return $ Frac $ (/) (fromIntegral l) r
       (Frac l, Int  r) -> return $ Frac $ (/) l (fromIntegral r)
       (Frac l, Frac r) -> return $ Frac $ (/) l r
       
interpreterArit (AritBin Mult lExpr rExpr) = do
  l <- interpreterArit lExpr
  r <- interpreterArit rExpr
  case (l,r) of
       (Int  l, Int  r) -> return $ Int $ (*) l r
       (Int  l, Frac r) -> return $ Frac $ (*) (fromIntegral l) r
       (Frac l, Int  r) -> return $ Frac $ (*) l (fromIntegral r)
       (Frac l, Frac r) -> return $ Frac $ (*) l r
       
interpreterArit (AritBin Soma lExpr rExpr) = do
  l <- interpreterArit lExpr
  r <- interpreterArit rExpr
  case (l,r) of
       (Int  l, Int  r) -> return $ Int $ (+) l r
       (Int  l, Frac r) -> return $ Frac $ (+) (fromIntegral l) r
       (Frac l, Int  r) -> return $ Frac $ (+) l (fromIntegral r)
       (Frac l, Frac r) -> return $ Frac $ (+) l r

interpreterArit (AritBin Subt lExpr rExpr) = do
  l <- interpreterArit lExpr
  r <- interpreterArit rExpr
  case (l,r) of
       (Int  l, Int  r) -> return $ Int $ (+) l r
       (Int  l, Frac r) -> return $ Frac $ (+) (fromIntegral l) r
       (Frac l, Int  r) -> return $ Frac $ (+) l (fromIntegral r)
       (Frac l, Frac r) -> return $ Frac $ (+) l r

interpreterArit (AritBin Rest lExpr rExpr) = do
  l <- interpreterArit lExpr
  r <- interpreterArit rExpr
  case (l,r) of
       (Int  l, Int  r) -> return $ Int $ mod l r
       (Int  l, Frac r) -> error $ "imposible exec '%' between 'int' and 'real'"
       (Frac l, Int  r) -> error $ "imposible exec '%' between 'real' and 'int'"
       (Frac l, Frac r) -> error $ "imposible exec '%' between 'real' and 'real'"

       
run (Algoritimo _ vars instr) =
  runStateT (interpreterStmt instr) (makeProgramState vars)

  
runfile file = (parseFile file >>= run)