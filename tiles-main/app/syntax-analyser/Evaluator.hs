{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Evaluator where
    import Grammar
    import Tokens
    import Data.Char
    import qualified GhcPlugins as Data.List

-- C -- Control
-- E -- Environment
-- (S) -- Store
-- K -- Continuation

{-
data Exp = TmLet String Exp Exp
         | TmIf Exp Exp Exp
         | TmFunction Exp  
         | TmHbuild Exp Exp 
         | TmVbuild Exp Exp 
         | TmRotate Exp Exp 
         | TmScale Exp Exp 
         | TmBlankPiece Exp 
         | TmDiagSplit Exp Exp Exp 
         | TmHstick Exp Exp 
         | TmVstick Exp Exp 
         | TmHReflect Exp
         | TmVReflect Exp
         | TmAnd Exp Exp
         | TmNegate Exp 
         | TmCreateSubtile Exp Exp Exp
         | TmInt Int 
         | TmPlus Exp Exp
         | TmMinus Exp Exp
         | TmMultiply Exp Exp
         | TmFst Exp 
         | TmSnd Exp 
         | TmCompSize Exp 
         | TmPair Exp Exp 
         | TmList Exp 
         | TmElements Exp Exp 
         | TmVariable String
         | TmParenthasized Exp
-}

--store the bindings of variables to their values during evaluation
type TypeEnvironment = [(String, Exp)]

--keep track of what remains to be evaluated after the current expression
data Kont = KArg Exp Kont    -- argument in a function call
          | KFn TypeEnvironment Exp Kont  -- function call
          | KIf Exp Exp TypeEnvironment Kont  -- condition in an if statement
          | KLet String Exp TypeEnvironment Kont  -- expression in a let binding
          | KRet  -- return from a function
          deriving Show

data Value = Closure String Exp TypeEnvironment

data State  = State Exp TypeEnvironment Kont

start :: Exp -> State 
start controll = State control (const undefined) KArg

--small step semantics step
step :: State -> State
-- evaluate the let binding
step (State (TmLet x e1 e2) env k) = State e1 env (KLet x e2 env k)
-- evaluate the if condition 
step (State (TmIf e1 e2 e3) env k) = State e1 env (KIf e2 e3 env k)
 -- create a closure
step (State (TmFunction x e) env k) = State (Closure x e env) env k
-- evaluate the first expression in horizontal build
step (State (TmHbuild e1 e2) env k) = State e1 env (KArg e2 k)
-- evaluate the first expression in vertical build 
step (State (TmVbuild e1 e2) env k) = State e1 env (KArg e2 k)
-- evaluate the first expression in rotate 
step (State (TmRotate e1 e2) env k) = State e1 env (KArg e2 k) 
-- evaluate the first expression in scale
step (State (TmScale e1 e2) env k) = State e1 env (KArg e2 k) 
-- blank piece is a value, just return it
step (State (TmBlankPiece e) env k) = State e env k 
-- evaluate the first expression in diagSplit
step (State (TmDiagSplit e1 e2 e3) env k) = State e1 env (KFn env (TmVbuild (TmHbuild e2 (TmRotate e3 (TmHReflect e2))) (TmHbuild (TmRotate e3 (TmVReflect e2)) (TmVbuild e2 (TmHbuild (TmHReflect e3) (TmVReflect e3))))) k) 
-- evaluate the first expression in hStick
step (State (TmHstick e1 e2) env k) = State e1 env (KArg e2 k) 
-- evaluate the first expression in vStick
step (State (TmVstick e1 e2) env k) = State e1 env (KArg e2 k) 
-- horizontal reflection is a value, just return it
step (State (TmHReflect e) env k) = State e env k 
-- vertical reflection is a value, just return it
step (State (TmVReflect e) env k) = State e env k 
-- evaluate the first expression in and
step (State (TmAnd e1 e2) env k) = State e1 env (KArg e2 k) 
-- negate is a value, just return it
step (State (TmNegate e) env k) = State e env k 
-- evaluate the first expression in createSubtile
step (State (TmCreateSubtile e1 e2 e3) env k) = State e1 env (KFn env (TmHbuild (TmVbuild e3 e2) (TmH Reflect (TmVbuild (TmVReflect e3) (TmHReflect e2))))) k 
-- lookup the variable in the environment
step (State (TmVar x) env k) = State (envLookup x env) env k
-- evaluate the function 
step (State (TmApp e1 e2) env k) = State e1 env (KArg e2 k) 
-- evaluate the first expression in sequence
step (State (TmSeq e1 e2) env k) = State e1 env (KSeq e2 env k)
-- numbers are values, just return them
step (State (TmNum _) env k) = State (TmNum _) env k 
-- booleans are values, just return them
step (State (TmBool _) env k) = State (TmBool _) env k 
step (State (Closure x e env') env k) = 
    case k of
        KArg e' k' -> State e' env (KFn env (TmApp (Closure x e env') e') k')
        KLet x' e' env' k' -> State e' env' (KLet x' e' ((x, Closure x e env'):env') k') 
        KIf e1 e2 env' k' -> State e1 env' (KIf e2 e3 env' k')
        KSeq e' env' k' -> State e' env' k'
        KFn env'' e' k' -> State (TmApp (Closure x e env') e') env'' k'
        _ -> error "Invalid continuation"

extend :: String -> Value -> TypeEnvironment -> TypeEnvironment
extend i v f j
    | i == j = v
    | otherwise = f j

final :: Exp -> TypeEnvironment -> Kont -> Value
final e env k = 
  let s = State e env k
      s' = eval s
  in value s'
    where
        value :: State -> Value
        value (State e env KRet) = evalValue e env

evaluate:: State -> State
evaluate = until final step
{-
eval :: State -> State
eval s@(State (TmInt _) _ _) = s
eval s@(State (TmBlankPiece _) _ _) = s
eval (State (TmPlus e1 e2) env k) =
  case eval e1 env of
    Closure x e env' -> State e2 env' (KFn env (TmPlus (TmInt x) e2) k)
    TmInt n -> State e2 env (KArg (TmInt (n + evalInt e2 env)) k)
    _ -> error "Invalid argument to + operator"
eval (State (TmMinus e1 e2) env k) = ...

-}