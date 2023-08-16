module TypeChecker where
    import Grammar

-- Data type to represent the type of an expression
data Type = TInt | TBool | TList Type | TPair Type Type | TTile deriving (Show, Eq)

-- Data type to represent a type environment
type TypeEnv = [(String, Type)]

-- Function to look up a variable's type in the type environment
lookupVarType :: String -> TypeEnv -> Type
lookupVarType varName environment =
  case lookup varName environment of
    Just t -> t
    Nothing -> error ("Variable " ++ varName ++ " not found in type environment")

-- Type check an expression
typeCheckExp :: Exp -> TypeEnv -> Type
typeCheckExp (Variable varName) environment = lookupVarType varName environment
typeCheckExp (Assign varName funcExp) environment =
  let funcType = typeCheckFunction funcExp environment
   in case lookup varName environment of
        Just t ->
          if t == funcType
            then t
            else error ("Type error: cannot assign " ++ show funcType ++ " to " ++ show t)
        Nothing -> funcType
typeCheckExp (Hbuild intExp funcExp) environment =
  if typeCheckExp intExp environment == TInt && typeCheckFunction funcExp environment == TPair TTile TTile
    then TTile
    else error "Type error: invalid arguments for HBuild"

typeCheckExp (Vbuild intExp funcExp) environment =
  if typeCheckExp intExp environment == TInt && typeCheckFunction funcExp environment == TPair TTile TTile
    then TTile
    else error "Type error: invalid arguments for VBuild"

typeCheckExp (Rotate intExp funcExp) environment =
  if typeCheckExp intExp environment == TInt && typeCheckFunction funcExp environment == TPair TTile TTile
    then TTile
    else error "Type error: invalid arguments for Rotate"

typeCheckExp (Scale intExp funcExp) environment =
  if typeCheckExp intExp environment == TInt && typeCheckFunction funcExp environment == TPair TTile TTile
    then TTile
    else error "Type error: invalid arguments for Scale"

typeCheckExp (BlankPiece pairExp) environment =
  if typeCheckPair pairExp environment == TPair TInt TInt
    then TTile
    else error "Type error: invalid arguments for BlankPiece"

typeCheckExp (DiagSplit pairExp funcExp1 funcExp2) environment =
  if typeCheckPair pairExp environment == TPair TInt TInt && typeCheckFunction funcExp1 environment == TTile && typeCheckFunction funcExp2 environment == TTile
    then TTile
    else error "Type error: invalid arguments for DiagSplit"

typeCheckExp (Hstick funcExp1 funcExp2) environment =
  if typeCheckFunction funcExp1 environment == TTile && typeCheckFunction funcExp2 environment == TTile
    then TTile
    else error "Type error: invalid arguments for Hstick"

typeCheckExp (Vstick funcExp1 funcExp2) environment =
  if typeCheckFunction funcExp1 environment == TTile && typeCheckFunction funcExp2 environment == TTile
    then TTile
    else error "Type error: invalid arguments for Vstick"

typeCheckExp (And funcExp1 funcExp2) environment =
  if typeCheckFunction funcExp1 environment == TTile && typeCheckFunction funcExp2 environment == TTile
    then TTile
    else error "Type error: invalid arguments for And" 

typeCheckExp (Negate funcExp) environment =
  if typeCheckFunction funcExp environment == TTile
    then TTile
    else error "Type error: invalid argument for Negate"

typeCheckExp (CreateSubtile intExp1 intExp2 funcExp) environment =
  if typeCheckExp intExp1 environment == TInt && typeCheckExp intExp2 environment == TInt && typeCheckFunction funcExp environment == TTile
    then TTile
    else error "Type error: invalid arguments for CreateSubtile"


-- Type check a pair expression
typeCheckPair :: Pair -> TypeEnv -> Type
typeCheckPair (Pair exp1 exp2) environment =
if typeCheckExp exp1 environment == TInt && typeCheckExp exp2 environment == TInt
then TPair TInt TInt
else error "Type error: invalid arguments for PairExp"

-- Type check a function expression
typeCheckFunction :: Function -> TypeEnv -> Type

typeCheckFunction (Lambda argName argType bodyExp) environment =
    let 
        updatedEnv = (argName, argType) : environment
    in 
        TPair argType (typeCheckExp bodyExp updatedEnv)

typeCheckFunction (App funcExp argExp) environment =
    let funcType = typeCheckFunction funcExp environment
        argType = typeCheckExp argExp environment
    in case funcType of
        TPair argType' returnType ->
        if argType == argType'
        then returnType
        else error ("Type error: cannot apply " ++ show argType ++ " to " ++ show argType')
    _ -> error "Type error: cannot apply a non-function expression"

typeCheckFunction (If condExp thenExp elseExp) environment =
    if typeCheckExp condExp environment == TBool && typeCheckFunction thenExp environment == typeCheckFunction elseExp environment
    then typeCheckFunction thenExp environment
    else error "Type error: invalid arguments for If expression"

typeCheckFunction (Let varName varExp bodyExp) environment =
    let varType = typeCheckExp varExp environment
        updatedEnv = (varName, varType) : environment
    in typeCheckExp bodyExp updatedEnv

typeCheckFunction (Not funcExp) environment =
  if typeCheckFunction funcExp environment == TBool
    then TBool
    else error "Type error: invalid argument for Not"



