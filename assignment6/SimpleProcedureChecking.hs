{-# LANGUAGE TupleSections #-}

module SimpleProcedureChecking where

import ExtendedBTLAST
import ExtendedBTLDeclarations
import InterpretBiplAst (Stmt (..))
import SimpleProcedure
  ( Parameter,
    Proc (..),
    computusProc,
    eucliddivProc,
    extractParameterNames,
    initproblemProc,
    simplifiedPascalStandardFunctions,
  )
import WellTypedAST (typeCheck)

-- | Type checking the procedure: build the type environment and check the statements.
typeCheckProc :: [FunType] -> Proc -> Bool
typeCheckProc funtypes (ProcedureDeclaration _ params localVars stmt) = typeCheckStmt (funtypes, vartypes) stmt
  where
    vartypes = map snd params ++ localVars

-- | Type checking the statements.
typeCheckStmt :: Declarations -> Stmt -> Bool
typeCheckStmt decls@(_, vartypes) (Assign name expr) =
  (declaredType == assignedType)
    || error ("Trying to assign " ++ show assignedType ++ " to variable " ++ name ++ " of type " ++ show declaredType)
  where
    declaredType = findVarType name vartypes
    assignedType = typeCheck decls expr
typeCheckStmt decls (Block stmts) = and [typeCheckStmt decls s | s <- stmts]
typeCheckStmt decls (IfStmt con _ _) = (conType == TBool) || error "Condition is not of type TBool"
  where
    conType = typeCheck decls con
typeCheckStmt decls (While con _) = (conType == TBool) || error "Condition is not of type TBool"
  where
    conType = typeCheck decls con

-- | Checking the procedure: no duplicate declarations in parameter list and local variables.
variableCheckProc :: Proc -> Bool
variableCheckProc (ProcedureDeclaration name params localVars stmt) =
  not $ hasDuplicates (paramAndVarNames params localVars)

hasDuplicates :: [String] -> Bool
hasDuplicates [] = False
hasDuplicates (x : xs)
  | x `elem` xs = True
  | otherwise = hasDuplicates xs

paramAndVarNames :: [Parameter] -> [VarType] -> [String]
paramAndVarNames params localVars = extractParameterNames params ++ map fst localVars

-- | Checking the procedure: no use of uninitialised variables
--   and checking that all variables have been declared.
initializedCheckProc :: Proc -> Bool
initializedCheckProc (ProcedureDeclaration _ params localVars stmt) = not $ null $ initializedCheckStmt stmt vars
  where
    vars = map (,True) (extractParameterNames params) ++ map ((,False) . fst) localVars

initializedCheckStmt :: Stmt -> [(String, Bool)] -> [(String, Bool)]
initializedCheckStmt (Block stmts) vars = foldl (flip initializedCheckStmt) vars stmts
initializedCheckStmt (Assign name expr) vars = case lookup name vars of
  Nothing -> error (name ++ " is not declared")
  Just initialized ->
    if initialized
      then initializedCheckExpr expr vars
      else initialize name $ initializedCheckExpr expr vars
initializedCheckStmt (IfStmt expr thenStmt elseStmt) vars =
  initializedCheckExpr expr $
    initializedCheckStmt thenStmt $
      initializedCheckStmt elseStmt vars
initializedCheckStmt (While expr stmt) vars = initializedCheckExpr expr $ initializedCheckStmt stmt vars

initializedCheckExpr :: Expr -> [(String, Bool)] -> [(String, Bool)]
initializedCheckExpr (IntLiteral _) vars = vars
initializedCheckExpr (BoolLiteral _) vars = vars
initializedCheckExpr (VarId name) vars = case lookup name vars of
  Nothing -> error (name ++ " is not in scope")
  Just initialized -> if initialized then vars else error (name ++ " is not initialized")
initializedCheckExpr (UPlus expr) vars = initializedCheckExpr expr vars
initializedCheckExpr (UMinus expr) vars = initializedCheckExpr expr vars
initializedCheckExpr (UNot expr) vars = initializedCheckExpr expr vars
initializedCheckExpr (FunctionCall _ exprList) vars = foldl (flip initializedCheckExpr) vars exprList
initializedCheckExpr (BinaryExpr _ exprLeft exprRight) vars =
  initializedCheckExpr exprLeft $
    initializedCheckExpr exprRight vars
initializedCheckExpr (IfExpr conExpr thenExpr elseExpr) vars =
  initializedCheckExpr conExpr $
    initializedCheckExpr thenExpr $
      initializedCheckExpr elseExpr vars

initialize :: String -> [(String, Bool)] -> [(String, Bool)]
initialize _ [] = []
initialize name ((v, i) : vars)
  | name == v = (v, True) : initialize name vars
  | otherwise = (v, i) : initialize name vars

-- | Doing a complete safety check of a procedure
-- • Type correctness
-- • Variables declared before use
-- • No dublicate declarations
-- • No use before initialisation
safetyCheck :: Proc -> Bool
safetyCheck proc =
  initializedCheckProc proc
    && variableCheckProc proc
    && typeCheckProc simplifiedPascalStandardFunctions proc

-- | Unit tests
unitTestEucliddiv :: Bool
unitTestEucliddiv = safetyCheck eucliddivProc

unitTestComputus :: Bool
unitTestComputus = safetyCheck computusProc

unitTestInitProblem :: Bool
unitTestInitProblem = safetyCheck initproblemProc

-- | More unit tests
unitTestProcNotDeclared :: Bool
unitTestProcNotDeclared =
  safetyCheck
    ( ProcedureDeclaration
        "procNotDeclared"
        []
        []
        ( Assign
            "x"
            (IntLiteral 1)
        )
    )

unitTestProcNotInitialized :: Bool
unitTestProcNotInitialized =
  safetyCheck
    ( ProcedureDeclaration
        "procNotInitialized"
        []
        [("x", TInt), ("y", TInt)]
        (Assign "y" (VarId "x"))
    )

unitTestProcNotInScope :: Bool
unitTestProcNotInScope =
  safetyCheck
    ( ProcedureDeclaration
        "procNotInScope"
        []
        [("x", TInt), ("y", TInt)]
        (Assign "y" (VarId "q"))
    )