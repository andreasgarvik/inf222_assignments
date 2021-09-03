-- | AST and examples for simplified Pascal style procedures
--
-- Author Magne Haveraaen
-- Since 2021-02-22
module SimpleProcedure where

-- Uses extended BTL expression AST from L0502

-- Uses typing framework from L0502

-- Uses extended BIPL statements and interpreters from L0602
import EvalBstAst
  ( Environment,
    Store,
    ValueDomain (VDInt),
    addvariable,
    getvalue,
    newcontext,
    updvariable,
  )
import ExtendedBTLAST (Binop (..), Expr (..))
import ExtendedBTLDeclarations
  ( FunType,
    Type (..),
    VarType,
    varType,
  )
import InterpretBiplAst (Stmt (..), eucliddivStmt, exec)

--------------------------

-- | AST for simplified procedure declarations, see Pascal User Manual and Report (LNCS18).

{-
<procedure declaration> ::= <procedure heading> <block>

<procedure heading> ::=
    procedure <identifier> ( <formal parameter section> {; <formal parameter section>} )

<formal parameter section> ::= <parameter group> | var <parameter group>

<parameter group> ::= <identifier> {, <identifier> } : <type identifier>

<block> ::= <variable declaration part> <statement part>

<variable declaration part> ::=
    <identifier> {, <identifier> } : <type> ;
    { <identifier> {, <identifier> } : <type> ; }

<statement part> ::= <statement>
-}
data Proc
  = ProcedureDeclaration
      String -- Name of the procedure
      [Parameter] -- Parameter list
      [VarType] -- Local variables
      Stmt -- Statement part
  deriving (Show, Eq, Read)

-- | Characterisation of a simplified procedure's parameters:
-- First component is whether the parameter is a reference (var keyword) or not (value component).
-- The second component is the variable name and type.
type Parameter = (Bool, VarType)

-- | Extracts the list of variable names from a parameter list
extractParameterNames :: [Parameter] -> [String]
extractParameterNames = map (fst . snd)

-- | Extracts the list of reference variable names from a parameter list
extractReferenceParameterNames :: [Parameter] -> [String]
extractReferenceParameterNames params = [vname | (ref, (vname, vtype)) <- params, ref]

--------------------------

-- | Standard functions from Pascal.
-- The declarations are needed for type checking
simplifiedPascalStandardFunctions :: [FunType]
simplifiedPascalStandardFunctions =
  [ ("false", ([], TBool)),
    ("true", ([], TBool)),
    ("odd", ([TInt], TInt)),
    ("abs", ([TInt], TInt)),
    ("sqr", ([TInt], TInt)),
    ("succ", ([TInt], TInt)),
    ("pred", ([TInt], TInt)),
    ("maxint", ([], TInt))
  ]

--------------------------

-- | Perform a procedure declaration: take a list of argument values, pass them to the procedure interpreter, and gather the return values.
-- | Internally perform
-- • sets up the execution environment for the <statement part>,
-- • copies the argument values into the store
-- • interprets the procedure in this environment,
-- • gathers the resulting values of the reference parameters and returns this list.
perform :: Proc -> Args -> Rets
perform proc args = rets
  where
    ProcedureDeclaration pname params lvars stmt = proc
    (env, store) = allocateParameterAndValue (extractParameterNames params) args newcontext
    (env', store') = allocateLocalVariables lvars (env, store)
    store'' = exec stmt env' store'
    rets = extractValuesFromStore (extractReferenceParameterNames params) env store''

-- rets = returnproc params env store''

-- | List of actual argument values to parameters
type Args = [ValueDomain]

-- | List of return values from reference parameters
type Rets = [ValueDomain]

--------------------------

-- | Allocates variable names and inserts their argument values into the store.
allocateParameterAndValue :: [String] -> [ValueDomain] -> (Environment, Store) -> (Environment, Store)
allocateParameterAndValue vnames vals context@(env, store) = foldl (\context (vname, val) -> updvariable vname val $ addvariable vname context) context $ zip vnames vals

-- | Takes a list of local variable declarations and allocates them in the store.
allocateLocalVariables :: [VarType] -> (Environment, Store) -> (Environment, Store)
allocateLocalVariables vdecls context@(env, store) = foldl (\context (vname, vtype) -> addvariable vname context) context vdecls

-- | From a list of variables extract their values from the store
extractValuesFromStore :: [String] -> Environment -> Store -> Rets
extractValuesFromStore vnames env store = [getvalue x (env, store) | x <- vnames]

--------------------------
{-
   EXAMPLES
-}
--------------------------

-- | Procedure for Euclidean division.

{-
    procedure eucliddiv ( x,y:integer, var r,q:integer );
    begin
    q := 0;
    r := x;
    while y <= r do
        begin
        r := r - y;
        q := q + 1;
        end;
    end;
-}
-- The eucliddiv_stmt statement is taken from InterpretBiplAst.
eucliddivProc :: Proc
eucliddivProc =
  ProcedureDeclaration
    "eucliddiv"
    [ (False, ("x", TInt)),
      (False, ("y", TInt)),
      (True, ("q", TInt)),
      (True, ("r", TInt))
    ]
    []
    eucliddivStmt

eucliddivRun :: Integer -> Integer -> (Integer, Integer)
eucliddivRun x y = (toInteger r, toInteger q)
  where
    [VDInt r, VDInt q] =
      perform eucliddivProc [VDInt (fromInteger x), VDInt (fromInteger y), VDInt 0, VDInt 0]

unittestEucliddiv :: IO ()
unittestEucliddiv = do
  let check = [eucliddivRun x y == divMod x y | x <- [0 .. 100], y <- [1 .. 100]]
  let ok = and check
  print $ if ok then "Euclidean division tests passed!" else "Euclidean division failed!"

--------------------------

-- | Computus

{-
    (** Computing Easter Day for 2020 using "Anonymous Gregorian algorithm". *)
    procedure computus ( Y:integer; var month,day:integer ) ;
    var a,b,c,d,e,f,g,h,i,k,l,m,n,o:integer;
    begin
    a := Y mod 19;
    b := Y div 100;
    c := Y mod 100;
    d := b div 4;
    e := b mod 4;
    f := (b + 8) div 25;
    g := (b - f + 1) div 3;
    h := (19*a + b - d - g + 15) mod 30;
    i := c div 4;
    k := c mod 4;
    l := (32 + 2*e + 2*i - h - k) mod 7;
    m := (a + 11*h + 22*l) div 451;
    n := (h + l - 7*m + 114) div 31;
    o := (h + l - 7*m + 114) mod 31;
    month := n;
    day := o + 1;
    end ;
-}
computusProc :: Proc
computusProc =
  ProcedureDeclaration
    "computus"
    [ (False, ("Y", TInt)),
      (True, ("month", TInt)),
      (True, ("day", TInt))
    ]
    (varType ["a", "b", "c", "d", "e", "f", "g", "h", "i", "k", "l", "m", "n", "o"] TInt)
    computusStmt

computusStmt :: Stmt
computusStmt =
  Block
    [ Assign "a" (BinaryExpr Mod (VarId "Y") (IntLiteral 19)),
      Assign "b" (BinaryExpr Div (VarId "Y") (IntLiteral 100)),
      Assign "c" (BinaryExpr Mod (VarId "Y") (IntLiteral 100)),
      Assign "d" (BinaryExpr Div (VarId "b") (IntLiteral 4)),
      Assign "e" (BinaryExpr Mod (VarId "b") (IntLiteral 4)),
      Assign "f" (BinaryExpr Div (BinaryExpr Plus (VarId "b") (IntLiteral 8)) (IntLiteral 25)),
      Assign "g" (BinaryExpr Div (BinaryExpr Plus (BinaryExpr Minus (VarId "b") (VarId "f")) (IntLiteral 1)) (IntLiteral 3)),
      Assign "h" (BinaryExpr Mod hexp (IntLiteral 30)),
      Assign "i" (BinaryExpr Div (VarId "c") (IntLiteral 4)),
      Assign "k" (BinaryExpr Mod (VarId "c") (IntLiteral 4)),
      Assign "l" (BinaryExpr Mod lexp (IntLiteral 7)),
      Assign "m" (BinaryExpr Div (BinaryExpr Plus (BinaryExpr Plus (VarId "a") (BinaryExpr Mult (IntLiteral 11) (VarId "h"))) (BinaryExpr Mult (IntLiteral 22) (VarId "l"))) (IntLiteral 451)),
      Assign "n" (BinaryExpr Div (BinaryExpr Plus (BinaryExpr Minus (BinaryExpr Plus (VarId "h") (VarId "l")) (BinaryExpr Mult (IntLiteral 7) (VarId "m"))) (IntLiteral 114)) (IntLiteral 31)),
      Assign "o" (BinaryExpr Mod (BinaryExpr Plus (BinaryExpr Minus (BinaryExpr Plus (VarId "h") (VarId "l")) (BinaryExpr Mult (IntLiteral 7) (VarId "m"))) (IntLiteral 114)) (IntLiteral 31)),
      Assign "month" (VarId "n"),
      Assign "day" (BinaryExpr Plus (VarId "o") (IntLiteral 1))
    ]

hexp :: Expr
hexp = BinaryExpr Plus (BinaryExpr Minus (BinaryExpr Minus (BinaryExpr Plus (BinaryExpr Mult (IntLiteral 19) (VarId "a")) (VarId "b")) (VarId "d")) (VarId "g")) (IntLiteral 15)

lexp :: Expr
lexp = BinaryExpr Minus (BinaryExpr Minus (BinaryExpr Plus (BinaryExpr Plus (IntLiteral 32) (BinaryExpr Mult (IntLiteral 2) (VarId "e"))) (BinaryExpr Mult (IntLiteral 2) (VarId "i"))) (VarId "h")) (VarId "k")

computusRun :: Integer -> (Integer, Integer, Integer)
computusRun y = (toInteger y, toInteger month, toInteger day)
  where
    [VDInt month, VDInt day] =
      perform computusProc [VDInt (fromInteger y), VDInt 0, VDInt 0]

unittestComputus :: IO ()
unittestComputus = do
  let ok =
        computusRun 2021 == (2021, 4, 4)
          && computusRun 2020 == (2020, 4, 12)
          && computusRun 2019 == (2019, 4, 21)
          && computusRun 2008 == (2008, 3, 23)
          && computusRun 2011 == (2011, 4, 24)
          && computusRun 2038 == (2038, 4, 25)
  print $ if ok then "Computus tests passed!" else "Computus failed!"

--------------------------

-- | Procedure with initialisation problem.

{-
procedure init_problem ( var x:integer ) ;
var y:integer;
begin
  if (x <> 0) and (x*x = 0)
  then x := y
  else x := x div 2;
end ;
-}

initproblemProc :: Proc
initproblemProc =
  ProcedureDeclaration
    "init_problem"
    [(True, ("x", TInt))]
    [("y", TInt)]
    ( Block
        [ IfStmt
            ( BinaryExpr
                And
                (BinaryExpr Ne (VarId "x") (IntLiteral 0))
                (BinaryExpr Eq (BinaryExpr Mult (VarId "x") (VarId "x")) (IntLiteral 0))
            )
            (Assign "x" (VarId "y"))
            (Assign "x" (FunctionCall "pred" [VarId "x"]))
        ]
    )

initproblemRun :: ValueDomain -> ValueDomain
initproblemRun x = head (perform initproblemProc [x])

unittestInitproblem :: IO ()
unittestInitproblem = do
  let check = [(initproblemRun (VDInt x) == VDInt (x -1), x) | x <- [-32768 .. 32767]]
  let fails = map snd $ filter (\(c, _) -> not c) check
  let ok = all fst check
  if ok
    then print "Initialisation problem tests passed!"
    else do
      print "Initialisation problem failed!"
      print fails

-- | 4.4.1
-- (1)
-- Explain why the unit test unittest_initproblem fails:
-- Every value divisible by 256, square root of 65536,
-- of the possible values of VDInt, when squared is equal to 0 mod 65536 (number of possible values Int16)
-- higher values then 65536 will be wrapped around
-- resulting in value divisible by 256 being evaluated to 0.
-- (3)
-- Will this problem dissappear if we increase the range of our integer type to Int32 or In64?
-- No, the same thing will happen.
-- Int32 with values divisible by 65536, square root of 4294967296 (number of possible values in Int32)

-- | BONUS
-- (1)
prettyPrinter :: Proc -> IO ()
prettyPrinter (ProcedureDeclaration name params localVars stmt) =
  putStr
    ( "procedure " ++ name ++ " ( " ++ prettyParams ++ " );"
        ++ prettyLocalVars
        ++ "\nbegin"
        ++ prettyPrintStmt 2 stmt
        ++ "\nend; \n"
    )
  where
    prettyParams = if null val then if null ref then "" else init (init (concatMap ((\(n, t) -> "var " ++ n ++ ": " ++ prettyPrintType t ++ ", ") . snd) ref)) else init (init (concatMap ((\(n, t) -> n ++ ": " ++ prettyPrintType t ++ ", ") . snd) val)) ++ ", " ++ if null ref then "" else init (init (concatMap ((\(n, t) -> "var " ++ n ++ ": " ++ prettyPrintType t ++ ", ") . snd) ref))
    prettyLocalVars = if null localVars then "" else "\n" ++ init (init (concatMap (\(n, t) -> "var " ++ n ++ ": " ++ prettyPrintType t ++ ", ") localVars)) ++ "; "
    val = filter (\(p, _) -> not p) params
    ref = filter fst params

prettyPrintType :: Type -> String
prettyPrintType TInt = "integer"
prettyPrintType TBool = "boolean"

prettyPrintStmt :: Int -> Stmt -> String
prettyPrintStmt i (Block stmts) = concat ["\n" ++ replicate i ' ' ++ prettyPrintStmt i s | s <- stmts]
prettyPrintStmt i (Assign name expr) = name ++ " := " ++ prettyPrintExpr expr
prettyPrintStmt i (IfStmt expr thenStmt elseStmt) = "if " ++ prettyPrintExpr expr ++ "\n" ++ replicate i ' ' ++ "then " ++ prettyPrintStmt i thenStmt ++ "\n" ++ replicate i ' ' ++ "else " ++ prettyPrintStmt i elseStmt
prettyPrintStmt i (While expr stmt) = "while " ++ prettyPrintExpr expr ++ " do" ++ "\n" ++ replicate (i + 2) ' ' ++ "begin" ++ prettyPrintStmt (i + 4) stmt ++ "\n" ++ replicate (i + 2) ' ' ++ "end;"

prettyPrintExpr :: Expr -> String
prettyPrintExpr (IntLiteral value) = show value
prettyPrintExpr (BoolLiteral value) = show value
prettyPrintExpr (VarId name) = name
prettyPrintExpr (UPlus expr) = prettyPrintExpr expr
prettyPrintExpr (UMinus expr) = "-" ++ prettyPrintExpr expr
prettyPrintExpr (UNot expr) = "not " ++ prettyPrintExpr expr
prettyPrintExpr (FunctionCall name exprList) = name ++ "(" ++ init (init (concat [prettyPrintExpr expr ++ ", " | expr <- exprList])) ++ ");"
prettyPrintExpr (BinaryExpr op exprLeft exprRight) = "(" ++ prettyPrintExpr exprLeft ++ " " ++ prettyPrintBinop op ++ " " ++ prettyPrintExpr exprRight ++ ")"
prettyPrintExpr (IfExpr conExpr thenExpr elseExpr) = "if " ++ prettyPrintExpr conExpr ++ "\nthen " ++ prettyPrintExpr thenExpr ++ "\nelse " ++ prettyPrintExpr elseExpr

prettyPrintBinop :: Binop -> String
prettyPrintBinop Plus = "+"
prettyPrintBinop Minus = "-"
prettyPrintBinop Or = "||"
prettyPrintBinop Mult = "*"
prettyPrintBinop Slash = "/"
prettyPrintBinop Div = "/"
prettyPrintBinop Mod = "%"
prettyPrintBinop And = "and"
prettyPrintBinop Eq = "=="
prettyPrintBinop Lt = "<"
prettyPrintBinop Gt = ">"
prettyPrintBinop Ne = "<>"
prettyPrintBinop Le = "<="
prettyPrintBinop Ge = ">="