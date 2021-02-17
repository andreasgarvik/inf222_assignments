-- 2.2
-- Write a type checker and a type inferencer for “General Extended BTL ASTs”
data Expr
  = Unary String Expr
  | IntLiteral Integer
  | BoolLiteral Bool
  | Varid String
  | FunctionCall String [Expr]
  | BinaryExpr String Expr Expr
  | Ifexpr Expr Expr Expr
  deriving (Show, Eq, Read)

data Type
  = Bool
  | Integer
  | Expr
  | Error
  deriving (Show, Eq, Read)

typeInfer :: Expr -> Type
typeInfer (Unary s e) =
  if typeInfer e == Expr
    then Expr
    else Error
typeInfer (IntLiteral i) = Integer
typeInfer (BoolLiteral b) = Bool
typeInfer (Varid s) = Expr
typeInfer (FunctionCall s args) =
  if all (\e -> typeInfer e == Expr) args
    then Expr
    else Error
typeInfer (BinaryExpr s e1 e2) =
  if typeInfer e1 == typeInfer e2
    then Expr
    else Error
typeInfer (Ifexpr e1 e2 e3) =
  if (typeInfer e1 == Bool) && typeInfer e2 == typeInfer e3
    then Expr
    else Error

-- 2.3
{-
  type objectEntry = string x value
  symbol object: objectEntry* -> value
  symbol array: value* -> value
  symbol string: string -> value
  symbol number: -> string -> integer -> integer -> string -> integer -> value
  symbol true: -> value
  symbol false: -> value
  symbol null: -> value
-}

-- 2.4
type Sign = String

type ESign = String

type ENumber = Integer

type Number = Integer

type Frac = Float

data JSONValue
  = JSONNull
  | JSONBoolean Bool
  | JSONNumber Sign Number Frac ESign ENumber
  | JSONString String
  | JSONArray [JSONValue]
  | JSONObject [(String, JSONValue)]
  deriving (Show)

prettyPrinter :: Int -> JSONValue -> String
prettyPrinter _ JSONNull = "null"
prettyPrinter _ (JSONBoolean b) = if b then "true" else "false"
prettyPrinter _ (JSONNumber s n f es en) = s ++ show n ++ "." ++ drop 2 (show f) ++ "E" ++ es ++ show en
prettyPrinter _ (JSONString s) = "\"" ++ s ++ "\""
prettyPrinter _ (JSONArray []) = "[]"
prettyPrinter i (JSONArray arr) = "[\n" ++ init (init (concat [replicate i ' ' ++ prettyPrinter (i + 2) s ++ ",\n" | s <- arr])) ++ "\n" ++ replicate (i -2) ' ' ++ "]"
prettyPrinter _ (JSONObject []) = "{}"
prettyPrinter i (JSONObject obj) = "{\n" ++ init (init (concat [replicate i ' ' ++ "\"" ++ k ++ "\"" ++ ":" ++ prettyPrinter (i + 2) v ++ ",\n" | (k, v) <- obj])) ++ "\n" ++ replicate (i -2) ' ' ++ "}"

testPrettyPrinter :: IO ()
testPrettyPrinter = do
  let json =
        JSONObject
          [ ("ab", JSONObject [("json", JSONBoolean True), ("XML", JSONBoolean False)]),
            ("b", JSONNull),
            ("cdef", JSONArray [JSONNumber "-" 3 0.3 "-" 10, JSONNull, JSONBoolean False, JSONString "STRING"])
          ]
   in putStrLn (prettyPrinter 2 json)
