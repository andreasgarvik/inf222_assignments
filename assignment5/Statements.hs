-- | Statement example from inf222-2021-lec0701-grammar_parsing
--
-- 2021-02-18
module Statements where

-- Use grammar tools: acceptor and parser generator.

import Data.Tree (Tree (..), drawTree)
import ParserGenerator

--------------------------

-- | An introductory grammar example.

{-
<stmt> ::= <stmt> ";" <stmt>
<stmt> ::= <ident> ":=" <expr>
<ident> ::= "x"
<expr> ::= <ident>
<expr> ::= <lit>
<lit> ::= "123"
-}
introGrammar = ("stmt", introRules) :: Grammar

introRules = [r2, r1, r3, r4, r5, r6] :: Rules

r1 = ("stmt", [N "stmt", T ";", N "stmt"]) :: Rule

r2 = ("stmt", [N "ident", T ":=", N "expr"]) :: Rule

r3 = ("ident", [T "x"]) :: Rule

r4 = ("expr", [N "ident"]) :: Rule

r5 = ("expr", [N "lit"]) :: Rule

r6 = ("lit", [T "123"]) :: Rule

test_intro = do
  print $ "test_intro"
  if acceptSteps introRules [N "stmt"] ["x", ":=", "123"]
    && acceptSteps introRules [N "stmt"] ["x", ":=", "123", ";", "x", ":=", "x"]
    then -- (not $ acceptSteps introRules [N "stmt"] [])
    -- (not $ acceptSteps introRules [N "stmt"] ["123","while",";",":="])
    -- (not $ acceptSteps introRules [N "stmt"] ["x"])
      print $ "Success"
    else print $ "Expectations not satisfied!"

--------------------------

-- | A very simple statement language: non-empty sequences of skip statements.

{-
<stmt> ::= “skip”
<stmt> ::= <stmt> “;” <stmt>
Defines language: "skip" “skip ; skip” “skip ; skip ; skip” “skip ; skip ; skip ; skip” ...
-}
stmt1 = "skip ; skip"

-- | Grammar giving "infinite" loop for acceptor and parser
stmt1rules :: Rules
stmt1rules =
  [ ("stmt", [N "stmt", T ";", N "stmt"]),
    ("stmt", [T "skip"])
  ]

stmt1grammar :: Grammar
stmt1grammar = ("stmt", stmt1rules)

-- | Grammar for terminating acceptor but gives only partial parse
stmt1rules' :: Rules
stmt1rules' =
  [ ("stmt", [T "skip"]),
    ("stmt", [N "stmt", T ";", N "stmt"])
  ]

stmt1grammar' :: Grammar
stmt1grammar' = ("stmt", stmt1rules')

-- | 5.1 TASK
mystmt1rules :: Rules
mystmt1rules =
  [ ("stmt", [T "skip", T ";", N "stmt"]),
    ("stmt", [T "skip"])
  ]

mystmt1grammar :: Grammar
mystmt1grammar = ("stmt", mystmt1rules)

--------------------------

-- | Running tests on stmt1

-- | Acceptor goes into infinite loop on stmt1.
test_stmt1_accept = do
  print $ "stmt1 = " ++ stmt1
  print $ accept stmt1grammar' words stmt1
  print $ "stmt1 = " ++ stmt1
  print $ accept stmt1grammar words stmt1
  print $ "Done"

test_my_stmt1_accept :: IO ()
test_my_stmt1_accept = do
  print $ "stmt1 = " ++ stmt1
  print $ accept mystmt1grammar words stmt1
  print $ "Done"

-- | Parser recognises only prefix of stmt1.
test_stmt1_parse = do
  print $ "stmt1 = " ++ stmt1
  let pstmt = parse stmt1grammar' words stmt1
  print $ pstmt
  parseDisplay pstmt unwords
  print $ "stmt1 = " ++ stmt1
  let pstmt = parse stmt1grammar words stmt1
  print $ pstmt
  parseDisplay pstmt unwords
  print $ "Done"

test_my_stmt1_parse :: IO ()
test_my_stmt1_parse = do
  print $ "stmt1 = " ++ stmt1
  let pstmt = parse mystmt1grammar words stmt1
  print $ pstmt
  parseDisplay pstmt unwords
  print $ "Done"

--------------------------

-- | Language with some statements and some expressions.
exprGrammar :: Grammar
exprGrammar = ("Expr", exprRules)

exprRules :: Rules
exprRules =
  [ ("NExprList", [N "Expr", T ",", N "NExprList"]),
    ("NExprList", [N "Expr"]),
    ("ExprList", [N "NExprList"]),
    ("ExprList", []),
    ("TExpr", [T "(", N "Expr", T ")"]),
    ("TExpr", [N "LITERAL"]),
    ("TExpr", [T "-", N "Expr"]),
    ("TExpr", [N "IDENTIFIER", T "(", N "ExprList", T ")"]),
    ("TExpr", [N "IDENTIFIER"]),
    ("Expr", [N "TExpr", T "div", N "Expr"]),
    ("Expr", [N "TExpr", T "*", N "Expr"]),
    ("Expr", [N "TExpr", T "-", N "Expr"]),
    ("Expr", [N "TExpr", T "+", N "Expr"]),
    ("Expr", [N "TExpr"])
  ]

exprEx1 = "1 + 2 * 3 - f ( ) - g"

stmtGrammar :: Grammar
stmtGrammar = ("Stmt", stmtRules ++ exprRules)

stmtRules :: Rules
stmtRules =
  [ ("StmtP", [T "skip"]),
    ("StmtP", [T "if", N "Expr", T "then", N "StmtP", T "else", N "StmtP"]),
    ("StmtP", [T "if", N "Expr", T "then", N "StmtP"]),
    ("StmtP", [T "begin", N "Stmt", T "end"]),
    ("StmtP", [N "IDENTIFIER", T ":=", N "Expr"]),
    ("StmtP", [N "IDENTIFIER", T "(", N "ExprList", T ")"]),
    ("Stmt", [N "StmtP", T ";", N "Stmt"]),
    ("Stmt", [N "StmtP"])
  ]

stmtEx1 = "skip ; if b1 then if b2 then proc ( ) else a := 123"

-- | Accepting both examples.
test_accept = do
  print $ "test_accept"
  print $ "expEx1 = " ++ exprEx1
  print $ accept exprGrammar words exprEx1
  print $ "stmtEx1 = " ++ stmtEx1
  print $ accept stmtGrammar words stmtEx1
  print $ "Done"

-- | The example parse trees look strange.
test_parse = do
  print $ "test_parse"
  print $ "expEx1 = " ++ exprEx1
  let pexp = parse exprGrammar words exprEx1
  -- print $ pexp
  parseDisplay pexp unwords
  print $ "stmtEx1 = " ++ stmtEx1
  let pstmt = parse stmtGrammar words stmtEx1
  -- print $ pstmt
  parseDisplay pstmt unwords
  print $ "Done"
