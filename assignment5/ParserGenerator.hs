-- | Defines a grammar driven language acceptor and parser.
-- The grammar is a choice of start symbol and a list of rules defining context-free languages.
-- No attempt is made to give nice warning messages if the grammar is inconsistent or an accept/parse fails.
-- Also provides some simple utility functions to view the contents of a CST as ASCII graphics tree or a string.
--
-- Authors: Magne Haveraaen & Jaakko Järvi
-- Since 2020-02-12

module ParserGenerator where

-- Use Haskell's general tree library
-- This defines "data Tree a = Node a [Tree a]" and many utility functions.
import Data.Tree (Tree(..), drawTree)

-- Support for Unicode characterisation
import Data.Char ( isAlpha, isAlphaNum, isDigit )

------------------------------

-- | General definition of a context free grammar as Haskell code.
-- We do not declare which terminals and nonterminals are in use as grammar theory defines.
-- Instead we use the actual strings used to represent terminals and nonterminals as the ones declared.
-- This has the possible drawback that a misspelling dramatically may change the intended grammar in unexpected ways.


-- | A grammar defines a start symbol and rules.
type Grammar = (String,Rules)
-- | A Rule is a nonterminal symbol (String) and a list of Grammar symbols.
type Rule = (Nonterminal, [GSymbol])
type Rules = [Rule]
-- | A grammar symbol is either a terminal (T String) or a nonterminal (N String).
-- Note there are two special kinds of nonterminals, the "IDENTIFIER" which matches any identifier and the "LITERAL" which matches any literal.
data GSymbol = T Terminal | N Nonterminal deriving (Show, Eq, Read)
type Terminal = String
type Nonterminal = String

-- | Grammar symbols are displayed BNF style, i.e. a nonterminal is a string in angular brackets and a terminal is a string.
-- Recall to remove deriving Show from GSymbol if activating this alternative.
{-
instance Show GSymbol where
  show (T s) = s
  show (N s) = "<" ++ s ++ ">"
-}

-- | Is a string an identifier
isIdentifier :: String -> Bool
isIdentifier str = str /= "" && isAlpha (head str) && all isAlphaNum str
-- | Is a string a literal
isLiteral :: String -> Bool
isLiteral str = str /= "" && all isDigit str


------------------------------

-- | A lexer takes a string and splits it into a list of tokens.
type Lexer = String -> [Token]

-- | An unlexer takes a list of tokens back to a string.
-- Note that  "unlexer (lexer str)" may not always yield back the same source string.
type Unlexer = [Token] -> String

-- | Property that retockenising a string from an unlexer should be the identity
lexerProperty :: Lexer -> Unlexer -> [Token] -> Bool
lexerProperty lexer unlexer tokens = lexer (unlexer tokens) == tokens
-- | It is important for lexerProperty that the token list is a result of using the proper lexer.
lexerTest :: Lexer -> Unlexer -> String -> Bool
lexerTest lexer unlexer str = lexerProperty lexer unlexer (lexer str)


-- | A token here is just a string.
type Token = String

-- | Implementation of a lexer that splits strings on spaces.
wordlexer :: Lexer
wordlexer str = words str

-- | Unlexer goes from a list of tokens to a string
wordunlexer :: Unlexer
wordunlexer tokens = unwords tokens


------------------------------

-- | An acceptor takes a grammar, a lexer and a string, and tells if the string is in the language defined by the grammar.
-- Note that the input string is split into a list of strings by a "lexer" function given as parameter.
accept :: Grammar -> Lexer -> String -> Bool
accept (start,rules) lexer str = acceptSteps rules [N start] (lexer str)



-- | The call "acceptSteps rules gsyms istrs" checks if the list of grammar symbols gsyms can generate the list istrs.
acceptSteps :: Rules -> [GSymbol] -> [String] -> Bool

-- Both gsyms and istrs are empty: grammar accepts string
acceptSteps _ [] [] = True
-- If the first gsymbol is a terminal, then the first istr must be that same terminal.
acceptSteps rules (T t:gsyms) (t':istrs) | t == t' = acceptSteps rules gsyms istrs
-- If the first gsymbol is the nonterminal "IDENTIFIER", then the first istr will be accepted if it is an identifier.
acceptSteps rules (N "IDENTIFIER":gsyms) (t':istrs) | isIdentifier t' = acceptSteps rules gsyms istrs
-- If the first gsymbol is the nonterminal "LITERAL", then the first istr will be accepted if it is a literal.
acceptSteps rules (N "LITERAL":gsyms) (t':istrs) | isLiteral t' = acceptSteps rules gsyms istrs
-- If the first gsymbol is a nonterminal N, then replace that nonterminal with all possible right-hand side of rules N->rhs.
-- For instance, if the rules contain N->ab, N->aN, and the gsymbols is NxyC, then we try gsymbols abxyC, aNxyC in turn.
acceptSteps rules (N n:gsyms) istrs =
  or (map (\rhs -> acceptSteps rules (rhs++gsyms) istrs) rhss)
  where
    rhss = [ rhs | (n', rhs) <- rules, n == n']
-- Reject if none of the cases above worked out.
acceptSteps _ _ _ = False



------------------------------

-- | The parser creates a concrete syntax tree according to the production rules of the grammar that were used to parse the input string.
-- If the grammar is ambigous, i.e., more than one set of rules could generate the same string, the parser chooses one of those possibilities.
-- Care should be taken to formulate the grammar such that parsing terminates (avoid certain kinds of circularities), and ambiguities,
-- e.g., arising from lack of presedence of operators or prefix constructs ("dangling else" problem).


-- | The concrete syntax tree (CST) data structure.
-- It contains terminal symbols and defines nesting by expansion of nonterminals.
type CST = Tree GSymbol

-- | The parser return type, the unparsed suffix string list and the CST of the parsed prefix.
type Parse = Maybe ([String], CST)


-- | A parser takes a grammar, a lexer, and an input string and returns the CST from the start symbol, and any remaining unparsed part of the input
-- Since parsing may fail, the result is encapsulated by the Maybe monad.
-- Note that the input string is split into a list of strings by the "lexer" function.
parse :: Grammar -> Lexer -> String -> Parse
parse (start,rules) lexer str = parseTree rules (N start) (lexer str)



-- | The parseTree function takes rules, a gsymbol and a list of input strings.
-- If a prefix of the input strings is reachable from the gsymbol (according to the rules), the parseTree function returns the remainder of the input and the CST for the prefix.
parseTree :: Rules -> GSymbol -> [String] -> Parse
-- If the gsymbol is a terminal, then the first istr must be that same terminal.
parseTree _ (T t) (i:istrs) | t == i = Just (istrs, Node (T t) [])
-- If the gsymbol is an indentifier, then the first istr is converted.
parseTree _ (N "IDENTIFIER") (i:istrs) | isIdentifier i = Just (istrs, Node (N "IDENTIFIER") [Node (T i) []])
-- If the gsymbol is an indentifier, then the first istr is converted.
parseTree _ (N "LITERAL") (i:istrs) | isLiteral i = Just (istrs, Node (N "LITERAL") [Node (T i) []])
-- If the gsymbol is a nonterminal, then apply the tryrule function to each of the rules of the grammar, returning the first successful parse.
parseTree rules (N n) istrs = firstJust (map tryrule rules)
  where
    -- In the context of istrs and nonterminal n, for a given rule, if the lhs of the rule matches n, then attempt to parse the input strings 
    tryrule :: Rule -> Parse
    tryrule (n', rhs) = do
      (istrs', csts) <- if (n==n') then parseTrees rules rhs istrs else Nothing
      return (istrs', Node (N n) csts)
-- Parsing failed, return Nothing, thus no error message explaining the problem either.
parseTree _ _ _ = Nothing


-- | The parseTrees function takes rules, a list of gsymbols and a list of input strings.
-- It gobbles up the prefix of the input strings reachable from the gsymbols (neck to neck), returning the remainder of the input and the list of CSTs from the gsymbols.
parseTrees :: Rules -> [GSymbol] -> [String] -> Maybe ([String], [CST])
parseTrees _ [] istrs = return (istrs, [])
parseTrees rules (gsym:gsyms) istrs = do
  -- Parse the prefix of the input strings according to the first gsymbol.
  (istrs', cst) <- parseTree rules gsym istrs
  -- Parse the remaining input strings according to the remaining gsymbols.
  (istrs'', csts) <- parseTrees rules gsyms istrs'
  -- Create the Just value of the unparsed suffix of the input strings and the sequence of parsed CSTs.
  return (istrs'', cst:csts)


-- | Extract the first "Just a" from a list of "Maybe a", return "Nothing" if there is no proper value in the list.
firstJust [] = Nothing
firstJust (Just v:_) = Just v
{- Does not achieve intended effect
firstJust ((Just v@(strs,cst)):ms) =
  if strs == [] then Just v else if rest == Nothing then Just v else rest
    where rest = firstJust (ms)
-}
firstJust (_:ms) = firstJust ms


------------------------------

-- | Support tools for extracting and visualising concrete parse trees from the parsed Maybe construct.

-- | Extract the CST, ignore unparsed suffix, i.e., there may be extra text following the parsed bit of the input text.
parseToCstWeak :: Parse -> CST
parseToCstWeak Nothing = error "No parse"
parseToCstWeak (Just ([], cst)) = cst
parseToCstWeak (Just (strs, cst)) = cst

-- | Extract the CST, stop with an error message if the unparsed suffix string list is non-empty. 
parseToCstStrict :: Parse -> Unlexer -> CST
parseToCstStrict Nothing unlexer = error "No parse"
parseToCstStrict (Just ([], cst)) unlexer = cst
parseToCstStrict (Just (strs, _)) unlexer = error $ "Unparsed suffix: " ++ (unlexer strs)

-- | Display the CST as a tree, i.e., a Data.Tree ASCII graphics.
-- A warning message is provided if there is an unparsed suffix of the input remaining.
parseDisplay :: Parse -> Unlexer -> IO ()
parseDisplay Nothing unlexer = putStrLn "No parse"
parseDisplay (Just ([], cst)) unlexer = displayCst cst
parseDisplay (Just (strs, cst)) unlexer = do
  displayCst cst
  putStrLn $ "Unparsed suffix: " ++ (show $ unlexer strs)


------------------------------

-- | Support tools for visualising concrete parse trees.

-- | Presents a concrete parse tree as ASCII graphics.
displayCst :: Show a => Tree a -> IO ()
displayCst cst = putStrLn $ drawTree (fmap show cst)

-- | Show the CST as a string:
-- Traverse a concrete parse tree and concatenate the terminals in order, giving back a parsable string.
showCst :: CST -> String
showCst (Node (T str) []) = str
showCst (Node (N str) []) = ""
showCst (Node (T str) (e:es)) = str ++ " " ++ showCst e ++ " " ++ showCst (Node (N "temp") es)
showCst (Node (N str) (e:es)) = showCst e ++ " " ++ showCst (Node (N "temp") es)


-- | Printing a parse tree and reparsing it should yield the same parse tree.
-- Note that combining the other way, i.e., going from a string to a parse tree and back to a string, may yield a different string.
showCstProperty :: Grammar -> Lexer -> Unlexer -> CST -> Bool
showCstProperty grammar lexer unlexer cst = parseToCstStrict (parse grammar lexer (showCst cst)) unlexer == cst
-- | It is important for showCstProperty that the CST is a result of parsing with the chosen grammar.
showCstTest :: Grammar -> Lexer -> Unlexer -> String -> Bool
showCstTest grammar lexer unlexer str = if accept grammar lexer str then showCstProperty grammar lexer unlexer (parseToCstStrict (parse grammar lexer str) unlexer) else True


------------------------------

-- | Refactoring tools for grammars and concrete parse trees.
-- Tools include:
-- Going from a list to repeated use of pairs
-- Changing a left recursive grammar to a safe grammar.
-- Providing priority and associativity for operators.
-- Dealing with dangling else problem: 
-- here it is just rearranging the grammar so that the longer rule is tried before the prefix rule: this gives priority to the inner binding of the long form.



------------------------------

-- | Support tools for renaming symbols in grammars and concrete parse trees.
-- This can be useful for keeping symbols separate when combining grammars and controlling the overlap.

-- | Traverses a concrete parse tree and replaces the gsymbols.
-- A call "replaceCst cst gs1 gs2" creates a new parse tree where gs1 is replaced by gs2.
replaceCst :: CST -> GSymbol -> GSymbol -> CST
replaceCst cst gs1@(T str1) gs2@(N str2) = error $ "Syntax rule terminal/nonterminal mismatch: " ++ (show gs1) ++ " " ++ (show gs2)
replaceCst cst gs1@(N str1) gs2@(T str2) = error $ "Syntax rule terminal/nonterminal mismatch: " ++ (show gs1) ++ " " ++ (show gs2)
replaceCst (Node (T str) []) (T str1) (T str2) = if str == str1 then (Node (T str2) []) else (Node (T str) [])
replaceCst (Node (T str) (cst:csts)) _ _ = error $ "Terminal symbol has children: " ++ (show $ cst:csts)
replaceCst (Node (N str) csts) (N str1) (N str2) =
  if str == str1 then (Node (N str2) (replaceCsts csts (N str1) (N str2))) else (Node (N str) (replaceCsts csts (N str1) (N str2)))
replaceCst (Node gsym csts) gs1 gs2 = (Node gsym (map (\cst -> replaceCst cst gs1 gs2) csts))

-- | Replace gsymbols in a list of concrete parse tree.
replaceCsts :: [CST] -> GSymbol -> GSymbol -> [CST]
replaceCsts [] _ _ = []
replaceCsts (cst:csts) gs1 gs2 = replaceCst cst gs1 gs2 : replaceCsts csts gs1 gs2

-- | Replace gsymbols in a rule list. 
-- NB! Will only replace terminals by terminals and non-terminal by non-terminals.
replaceRules :: Rules -> GSymbol -> GSymbol -> Rules
replaceRules rules gs1@(T str1) gs2@(N str2) = error $ "Syntax rule terminal/nonterminal mismatch: " ++ (show gs1) ++ " " ++ (show gs2)
replaceRules rules gs1@(N str1) gs2@(T str2) = error $ "Syntax rule terminal/nonterminal mismatch: " ++ (show gs1) ++ " " ++ (show gs2)
replaceRules [] _ _ = []
replaceRules ((nt,gsyms):rules) gs1@(T str1) gs2@(T str2) =
  (nt,map (\g -> case g of {(T str) -> (T (if str == str1 then str2 else str)) ; _ -> g}) (gsyms)) : replaceRules rules gs1 gs2
replaceRules ((nt,gsyms):rules) gs1@(N str1) gs2@(N str2) =
  (if nt == str1 then str2 else nt,
   map (\g -> case g of { (N str) -> (N (if str == str1 then str2 else str)); _ -> g}) (gsyms)
  ) : replaceRules rules gs1 gs2
 


-- | Replace gsymbols in a grammar list. 
-- NB! Will only replace terminals by terminals and non-terminal by non-terminals.
replaceGrammar :: Grammar -> GSymbol -> GSymbol -> Grammar
replaceGrammar (start,rules) gs1@(T s1) gs2@(T s2) = (start,replaceRules rules gs1 gs2)
replaceGrammar (start,rules) gs1@(N s1) gs2@(N s2) = (start',replaceRules rules gs1 gs2)
  where start' = if start == s1 then s2 else start
replaceGrammar (start,rules) gs1 gs2 = error $ "Cannot replace nonterminal with terminal or vice versa: " ++ (show gs1) ++ " " ++ (show gs2)

-- | The union of two grammars with the new start symbol given as first parameter.
grammarUnion :: String -> Grammar -> Grammar -> Grammar
grammarUnion str (n1,rules1) (n2,rules2) =
  (str,(str,[N n1]):((str,[N n2]):rules1)++rules2)

  
------------------------------

-- | Print nice message rather than just True / False
ok True = putStrLn "OK"
ok False = putStrLn "*** TEST FAILED ***"


