module Transact.SQL.Parse where

import qualified Data.Char as C

parseError :: [Token] -> a
parseError tokens = error $ "Parse error. Tokens: " ++ (show tokens)

data Token
  = T'Select
  | T'From
  | T'Where
  | T'As
  | T'Comma
  | T'StatementEnd
  | T'Identifier String
  | T'QuotedString String
  | T'Bool Bool
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | C.isSpace c = lexer cs
      | C.isAlpha c = lexAlphaString (c:cs)
lexer ('\'':cs) = lexQuotedString cs
lexer (',':cs) = T'Comma : lexer cs
lexer (';':cs) = T'StatementEnd : lexer cs
lexer _ = error "Cannot lex the input."

-- | This is called whenever an alphabetical character is encountered
-- after a white space. Notice that this doesn't find quoted strings
-- like "Bob" in the SQL code: WHERE Customer.FirstName = "Bob".
lexAlphaString cs =
   let (token, rest) = span C.isAlphaNum cs
       tokenLower = (map C.toLower token)
   in case tokenLower of
      "select" -> T'Select : lexer rest
      "from"   -> T'From : lexer rest
      "where"  -> T'Where : lexer rest
      "as"     -> T'As : lexer rest
      "true"   -> T'Bool True : lexer rest
      "false"  -> T'Bool False : lexer rest
      _        -> T'Identifier token : lexer rest

-- | For now, we don't support quoted strings containing escape characters.
-- That is, we interpret any instances of \ literally. The input string
-- should no longer has the first quotation mark present.
lexQuotedString cs =
   let (token, rest) = span (/='\'') cs
   in T'QuotedString token : lexer (safePop rest)

-- | Pop off the first element, or do nothing if the list is empty
safePop [] = []
safePop (c:cs) = cs
