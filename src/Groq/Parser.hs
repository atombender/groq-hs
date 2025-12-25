{-# LANGUAGE OverloadedStrings #-}

module Groq.Parser (parseGroq, expr) where

import Control.Monad ()
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Groq.AST as AST
import Data.Scientific (Scientific)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Reserved words
rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy (alphaNumChar <|> char '_'))

-- | Identifiers
identifier :: Parser Text
identifier = (lexeme . try) $ do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_')
  return $ T.pack (c:cs)

-- | Literals
stringLiteral :: Parser AST.Literal
stringLiteral = AST.StringLiteral . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"') <|> char '\'' *> manyTill L.charLiteral (char '\''))

parseNumber :: Parser AST.Literal
parseNumber = do
  -- Parse integer part
  i <- L.decimal
  -- Check for dot
  (do
    try $ do
      _ <- char '.'
      notFollowedBy (char '.') -- crucial: if next is '.', back off!
    -- It is a decimal point
    frac <- L.decimal
    -- Combine
    let f = read (show i ++ "." ++ show frac) :: Scientific
    -- Optional exponent
    expPart <- optional ((char 'e' <|> char 'E') *> L.signed sc L.decimal)
    let final = case expPart of
                  Nothing -> f
                  Just e -> f * (10 ^^ e)
    return $ AST.FloatLiteral final
   ) <|> (do
     -- No dot or dot followed by dot
     -- Check exponent for integer
     expPart <- optional ((char 'e' <|> char 'E') *> L.signed sc L.decimal)
     case expPart of
       Nothing -> return $ AST.IntegerLiteral i
       Just e -> return $ AST.FloatLiteral (fromIntegral i * (10 ^^ e))
   )

booleanLiteral :: Parser AST.Literal
booleanLiteral = (rword "true" *> pure (AST.BooleanLiteral True))
             <|> (rword "false" *> pure (AST.BooleanLiteral False))

nullLiteral :: Parser AST.Literal
nullLiteral = rword "null" *> pure AST.NullLiteral

literal :: Parser AST.Expression
literal = AST.Literal <$> lexeme (try parseNumber <|> stringLiteral <|> booleanLiteral <|> nullLiteral)

-- | Terms

term :: Parser AST.Expression

term = choice

  [ parenthesized

  , AST.Everything <$ symbol "*"

  , AST.This <$ symbol "@"

  , AST.Parent <$ symbol "^"

  , AST.Ellipsis <$ symbol "..."

  , AST.Param <$> (char '$' *> identifier)
  , try functionCall
  , literal
  , AST.Attribute <$> identifier
  , parseArray
  , parseObject
  ]

-- | Parenthesized Expression (Group or Tuple)
parenthesized :: Parser AST.Expression
parenthesized = do
  es <- parens (expr `sepBy1` symbol ",")
  case es of
    [e] -> return $ AST.Group e
    es' -> return $ AST.Tuple es'

-- | Function Call
functionCall :: Parser AST.Expression
functionCall = do
  ns <- optional (try (identifier <* symbol "::"))
  name <- identifier
  args <- parens (expr `sepBy` symbol ",")
  return $ AST.FunctionCall ns name args

-- | Array
parseArray :: Parser AST.Expression
parseArray = AST.Array <$> brackets (expr `sepBy` symbol ",")

-- | Object
parseObject :: Parser AST.Expression
parseObject = AST.Object <$> braces (objectField `sepBy` symbol ",")

objectField :: Parser AST.ObjectField
objectField = try spread <|> attribute
  where
    spread = AST.ObjectSpread <$> (symbol "..." *> expr)
    attribute = do
      key <- (AST.StringLiteral <$> lexeme (char '"' *> (T.pack <$> manyTill L.charLiteral (char '"')))) <|> (AST.StringLiteral <$> identifier)
      val <- optional (symbol ":" *> expr)
      case (key, val) of
        (AST.StringLiteral k, Just v) -> return $ AST.ObjectAttribute k v
        (AST.StringLiteral k, Nothing) -> return $ AST.ObjectAttribute k (AST.Attribute k) -- shorthand
        _ -> fail "Invalid object key"

-- | Operator Table
operatorTable :: [[Operator Parser AST.Expression]]
operatorTable = 
  [
    [ Prefix (AST.PrefixOperator AST.OpNot <$ symbol "!")
    , Prefix (AST.PrefixOperator AST.OpUnaryPlus <$ symbol "+")
    , Prefix (AST.PrefixOperator AST.OpUnaryMinus <$ symbol "-")
    ]
  , [ InfixR (AST.BinaryOperator AST.OpExponentiation <$ symbol "**") ]
  , [ InfixL (AST.BinaryOperator AST.OpAsterisk <$ symbol "*")
    , InfixL (AST.BinaryOperator AST.OpSlash <$ symbol "/")
    , InfixL (AST.BinaryOperator AST.OpPercent <$ symbol "%")
    ]
  , [ InfixL (AST.BinaryOperator AST.OpPlus <$ symbol "+")
    , InfixL (AST.BinaryOperator AST.OpMinus <$ symbol "-")
    ]
  , [ InfixL (AST.BinaryOperator AST.OpIn <$ symbol "in")
    , InfixL (AST.BinaryOperator AST.OpMatch <$ symbol "match")
    ]
  , [ InfixL (AST.BinaryOperator AST.OpLTE <$ symbol "<=")
    , InfixL (AST.BinaryOperator AST.OpLT <$ symbol "<")
    , InfixL (AST.BinaryOperator AST.OpGTE <$ symbol ">=")
    , InfixL (AST.BinaryOperator AST.OpGT <$ symbol ">")
    ]
  , [ InfixL (AST.BinaryOperator AST.OpEquals <$ symbol "==")
    , InfixL (AST.BinaryOperator AST.OpNotEquals <$ symbol "!=")
    ]
  , [ InfixL (AST.BinaryOperator AST.OpAnd <$ symbol "&&") ]
  , [ InfixL (AST.BinaryOperator AST.OpOr <$ symbol "||") ]
  , [ InfixL (AST.PipeOperator <$ symbol "|") ]
  , [ Postfix (AST.PostfixOperator AST.OpAsc <$ symbol "asc")
    , Postfix (AST.PostfixOperator AST.OpDesc <$ symbol "desc")
    ]
  ]

-- | Expression parser with postfix handling (traversals)
expr :: Parser AST.Expression
expr = makeExprParser termWithTraversals operatorTable

termWithTraversals :: Parser AST.Expression
termWithTraversals = do
  base <- term
  traversals base

traversals :: AST.Expression -> Parser AST.Expression
traversals base = (do
    t <- traversal
    traversals (applyTraversal base t)
  ) <|> pure base

data Traversal
  = TDot Text
  | TFilter AST.Expression
  | TElement AST.Expression
  | TSlice AST.Expression AST.Expression Bool -- start, end, inclusive
  | TArrayPostfix
  | TProjection AST.Expression
  | TDereference (Maybe Text)

traversal :: Parser Traversal
traversal = choice
    [ try (TDot <$> (symbol "." *> identifier))
    , try (symbol "[" *> symbol "]" *> pure TArrayPostfix)
    , brackets bracketContent
    , TDereference <$> (symbol "->" *> optional identifier)
    , TProjection <$> AST.Object <$> braces (objectField `sepBy` symbol ",")
    ]

-- | Disambiguate [...] content
bracketContent :: Parser Traversal
bracketContent = do
  e <- expr
  -- check for range
  (do
      op <- (try (symbol "...") *> pure False) <|> (symbol ".." *> pure True)
      end <- expr
      return $ TSlice e end op
    ) <|> (return $ disambiguateBracket e)

disambiguateBracket :: AST.Expression -> Traversal
disambiguateBracket e@(AST.Literal (AST.IntegerLiteral _)) = TElement e
disambiguateBracket (AST.Literal (AST.StringLiteral s)) = TDot s -- ["foo"] is .foo
disambiguateBracket e = TFilter e

applyTraversal :: AST.Expression -> Traversal -> AST.Expression
applyTraversal base (TDot t) = AST.DotOperator base (AST.Attribute t)
applyTraversal base (TFilter e) = AST.Filter base e
applyTraversal base (TElement e) = AST.Element base e
applyTraversal base (TSlice start end incl) = AST.Slice base (AST.Range start end incl)
applyTraversal base TArrayPostfix = AST.ArrayTraversal base
applyTraversal base (TProjection (AST.Object obj)) = AST.Projection base (AST.Object obj)
applyTraversal base (TProjection _) = AST.Projection base AST.Everything -- fallback or error?
applyTraversal base (TDereference attr) = AST.Dereference base attr

parseGroq :: Text -> Either (ParseErrorBundle Text Void) AST.Expression
parseGroq = parse (sc *> expr <* eof) ""
