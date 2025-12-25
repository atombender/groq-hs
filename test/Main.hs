{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse, ParseErrorBundle)
import Data.Void (Void)
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Groq.Parser as Parser
import qualified Groq.AST as AST

-- Helper to run parser
parseExpr :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) AST.Expression
parseExpr = Parser.parseGroq

main :: IO ()
main = hspec $ do
  describe "GROQ Parser" $ do
    describe "Literals" $ do
      it "parses integer" $ 
        parseExpr "123" `shouldParse` AST.Literal (AST.IntegerLiteral 123)
      
      it "parses float" $ 
        parseExpr "3.14" `shouldParse` AST.Literal (AST.FloatLiteral 3.14)
      
      it "parses float with exponent" $ 
        parseExpr "1.2e-3" `shouldParse` AST.Literal (AST.FloatLiteral 1.2e-3)

      it "parses string (double quote)" $ 
        parseExpr "\"hello\"" `shouldParse` AST.Literal (AST.StringLiteral "hello")
      
      it "parses string (single quote)" $ 
        parseExpr "'world'" `shouldParse` AST.Literal (AST.StringLiteral "world")

      it "parses boolean true" $ 
        parseExpr "true" `shouldParse` AST.Literal (AST.BooleanLiteral True)

      it "parses boolean false" $ 
        parseExpr "false" `shouldParse` AST.Literal (AST.BooleanLiteral False)

      it "parses null" $ 
        parseExpr "null" `shouldParse` AST.Literal (AST.NullLiteral)

    describe "Simple Expressions" $ do
      it "parses everything '*'" $ 
        parseExpr "*" `shouldParse` AST.Everything
      
      it "parses this '@'" $ 
        parseExpr "@" `shouldParse` AST.This
      
      it "parses parent '^'" $ 
        parseExpr "^" `shouldParse` AST.Parent

      it "parses ellipsis '...'" $ 
        parseExpr "..." `shouldParse` AST.Ellipsis
      
      it "parses parameters '$param'" $ 
        parseExpr "$myParam" `shouldParse` AST.Param "myParam"

    describe "Data Structures" $ do
      it "parses array" $ 
        parseExpr "[1, 2, 3]" `shouldParse` AST.Array [
            AST.Literal (AST.IntegerLiteral 1)
          , AST.Literal (AST.IntegerLiteral 2)
          , AST.Literal (AST.IntegerLiteral 3)
          ]
      
      it "parses object" $ 
        parseExpr "{\"a\": 1, b}" `shouldParse` AST.Object [
            AST.ObjectAttribute "a" (AST.Literal (AST.IntegerLiteral 1))
          , AST.ObjectAttribute "b" (AST.Attribute "b")
          ]

    describe "Operators" $ do
      it "parses basic arithmetic" $ 
        parseExpr "1 + 2 * 3" `shouldParse` AST.BinaryOperator {
            AST.binOp = AST.OpPlus
          , AST.binLHS = AST.Literal (AST.IntegerLiteral 1)
          , AST.binRHS = AST.BinaryOperator {
              AST.binOp = AST.OpAsterisk
            , AST.binLHS = AST.Literal (AST.IntegerLiteral 2)
            , AST.binRHS = AST.Literal (AST.IntegerLiteral 3)
            }
          }
      
      it "parses unary operators" $ 
        parseExpr "!true" `shouldParse` AST.PrefixOperator AST.OpNot (AST.Literal (AST.BooleanLiteral True))

      it "parses comparison" $ 
        parseExpr "a >= b" `shouldParse` AST.BinaryOperator {
            AST.binOp = AST.OpGTE
          , AST.binLHS = AST.Attribute "a"
          , AST.binRHS = AST.Attribute "b"
          }

      it "parses logic" $ 
        parseExpr "a && b || c" `shouldParse` AST.BinaryOperator {
            AST.binOp = AST.OpOr
          , AST.binLHS = AST.BinaryOperator {
              AST.binOp = AST.OpAnd
            , AST.binLHS = AST.Attribute "a"
            , AST.binRHS = AST.Attribute "b"
            }
          , AST.binRHS = AST.Attribute "c"
          }

    describe "Traversals" $ do
      it "parses attribute access" $ 
        parseExpr "a.b" `shouldParse` AST.DotOperator (AST.Attribute "a") (AST.Attribute "b")

      it "parses element access" $ 
        parseExpr "a[0]" `shouldParse` AST.Element (AST.Attribute "a") (AST.Literal (AST.IntegerLiteral 0))

      it "parses slice" $ 
        parseExpr "a[0..10]" `shouldParse` AST.Slice (AST.Attribute "a") (AST.Range (AST.Literal (AST.IntegerLiteral 0)) (AST.Literal (AST.IntegerLiteral 10)) True)
      
      it "parses slice exclusive" $ 
        parseExpr "a[0...10]" `shouldParse` AST.Slice (AST.Attribute "a") (AST.Range (AST.Literal (AST.IntegerLiteral 0)) (AST.Literal (AST.IntegerLiteral 10)) False)

      it "parses filter" $ 
        parseExpr "*[_type == 'movie']" `shouldParse` AST.Filter AST.Everything (
            AST.BinaryOperator AST.OpEquals (AST.Attribute "_type") (AST.Literal (AST.StringLiteral "movie"))
          )

      it "parses projection" $ 
        parseExpr "* { title, year }" `shouldParse` AST.Projection AST.Everything (
            AST.Object [
                AST.ObjectAttribute "title" (AST.Attribute "title")
              , AST.ObjectAttribute "year" (AST.Attribute "year")
              ]
          )
      
      it "parses dereference" $ 
        parseExpr "ref->name" `shouldParse` AST.Dereference (AST.Attribute "ref") (Just "name")

      it "parses array postfix" $ 
        parseExpr "foo[]" `shouldParse` AST.ArrayTraversal (AST.Attribute "foo")

    describe "Complex Queries" $ do
      it "parses chained traversals" $ 
        parseExpr "*[_type == 'movie'].cast[0].actor->name" `shouldParse` 
          AST.Dereference (
            AST.DotOperator (
              AST.Element (
                AST.DotOperator (
                  AST.Filter AST.Everything (
                    AST.BinaryOperator AST.OpEquals (AST.Attribute "_type") (AST.Literal (AST.StringLiteral "movie"))
                  )
                ) (AST.Attribute "cast")
              ) (AST.Literal (AST.IntegerLiteral 0))
            ) (AST.Attribute "actor")
          ) (Just "name")

      it "parses pipe order" $ 
         parseExpr "* | order(created desc)" `shouldParse` AST.PipeOperator {
             AST.pipeLHS = AST.Everything
           , AST.pipeRHS = AST.FunctionCall Nothing "order" [
               AST.PostfixOperator AST.OpDesc (AST.Attribute "created")
             ]
           }
      
      it "handles range vs float ambiguity" $ 
        parseExpr "a[0..5]" `shouldParse` AST.Slice (AST.Attribute "a") (
             AST.Range (AST.Literal (AST.IntegerLiteral 0)) (AST.Literal (AST.IntegerLiteral 5)) True
          )
