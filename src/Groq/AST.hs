{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Groq.AST where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Data (Data, Typeable)
import Data.Scientific (Scientific)

-- | Represents a position in the source code.
data Position = Position
  { posStart  :: Int
  , posEnd    :: Int
  , posSource :: Text
  } deriving (Show, Eq, Generic, Data, Typeable)

data Expression
  = Everything
  | This
  | Parent
  | Literal Literal
  | Attribute Text
  | Param Text
  | Array [Expression]
  | Object [ObjectField]
  | Constraint Expression -- ^ A filter/constraint on an expression
  | Subscript Expression -- ^ Accessing a specific element or range via []
  | Range { rangeStart :: Expression, rangeEnd :: Expression, rangeInclusive :: Bool }
  | FunctionCall { funcNamespace :: Maybe Text, funcName :: Text, funcArgs :: [Expression] }
  | BinaryOperator { binOp :: BinaryOp, binLHS :: Expression, binRHS :: Expression }
  | PrefixOperator { preOp :: PrefixOp, preRHS :: Expression }
  | PostfixOperator { postOp :: PostfixOp, postLHS :: Expression }
  | PipeOperator { pipeLHS :: Expression, pipeRHS :: Expression }
  | DotOperator { dotLHS :: Expression, dotRHS :: Expression }
  | Projection { projLHS :: Expression, projObject :: Expression } -- LHS | { ... }
  | Filter { filterLHS :: Expression, filterConstraint :: Expression } -- LHS [ ... ]
  | Element { elementLHS :: Expression, elementIdx :: Expression } -- LHS [0]
  | Slice { sliceLHS :: Expression, sliceRange :: Expression } -- LHS [0..10]
  | Dereference { derefLHS :: Expression, derefAttr :: Maybe Text } -- LHS -> or LHS -> attr
  | ArrayTraversal { arrTravLHS :: Expression } -- LHS []
  | Tuple [Expression]
  | Group Expression
  | Ellipsis
  deriving (Show, Eq, Generic, Data, Typeable)

data ObjectField
  = ObjectAttribute { fieldName :: Text, fieldValue :: Expression }
  | ObjectSpread Expression
  deriving (Show, Eq, Generic, Data, Typeable)

data Literal
  = StringLiteral Text
  | IntegerLiteral Int
  | FloatLiteral Scientific
  | BooleanLiteral Bool
  | NullLiteral
  deriving (Show, Eq, Generic, Data, Typeable)

data BinaryOp
  = OpPlus | OpMinus | OpAsterisk | OpSlash | OpPercent | OpExponentiation
  | OpEquals | OpNotEquals | OpGT | OpGTE | OpLT | OpLTE
  | OpAnd | OpOr | OpIn | OpMatch | OpArrow
  deriving (Show, Eq, Generic, Data, Typeable)

data PrefixOp
  = OpNot | OpUnaryPlus | OpUnaryMinus
  deriving (Show, Eq, Generic, Data, Typeable)

data PostfixOp
  = OpAsc | OpDesc
  deriving (Show, Eq, Generic, Data, Typeable)
