module Language.Hemp.Syntax where

import Data.Text (Text)

data Expr v
  = EApp (Expr v) [Arg v]           -- (f 1 2 x=12)
  | ELam [FormalParam v] (Expr v)   -- (lam a:Int y z => ... )
  | EVar v
  | ELet [Decl v] (Expr v)          -- (let a:Int = 42 in ...)
  | ELit Literal                    -- 42, "hello", 3.5
  | EArrow [(v, Expr v)] (Expr v)   -- {a:Int y:Bool z:Float=>(maybe Float)}
  | EGet v                          -- !foo
  | EAssign v (Expr v) (Expr v)     -- @foo=...;...
  deriving (Eq, Show)

data Arg v = APositional (Expr v) | ANamed v (Expr v)
  deriving (Eq, Show)

data Decl v = Decl v (Maybe (Expr v)) (Expr v)
  deriving (Eq, Show)

data FormalParam v = FormalParam v (Maybe (Expr v)) (Maybe (Expr v))
  deriving (Eq, Show)

data Literal
  = LInt Int
  | LFloat Double
  | LChar Char
  | LString Text
  | LType
  deriving (Eq, Show)