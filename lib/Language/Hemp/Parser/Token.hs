{-# LANGUAGE OverloadedStrings #-}
module Language.Hemp.Parser.Token where

import Prelude hiding (lex)

import Control.Applicative
import Data.Char

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

newtype PsId = PsId Text

data Token
  = TOpenParen
  | TCloseParen
  | TOpenBrace
  | TCloseBrace
  | TLam
  | TArrow
  | TLet
  | TSemicolon
  | TColon
  | TEquals
  | TBang
  | TAt
  | TVar PsId
  | TInt Int
  | TFloat Double
  | TChar Char
  | TString Text
  | TType

lexToken :: forall e m. (P.MonadParsec e Text m) => m Token
lexToken = P.choice
  [ TOpenParen  <$ sym "("
  , TCloseParen <$ sym ")"
  , TOpenBrace  <$ sym "{"
  , TCloseBrace <$ sym "}"
  , TLam        <$ sym "lam"
  , TArrow      <$ sym "=>"
  , TLet        <$ sym "let"
  , TSemicolon  <$ sym ";"
  , TColon      <$ sym ":"
  , TEquals     <$ sym "="
  , TBang       <$ sym "!"
  , TAt         <$ sym "@"
  , TInt        <$> lex int
  , TFloat      <$> lex float
  , TChar       <$> lex (P.single '\'' *> L.charLiteral <* P.single '\'')
  , TString . Text.pack <$> lex (P.single '"' *> P.many L.charLiteral <* P.single '"')
  , TType       <$ sym "TYPE"
  , TVar . PsId <$> lex var
  ]
  where
    spc :: m ()
    spc = L.space P.space1 (L.skipLineComment "--") empty
    lex :: m a -> m a
    lex = L.lexeme spc
    sym = L.symbol spc

    int = L.signed empty $ P.choice
      [ L.decimal
      , P.string "0b" *> L.binary
      , P.string "0x" *> L.hexadecimal
      ]
    
    float = L.signed empty L.float

    var = Text.cons
      <$> P.satisfy idStartChar
      <*> P.takeWhileP Nothing idChar

    idStartChar, idChar :: Char -> Bool
    idStartChar c = notElem @[] c "!@'\"-" && idChar c
    idChar c = notElem @[] c "(){}[]=" &&
      ( isLetter c
      || isMark c
      || isNumber c
      || isPunctuation c
      || isSymbol c
      )