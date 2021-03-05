module Language.Hemp.Parser where

import qualified Data.List.NonEmpty as NE

import qualified Data.Set as Set
import qualified Text.Megaparsec as P

import Language.Hemp.Parser.Token
import Language.Hemp.Syntax

parseExpr :: P.MonadParsec e [Token] m => m (Expr PsId)
parseExpr = P.choice
  [ parens $ EApp <$> parseExpr <*> P.many parseArg
  , parens do
    P.single TLam
    params <- P.many parseFormalParam
    P.single TArrow
    e <- parseExpr
    pure (ELam params e)
  , EVar <$> parseId
  , parens do
      P.single TLet
      decls <- parseDecl `P.sepEndBy` P.single TSemicolon
      e <- parseExpr
      pure (ELet decls e)
  , ELit <$> parseLiteral
  , braces $ EArrow <$> P.many typedArg <* P.single TArrow <*> parseExpr
  , EGet <$> do P.single TBang *> parseId
  , do
    P.single TAt
    i <- parseId
    P.single TEquals
    v <- parseExpr
    P.single TSemicolon
    e <- parseExpr
    pure (EAssign i v e)
  ]
  where
    parens = P.between (P.single TOpenParen) (P.single TCloseParen)
    braces = P.between (P.single TOpenBrace) (P.single TCloseBrace)
    typedArg = (,) <$> parseId <* P.single TColon <*> parseExpr

parseArg :: P.MonadParsec e [Token] m => m  (Arg PsId)
parseArg = P.choice
  [ ANamed <$> parseId <* P.single TEquals <*> parseExpr
  , APositional <$> parseExpr
  ]

parseFormalParam :: P.MonadParsec e [Token] m => m  (FormalParam PsId)
parseFormalParam = do
  name <- parseId
  ty <- P.optional (P.single TColon *> parseExpr)
  def <- P.optional (P.single TEquals *> parseExpr)
  pure (FormalParam name ty def)

parseDecl :: P.MonadParsec e [Token] m => m  (Decl PsId)
parseDecl = Decl <$> parseId <*> P.optional (P.single TColon *> parseExpr) <*> parseExpr

parseLiteral :: P.MonadParsec e [Token] m => m  Literal
parseLiteral = P.token
 \case
    TInt x -> Just (LInt x)
    TFloat x -> Just (LFloat x)
    TChar x -> Just (LChar x)
    TString x -> Just (LString x)
    TType -> Just LType
    _ -> Nothing
  do Set.fromList
      [ P.Label (NE.fromList "integer literal")
      , P.Label (NE.fromList "float literal")
      , P.Label (NE.fromList "character literal")
      , P.Label (NE.fromList "string literal")
      , P.Label (NE.fromList "TYPE")
      ]

parseId :: P.MonadParsec e [Token] m => m  PsId
parseId = P.token
  \case
    TVar x -> Just x
    _ -> Nothing
  do Set.singleton (P.Label (NE.fromList "identifier"))