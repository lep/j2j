{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jass.JSON where

import Jass.Ast hiding (traverse, fmap)
import Data.Aeson as Aeson hiding (Bool, Null, String)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parse, Parser)
import Control.Monad (guard)
import Data.Text (Text)
import Data.Vector ((!))
import Data.Foldable (toList)
import Debug.Trace
import Data.ByteString.Char8 (ByteString)

instance ToJSON var => ToJSON (Ast var x) where
  toJSON x =
    case x of
      SDef constantness name ty init ->
        object [
          "type" .= Aeson.String "vardef",
          "kind" .= Aeson.String "sdef",
          "constant" .= (constantness == Const),
          "name" .= name,
          "vartype" .= ty,
          "expr" .= init
        ]

      ADef name ty ->
        object [
          "type" .= Aeson.String "vardef",
          "kind" .= Aeson.String "adef",
          "vartype" .= ty,
          "name" .= name
        ]

      AVar name idx ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "avar",
          "name" .= name,
          "index" .= idx
        ]

      SVar name ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "svar",
          "name" .= name
        ]

      Var lvar -> toJSON lvar
      Null ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "null"
        ]
      Code fn ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "code",
          "name" .= fn
        ]

      String s ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "string",
          "value" .= s
        ]

      Bool b ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "bool",
          "value" .= b
        ]
      Real v ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "real",
          "value" .= v
        ]
      Rawcode v ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "rawcode",
          "value" .= v
        ]
      Int v ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "integer",
          "value" .= v
        ]

      Call name args ->
        object [
          "type" .= Aeson.String "call",
          "kind" .= Aeson.String "call",
          "name" .= name,
          "args" .= args
        ]

      Return v ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "return",
          "value" .= v
        ]
      Exitwhen v ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "exitwhen",
          "value" .= v
        ]
      Loop body ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "loop",
          "body" .= body
        ]

      If cond thenBody eifs elseB ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "if",
          "cond" .= cond,
          "then" .= thenBody,
          "elseifs" .= eifs,
          "else" .= elseB
        ]

      Local vdef ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "local",
          "def" .= vdef
        ]
      Set lvar expr ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "set",
          "lvar" .= lvar,
          "expr" .= expr
        ]
      Typedef name base ->
        object [
          "type" .= Aeson.String "toplevel",
          "kind" .= Aeson.String "typedef",
          "name" .= name,
          "base" .= base
        ]
      Global vdef ->
        object [
          "type" .= Aeson.String "toplevel",
          "kind" .= Aeson.String "global",
          "def" .= vdef
        ]
      Function constantness name params returnType body ->
        object [
          "type" .= Aeson.String "toplevel",
          "kind" .= Aeson.String "function",
          "name" .= name,
          "constant" .= (constantness == Const),
          "parameters" .= params,
          "returntype" .= returnType,
          "body" .= body
        ]
      Native constantness name params returnType ->
        object [
          "type" .= Aeson.String "toplevel",
          "kind" .= Aeson.String "native",
          "constant" .= (constantness == Const),
          "name" .= name,
          "parameters" .= params,
          "returntype" .= returnType
        ]

      Programm ts -> toJSON $ map toJSON ts

-- pLVar :: FromJSON var => Value -> Parser (Ast var LVar)
-- pLVar = withObject "lvar" $ \o -> do
-- TODO: maybe refactor the json output to have "lvar" as a sub-object
-- so this can use the usual "withObject" spiel.
pLVar :: FromJSON var => Object -> Parser (Ast var LVar)
pLVar o = do
  kind <- o .: "kind" :: Parser Text
  ty <- o .: "type" :: Parser Text
  guard $ ty == "expr"
  case kind of
    "svar" -> SVar <$> o .: "name"
    "avar" -> do
      name <- o .: "name"
      idx <- pExpr =<< o .: "index"
      pure $ AVar name idx
  

pExpr :: FromJSON var => Value -> Parser (Ast var Expr)
pExpr = withObject "expr" $ \v -> do
  ty <- v .: "type" :: Parser Text
  kind <- v .: "kind" :: Parser Text
  guard $ ty == "expr" || ty == "call"
  case kind of
    "string" -> String <$> v .: "value"
    "bool" -> Bool <$> v .: "value"
    "real" -> Real <$> v .: "value"
    "integer" -> Int <$> v .: "value"
    "rawcode" -> Rawcode <$> v .: "value"
    "code" -> Code <$> v .: "name"
    "null" -> pure Null
    "call" -> do
      name <- v .: "name"
      args <- mapM pExpr =<< v .: "args"
      pure $ Call name args
    "avar" -> Var <$> pLVar v
    "svar" -> Var <$> pLVar v

pStmt :: FromJSON var => Value -> Parser (Ast var Stmt)
pStmt = withObject "stmt" $ \v -> do
  ty <- v .: "type" :: Parser Text
  kind <- v .: "kind" :: Parser Text
  guard $ ty == "stmt" || ty == "call"
  case kind of
    "return" -> do
      r <- v .: "value"
      r' <- traverse pExpr r
      pure $ Return r'

    "call" -> do
      name <- v .: "name"
      args <- mapM pExpr =<< v .: "args"
      pure $ Call name args

    "exitwhen" -> do
      cond <- pExpr =<< v .: "value"
      pure $ Exitwhen cond

    "loop" -> do
      body <- mapM pStmt =<< v .:" body"
      pure $ Loop body

    "if" -> do
      cond <- pExpr =<< v .: "cond"
      thenBody <- mapM pStmt =<< v .: "then"
      elseifs <-  mapM pElseIf =<< v .: "elseifs"
      elseBody <- traverse (mapM pStmt) =<< v .: "else"

      pure $ If cond thenBody elseifs elseBody

    "local" -> do
      o <- v .: "def"
      def <- pVdef o
      pure $ Local def

    "set" -> do
      lvar <- pLVar =<< v .: "lvar"
      expr <- pExpr =<< v .: "expr"
      pure $ Set lvar expr
      



pElseIf :: FromJSON var => Value -> Parser (Ast var Expr, [Ast var Stmt])
pElseIf = withArray "elseif" $ \a -> do
  cond <- pExpr $ a ! 0
  body <- withArray "elseif-body" `flip` (a!1) $ \a' ->
    mapM pStmt a'
  pure (cond, toList body)


pVdef :: FromJSON var => Value -> Parser (Ast var VarDef)
pVdef = withObject "vardef" $ \o -> do
  ty <- o .: "type" :: Parser Text
  kind <- o .: "kind" :: Parser Text
  guard $ ty == "vardef"
  case kind of
    "adef" -> ADef <$> o .: "name" <*> o .: "vartype"
    "sdef" -> do
      constant <- o .: "constant"
      name <- o .: "name"
      vty <- o .: "vartype"
      init <- traverse pExpr =<< o .: "expr"
      -- let init = Nothing
      pure $ SDef (if constant then Const else Normal) name vty init
      

pToplevel :: FromJSON var => Value -> Parser (Ast var Toplevel)
pToplevel = withObject "toplevel" $ \o -> do
  ty <- o .: "type" :: Parser Text
  kind <- o .: "kind" :: Parser Text
  guard $ ty == "toplevel"
  case kind of
    "typedef" -> Typedef <$> o .: "name" <*> o .: "base"
    "global" -> do
      def <- pVdef =<< o .: "def"
      pure $ Global def
    "native" -> do
      constant <- o .: "constant"
      name <- o .: "name"
      parameters <- o .: "parameters"
      returnType <- o .: "returntype"
      pure $ Native (if constant then Const else Normal) name parameters returnType
    "function" -> do
      constant <- o .: "constant"
      name <- o .: "name"
      parameters <- o .: "parameters"
      returnType <- o .: "returntype"
      body <- mapM pStmt =<< o .: "body"
      pure $ Function (if constant then Const else Normal) name parameters returnType body

pProgram :: FromJSON var => Value -> Parser (Ast var Programm)
pProgram = withArray "program" $ \a -> do
  ts <- toList <$> mapM pToplevel a
  pure $ Programm ts
  

