{-# LANGUAGE GADTs #-}
module Main where

import Options.Applicative
import Text.Megaparsec (runParser, errorBundlePretty)
import Jass.Parser (programm)
import System.IO (stderr, hPutStrLn, stdout)
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Jass.JSON
import Jass.Ast (Ast(..), Constant(..), Programm)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Jass.Printer (pretty)
import Data.ByteString.Builder (hPutBuilder)

data Format = ToJass FilePath | ToJson FilePath
  deriving (Eq, Show)

format :: Parser Format
format = subparser
  ( command "to-jass"
      (info (ToJass <$> argument str (metavar "FILE"))
      (progDesc "Convert json to jass"))
  <> command "to-json"
      (info (ToJson <$> argument str (metavar "FILE"))
      (progDesc "Convert jass to json"))
  )

opts = info (format <**> helper)
  ( fullDesc
  <> progDesc "Converts between jass and json"
  <> header "j2j - jass<->json converter"
  )

main = do
  format <- execParser opts
  case format of
    ToJass path -> json2jass path
    ToJson path -> jass2json path

jass2json path = do
  r <- runParser programm path <$> readFile path
  case r of
    Left err -> hPutStrLn stderr $ errorBundlePretty err
    Right ast -> ByteString.putStrLn $ encode ast
  

json2jass path = do
  r1 <- eitherDecode <$> ByteString.readFile path
  case r1 of
    Left err -> hPutStrLn stderr $ unwords ["parsing json:", err]
    Right v -> do
      let  r2 = parseEither pProgram v
      case r2 of
        Left err -> hPutStrLn stderr $ unwords ["constructing ast:", err]
        Right ast -> hPutBuilder stdout $ pretty ast
