module Lib (addNetwork, removeNetwork, listNetworks) where

import Control.Monad.Writer.Lazy
import Text.Parsec hiding (State)
import qualified Text.Parsec.Char as Parsec
import Text.Parsec.String (Parser)
import System.Console.GetOpt


data ConfigEntry = Network String String | OtherContent String
  deriving (Show)

type Config = [ConfigEntry]

parser :: Parser Config
parser = many parseConfigEntry

parseConfigEntry :: Parser ConfigEntry
parseConfigEntry = parseNetwork <|> parseOtherContent

parseNetwork :: Parser ConfigEntry
parseNetwork = do

  try $ Parsec.string "network={"
  Parsec.newline

  Parsec.char '\t'
  Parsec.string "ssid="
  ssid <- parseQuotedString
  Parsec.newline

  Parsec.char '\t'
  Parsec.string "psk="
  psk <- parseQuotedString
  Parsec.newline

  Parsec.char '}'

  optional $ Parsec.newline
  optional $ Parsec.newline

  return $ Network ssid psk

parseQuotedString :: Parser String
parseQuotedString = between (char '"') (char '"') $ many $ satisfy (\c -> (c /= '"') && (c > '\026'))

parseOtherContent :: Parser ConfigEntry
parseOtherContent = do
  content <- many $ Parsec.satisfy (\c -> c /= '\n')
  Parsec.newline
  return $ OtherContent content

assembleConfig :: [ConfigEntry] -> String
assembleConfig configEntries = foldl (\s entry -> s ++ assembleConfigEntry entry ++ "\n") "" configEntries

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

assembleConfigEntry :: ConfigEntry -> String
assembleConfigEntry (OtherContent content) = content
assembleConfigEntry (Network ssid psk) = execWriter $ do

  tell "network={"
  tell "\n"

  tell "\t"
  tell "ssid="
  tell $ quoted ssid
  tell "\n"

  tell "\t"
  tell "psk="
  tell $ quoted psk
  tell "\n"

  tell "}"
  tell "\n"

fileName :: String
fileName = "/etc/wpa_supplicant/wpa_supplicant.conf"

parseFile :: IO (Either ParseError Config)
parseFile = do
  source <- readFile fileName
  return $ parse parser fileName source

addNetwork :: String -> String -> IO ()
addNetwork ssid psk = do

  result <- parseFile

  case result of

    Left errorMessage -> putStrLn $ "parse error:" ++ show errorMessage

    Right configEntries -> do
      let configEntries' = updateOrAddNetwork ssid psk configEntries
      let output = assembleConfig configEntries'
      writeFile fileName output

removeNetwork :: String -> IO ()
removeNetwork ssid = do

  result <- parseFile

  case result of

    Left errorMessage -> putStrLn $ "parse error:" ++ show errorMessage

    Right configEntries -> do
      let configEntries' = removeNetwork' ssid configEntries
      let output = assembleConfig configEntries'
      writeFile fileName output

listNetworks :: IO ()
listNetworks = do

  result <- parseFile

  case result of

    Left errorMessage -> putStrLn $ "parse error:" ++ show errorMessage

    Right configEntries -> do
      let ssids = listNetworks' configEntries
      mapM_ putStrLn ssids

listNetworks' :: Config -> [String]
listNetworks' = foldl getSsid [] where
  getSsid :: [String] -> ConfigEntry -> [String]
  getSsid ssids (OtherContent _) = ssids
  getSsid ssids (Network ssid _) = ssid : ssids

updateOrAddNetwork :: String -> String -> Config -> Config
updateOrAddNetwork ssid psk [] = [Network ssid psk]
updateOrAddNetwork ssid psk (x@(OtherContent _) : xs) = x : updateOrAddNetwork ssid psk xs
updateOrAddNetwork ssid psk (x@(Network ssid' _) : xs)
  | ssid == ssid' = Network ssid psk : xs
  | otherwise = x : updateOrAddNetwork ssid psk xs

removeNetwork' :: String -> Config -> Config
removeNetwork' ssid [] = []
removeNetwork' ssid (x@(OtherContent _) : xs) = x : removeNetwork' ssid xs
removeNetwork' ssid (x@(Network ssid' _) : xs)
  | ssid == ssid' = removeNetwork' ssid xs
  | otherwise = x : removeNetwork' ssid xs

