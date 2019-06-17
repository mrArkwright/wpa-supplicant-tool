module Main where

import System.Environment

import qualified Lib


main :: IO ()
main = do
  args <- getArgs
  command args

command :: [String] -> IO ()
command ("network" : args) = network args
command (c : _) = putStrLn $ "Unknown command: " ++ c
command _ = putStrLn "No command given"

network :: [String] -> IO ()
network ("add" : args) = addNetwork args
network ("remove": args) = removeNetwork args
network ("list": args) = listNetworks args
network (c : _) = putStrLn $ "Unknown sub-command for network: " ++ c
network _ = putStrLn "Wrong number of arguments for command network"

addNetwork :: [String] -> IO ()
addNetwork [ssid, psk] = Lib.addNetwork ssid psk
addNetwork _ = putStrLn "Wrong number of arguments for command network add"

removeNetwork :: [String] -> IO ()
removeNetwork [ssid] = Lib.removeNetwork ssid
removeNetwork _ = putStrLn "Wrong number of arguments for command network add"

listNetworks :: [String] -> IO ()
listNetworks [] = Lib.listNetworks
listNetworks _ = putStrLn "Wrong number of arguments for command network list"
