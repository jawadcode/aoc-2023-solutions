module Main (main) where

import Data.Functor ((<&>))
import Game
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run (part : [path]) | part `elem` ["1", "one"] = oneImpure path >>= print
run (part : [path]) | part `elem` ["2", "two"] = twoImpure path >>= print
run _ = usage

oneImpure :: String -> IO Int
oneImpure path = readFile path <&> Game.one

twoImpure :: String -> IO Int
twoImpure path = readFile path <&> Game.two

usage = putStrLn "Usage: day2 <part> <path/to/input>"
