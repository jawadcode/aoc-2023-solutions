module Main (main) where

import Control.Arrow ((>>>))
import Data.Function ((&))
import System.Environment (getArgs)

oneImpure :: String -> IO ()
oneImpure path = do
  content <- readFile path
  print $ one content

data Entry = Red | Blue | Green deriving (Show)

one :: String -> [(Int, [[(Int, Entry)]])]
one content =
  map (getIdAndData >>> mapSnd parseEntries) (lines content)
  where
    getIdAndData line =
      let (id, entries) = splitOnce ':' line
       in (read $ drop 5 id, splitOn ';' entries & map (map (drop 1) . splitOn ','))
    parseEntries =
      (map . map)
        ( \entry ->
            let (num, colour) = splitOnce ' ' entry
             in ( read num,
                  case length colour of
                    3 -> Red
                    4 -> Blue
                    5 -> Green
                    _ -> error "Unreachable"
                )
        )
    mapSnd f (a, b) = (a, f b)

twoImpure :: String -> IO Int
twoImpure path = undefined

-- splitOn and splitOnce are courtesy of the built-in `words` function, whose
-- source I copied

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where
      (w, s'') = break (== c) s'

splitOnce :: Char -> String -> (String, String)
splitOnce c "" = error "Empty string"
splitOnce c s =
  let (s1, s2) = break (== c) s
   in (s1, drop 1 s2)

main :: IO ()
main = do
  args <- getArgs
  print args
  run args

run :: [String] -> IO ()
run (part : [path]) | part `elem` ["1", "one"] = oneImpure path >>= print
run (part : [path]) | part `elem` ["2", "two"] = twoImpure path >>= print
run _ = usage

{- case args of
    _ : part : [path] | elem (toLower part) ["1", "one"] -> undefined
    _ : part : [path] | elem (toLower part) ["2", "two"] -> undefined
    name : _ -> usage name
    [] -> error "Unreachable" -}

usage = putStrLn $ "Usage: day2 <part> <path/to/input>"
