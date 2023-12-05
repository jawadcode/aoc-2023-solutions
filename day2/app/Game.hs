{-# LANGUAGE LambdaCase #-}

module Game (one, two) where

import Control.Arrow ((>>>))
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)

one :: String -> Int
one =
  lines
    >>> mapMaybe (getIdAndEntries >>> mapSnd parseEntries >>> validGame)
    >>> sum
  where
    getIdAndEntries = splitOnce ':' >>> mapPair (drop 5 >>> read) splitEntries
    validGame (id, entries) =
      if all
        ( \entry ->
            let (red, green, blue) =
                  foldl'
                    ( \(red, green, blue) -> \case
                        (n, Red) -> (red + n, green, blue)
                        (n, Green) -> (red, green + n, blue)
                        (n, Blue) -> (red, green, blue + n)
                    )
                    (0, 0, 0)
                    entry
             in red <= 12 && green <= 13 && blue <= 14
        )
        entries
        then Just id
        else Nothing

two :: String -> Int
two =
  lines >>> map (getEntries >>> parseEntries >>> minCubes) >>> sumPower
  where
    getEntries = break (== ':') >>> snd >>> drop 1 >>> splitEntries
    minCubes =
      foldl'
        ( foldl'
            ( \(red, green, blue) -> \case
                (num, Red) -> (max red num, green, blue)
                (num, Green) -> (red, max green num, blue)
                (num, Blue) -> (red, green, max blue num)
            )
        )
        (0, 0, 0)
    sumPower = map (\(x, y, z) -> x * y * z) >>> sum

splitEntries :: String -> [[String]]
splitEntries = splitOn ';' >>> map (splitOn ',' >>> map (drop 1))

data Entry = Red | Blue | Green deriving (Show)

parseEntries :: [[String]] -> [[(Int, Entry)]]
parseEntries =
  (map >>> map)
    ( \entry ->
        let (num, colour) = splitOnce ' ' entry
         in ( read num :: Int,
              case length colour of
                3 -> Red
                4 -> Blue
                5 -> Green
                _ -> error "Unreachable"
            )
    )

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where
      (w, s'') = break (== c) s'

splitOnce :: Char -> String -> (String, String)
splitOnce c = break (== c) >>> mapSnd (drop 1)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)
