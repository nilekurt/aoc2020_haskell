module Main where

import qualified Data.Attoparsec.Text as APT
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Either (rights)
import Data.List (foldl', sort)
import Data.Text (Text, pack, unpack)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

parseEntry = do
  first <- APT.takeWhile isDigit
  APT.char '-'
  second <- APT.takeWhile isDigit
  APT.takeWhile isSpace
  c <- APT.anyChar
  APT.char ':'
  APT.takeWhile isSpace
  phrase <- APT.takeWhile isAlphaNum
  return (parse first, parse second, c, unpack phrase)
  where
    parse = read . unpack :: Text -> Int

validate1 (a, b, c, d) = count >= a && count <= b
  where
    count = length (filter (== c) d)

validate2 (a, b, c, d) = xoreq (d !! x) (d !! y) c
  where
    xoreq i j k = (i == k) /= (j == k)
    x = a - 1
    y = b - 1

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> do
      let input_file = head args
      values <- lines <$> readFile input_file
      let parsed = rights $ map (APT.parseOnly parseEntry . pack) values
      let part1 = (length . filter validate1) parsed
      let part2 = (length . filter validate2) parsed
      print part1
      print part2
    _ -> hPutStrLn stderr "Usage: [application] [input_file]"
