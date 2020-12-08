module Main where

import Data.List (foldl')
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

solve :: Integral a => [String] -> Int -> Int -> Int -> Int -> a
solve [] _ _ _ _ = 0
solve ls x y dx dy
  | y < height =
    solve ls (x + dx) (y + dy) dx dy + case line !! index of
      '#' -> 1
      _ -> 0
  | otherwise = 0
  where
    height = length ls
    line = ls !! y
    width = length line
    index = x `rem` width

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> do
      let input_file = head args
      values <- lines <$> readFile input_file
      let part1 = solve values 0 0 3 1
      let part2_inputs = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
      let part2 = foldl' (\acc x -> acc * (uncurry $ solve values 0 0) x) 1 part2_inputs
      print part1
      print part2
    _ -> hPutStrLn stderr "Usage: [application] [input_file]"
