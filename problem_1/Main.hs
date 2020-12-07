{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Memo (MonadMemo (..), startEvalMemo)
import Data.List (foldl', sort)
import Data.List.Zipper
  ( Zipper,
    fromList,
    right,
    safeCursor,
    start,
    toList,
  )
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

solve :: (Integral a, MonadMemo (a, a) (Maybe [a]) m) => Zipper a -> (a, a) -> m (Maybe [a])
solve zip (target, depth) = case safeCursor zip of
  Nothing -> return Nothing
  Just a
    | a >= target -> return Nothing
    | depth <= 2 ->
      -- Base case
      if b == a || elem b (toList zip)
        then return $ Just [a, b]
        else memo (solve $ right zip) (target, depth)
    | otherwise ->
      -- General case
      do
        recurse <- memo (solve $ start zip) (b, depth - 1)
        case recurse of
          Just c -> return $ Just (a : c)
          Nothing -> memo (solve $ right zip) (target, depth)
    where
      b = target - a

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> do
      let input_file = head args
      values <- (map read . lines <$> readFile input_file) :: IO [Int]
      let sorted = sort values
      let zipped = fromList sorted :: Zipper Int
      let target = 2020
      let part1 = fmap (foldl' (\acc x -> acc * toInteger x) 1) $ startEvalMemo $ solve zipped (target, 2)
      let part2 = fmap (foldl' (\acc x -> acc * toInteger x) 1) $ startEvalMemo $ solve zipped (target, 3)
      print part1
      print part2
    _ -> hPutStrLn stderr "Usage: [application] [input_file]"
