module Main where

import Lib

import Codec.Picture
import System.Environment

main :: IO ()
main = do
  path:path' <- getArgs
  image <- readImage path
  case image of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right (ImageRGB8 img) -> print $ calcImageHisto img
    Right _ -> putStrLn "Could not read format image"
