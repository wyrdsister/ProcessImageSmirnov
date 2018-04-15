module Main where

import Lib
import ColorConverter
import Codec.Picture
import System.Environment

main :: IO ()
main = do
  path:path' <- getArgs
  image <- readImage path
  case image of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right img -> saveBmpImage "test.bmp" (ImageRGB8 $ (normalizeImg $ calcImageVCdf (convertRGB8 img)) (convertRGB8 img))

normalizeImg = pixelMap . normalizePixel
