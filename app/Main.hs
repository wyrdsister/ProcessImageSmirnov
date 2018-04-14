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
    Right img -> saveBmpImage "test.bmp" (ImageRGB8 $ (normalizeImg $ calcImageHisto (convertRGB8 img)) (convertRGB8 img))
    Right _ -> putStrLn "Could not read format image"

normalizeImg = pixelMap . normalizePixel
--   where func  (PixelRGB8 r g b)  = PixelRGB8 (smirnov vHist (fromIntegral b))
--                                               (smirnov vHist (fromIntegral g))
--                                               (smirnov vHist (fromIntegral b))
