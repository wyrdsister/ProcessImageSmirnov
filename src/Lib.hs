module Lib
    ( calcImageHisto
    ) where

import Codec.Picture
import Data.List

convertToPixels img = pixToTuple <$> [pixelAt img x y | x <- [0 .. imageWidth img - 1], y <- [0 .. imageHeight img - 1]]
  where pixToTuple (PixelRGB8 r g b) = (toInteger r, toInteger g, toInteger b)

calcHisto list = unfoldr generator 0 where
  getValue l x = toInteger $ length $ filter (== x) l
  generator x | x <= 255  = Just (getValue list x,succ x)
        | otherwise = Nothing

calcImageHisto img = (calcHisto rs, calcHisto gs, calcHisto bs) where
  (rs,bs,gs) = unzip3 $ convertToPixels img
