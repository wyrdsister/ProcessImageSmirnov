module Lib
      -- calcImageHisto,
      -- cdf,
      -- smirnov,
      -- normalizePixel
     where

import Codec.Picture
import Data.List
import ColorConverter

convertToPixels img = pixToRGB <$> [pixelAt img x y | x <- [0 .. imageWidth img - 1], y <- [0 .. imageHeight img - 1]]
pixToRGB (PixelRGB8 r g b) = rgb (toInteger r) (toInteger g) (toInteger b)

calcHisto list = unfoldr generator 0 where
  getValue l x = fromIntegral $ length $ filter (\y -> abs (y - x) <= 0.001) l
  generator x | x <= 1  = Just (getValue list x,x+0.001)
        | otherwise = Nothing

calcImageHisto img = calcHisto vs where
  vs = hsvV . rgbToHSV <$> convertToPixels img

cdf :: [Double] -> [Double]
cdf l = ( / maxEl) <$> cdf'
  where cdf' = reverse $ foldl (\acc x ->  (head acc + x):acc) [head l] (tail l)
        maxEl = maximum cdf'

smirnov list x = (cdf' !! (floor $ x * 255))
  -- where cdf' = floor . (* (fromIntegral $ length list - 1)) <$> cdf list
  where cdf' = cdf list


normalizePixel vHist pix  = PixelRGB8 (floor r*255) (floor g*255) (floor b*255)
  where hsvPix = rgbToHSV (pixToRGB pix)
        newV = smirnov vHist $ hsvV hsvPix
        RGB r g b = hsvToRGB hsvPix { hsvV = newV }
