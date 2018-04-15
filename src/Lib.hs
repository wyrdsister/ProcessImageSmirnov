module Lib
      -- calcImageHisto,
      -- cdf,
      -- smirnov,
      -- normalizePixel



     where

import Codec.Picture
import Data.List
import ColorConverter
import Data.Maybe

data Cdf' = Cdf { leftBound :: Double, count :: Double }  deriving (Show, Eq)

scale factor cdf = cdf { count = count cdf * factor }

instance Ord Cdf' where
  compare x y = compare (count x) (count y)

type Cdf = [Cdf']

calcCdf2 :: Double -> [Double] -> Cdf
calcCdf2 binSize list =  (scale factor) <$> cdfList where
  cdfList = unfoldr generator 0
  factor = recip $ maximum $ count <$> cdfList
  getValue l x = Cdf { count = fromIntegral $ length $ filter (<= x) l,
                       leftBound = x
                     }
  generator x | x <= 1  = Just (getValue list x,x+binSize)
              | otherwise = Nothing

getValFromCdf cdf x = count <$> cdfBin where
  filtred = filter (\x'-> leftBound x' < x) cdf
  cdfBin = if null filtred then Nothing
           else Just $ maximum filtred

convertToPixels img = pixToRGB <$> [pixelAt img x y | x <- [0 .. imageWidth img - 1], y <- [0 .. imageHeight img - 1]]
pixToRGB (PixelRGB8 r g b) = rgb (toInteger r) (toInteger g) (toInteger b)


smirnov :: Cdf  -> Double -> Double
-- smirnov cdf x = fromMaybe 0.0 $ getValFromCdf cdf x
smirnov cdf x = let Just val = getValFromCdf cdf x in val

normalizePixel :: Cdf -> PixelRGB8 -> PixelRGB8
normalizePixel vHist pix  = PixelRGB8 (floor $ r*255) (floor $ g*255) (floor $ b*255)
  where hsvPix = rgbToHSV (pixToRGB pix)
        newV = smirnov vHist $ hsvV hsvPix
        RGB r g b = hsvToRGB hsvPix { hsvV = newV }

calcImageVCdf img = calcCdf2 0.0001 vs where
  vs = hsvV . rgbToHSV <$> convertToPixels img







calcHisto list = unfoldr generator 0 where
  getValue l x = fromIntegral $ length $ filter (\y -> abs (y - x) <= 0.001) l
  generator x | x <= 1  = Just (getValue list x,x+0.001)
        | otherwise = Nothing

calcImageHisto img = calcHisto vs where
  vs = hsvV . rgbToHSV <$> convertToPixels img

cdf :: [Double] -> [Double]
cdf l = undefined --( / maxEl) <$> cdf'



  where cdf' = reverse $ foldl (\acc x ->  (head acc + x):acc) [ head l] (tail l)
        maxEl = maximum cdf'
