module ColorConverter
  (
  rgbToHSV,
  hsvToRGB,
  HSV(..), RGB(..),
  rgb
  ) where

data RGB a = RGB {
    rgbR :: !a
  , rgbG :: !a
  , rgbB :: !a
  }
  deriving( Eq, Ord )

instance Show a => Show (RGB a) where
  show (RGB r g b) = "RGB("++show r++", "++show g++", "++show b++")"

instance Functor RGB where
  fmap f (RGB r g b) = RGB (f r) (f g) (f b)

red, green, blue :: Fractional t => RGB t
red   = RGB 1.0 0.0 0.0
green = RGB 0.0 1.0 0.0
blue  = RGB 0.0 0.0 1.0

data HSV a = HSV {
    hsvH :: !a
  , hsvS :: !a
  , hsvV :: !a
  }
  deriving( Eq, Ord )

instance Show a => Show (HSV a) where
  show (HSV h s v) = "HSV("++show h++", "++show s++", "++show v++")"

instance Functor HSV where
  fmap f (HSV h s v) = HSV (f h) (f s) (f v)

rgbToHSV :: (Fractional t, Ord t) => RGB t -> HSV t
rgbToHSV (RGB r g b) = HSV h' s v
  where
  ub = max r (max g b)
  lb = min r (min g b)

  h  | ub == lb  = 0
     | ub == r   = 60 * (    (g-b)/(ub-lb))
     | ub == g   = 60 * (2 + (b-r)/(ub-lb))
     | otherwise = 60 * (4 + (r-g)/(ub-lb))

  h' | h < 0 = h + 360
     | otherwise = h

  s  | ub == 0   = 0
     | otherwise = (ub-lb)/ub

  v  = ub

hsvToRGB :: RealFrac t => HSV t -> RGB t
hsvToRGB (HSV h s v) = case hIdx of
  0 -> RGB v t p
  1 -> RGB q v p
  2 -> RGB p v t
  3 -> RGB p q v
  4 -> RGB t p v
  5 -> RGB v p q
  _ -> error "hsvToRGB: hue outside of range [0..360]"
  where
  hIdx = floor (h / 60)
  f    = h/60 - fromIntegral (hIdx::Int)
  p    = v*(1-s)
  q    = v*(1-s*f)
  t    = v*(1-s*(1-f))

rgb :: Integral a => a -> a -> a -> RGB Double
rgb r g b = RGB (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)
