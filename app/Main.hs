module Main where

import           Criterion
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import           Data.Word            (Word8)
import           PFM

clamp :: PFMColour -> PPMColour
clamp (PFMColour ri gi bi) =
  PPMColour (f ri) (f gi) (f bi)
  where
    v s = s * 255.0
    f s = if v s > 255.0 then 255 else fromInteger (round (v s))
clamp _ = undefined

clampImage :: PFMImage -> PPMImage
clampImage (PFMImage w h c) =
  PPMImage w h . reverse $ fmap clamp <$> c

main :: IO ()
main = do
  -- s <- B.readFile "/home/yannherklotz/Imperial/AdvancedGraphics/coursework1/CO417-Assignment1/UrbanProbe/urbanEM_latlong.pfm"
  s <- B.readFile "/home/yannherklotz/Downloads/memorial.pfm"
  BL.writeFile "random.ppm" . encodePPM . clampImage . parse $ s
