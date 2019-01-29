module Main where

import qualified Data.ByteString as B
import qualified Data.Text       as T
import           PFM

main :: IO ()
main = do
  s <- B.readFile "/home/yannherklotz/Imperial/AdvancedGraphics/coursework1/CO417-Assignment1/UrbanProbe/urbanEM_latlong.pfm"
  parse s
