{-|
Module      : PFM
Description : Debevec PFM reader
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Debevec PFM reader
-}

module PFM where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import           Data.Binary.Get            (runGet)
import           Data.Binary.IEEE754        (getFloat32be, getFloat32le)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Word                  (Word8)

data Image = Image { width  :: Int
                   , height :: Int
                   , colour :: [Colour]
                   } deriving (Show)

data Colour = Colour { r :: Float
                     , g :: Float
                     , b :: Float
                     }
            | Mono Float
            deriving (Show)

matchText :: Text -> Parser ByteString
matchText = P.string . T.encodeUtf8

magicNumMono :: Parser ()
magicNumMono = void $ matchText "Pf"

magicNumRGB :: Parser ()
magicNumRGB = void $ matchText "PF"

skipNewline :: Parser ()
skipNewline = P.skip isNewline
  where
    isNewline w = w == 13 || w == 10

skipSpace :: Parser ()
skipSpace = P.skip (== 32)

decode :: (Read a) => [Word8] -> a
decode = read . T.unpack . T.decodeUtf8 . B.pack

matchMult :: String -> Parser [Word8]
matchMult = P.many1 . P.satisfy . P.inClass

num :: Parser Int
num = decode <$> matchMult "0-9"

endianness :: Parser Float
endianness = decode <$> matchMult "0-9.-"

float :: Parser Float
float = runGet getFloat32le . fromStrict <$> P.take 4

header :: Parser (Int, Int, Float)
header = do
  magicNumRGB
  skipNewline
  n1 <- num
  skipSpace
  n2 <- num
  skipNewline
  s <- endianness
  skipNewline
  return (n1, n2, s)

parseColour :: Parser Colour
parseColour = do
  ri <- float
  gi <- float
  bi <- float
  return $ Colour ri gi bi

parseMono :: Parser Colour
parseMono = Mono <$> float

parser :: Parser Image
parser = do
  (w, h, f) <- header
  c <- P.many1 parseColour
  return $ Image w h c

parse :: ByteString -> IO ()
parse s = case P.parseOnly parser s of
  Left str -> putStrLn str
  Right i  -> print i
