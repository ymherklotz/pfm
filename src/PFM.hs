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

data Endianness = Big | Little

data ImageType = MonoImage | ColourImage

matchText :: Text -> Parser ByteString
matchText = P.string . T.encodeUtf8

magicNum :: Parser ImageType
magicNum = do
  match <- T.decodeUtf8 <$> (matchText "Pf" <|> matchText "PF")
  if match == "Pf"
    then return MonoImage
    else return ColourImage


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

endianness :: Parser Endianness
endianness =
  getEnd . (<0.0) . decode <$> matchMult "0-9.-"
  where
    getEnd True  = Little
    getEnd False = Big

float :: Endianness -> Parser Float
float e =
  runGet conv . fromStrict <$> P.take 4
  where
    conv = case e of
      Big    -> getFloat32be
      Little -> getFloat32le

header :: Parser (Int, Int, Endianness, ImageType)
header = do
  n <- magicNum
  skipNewline
  n1 <- num
  skipSpace
  n2 <- num
  skipNewline
  s <- endianness
  skipNewline
  return (n1, n2, s, n)

parseColour :: Endianness -> Parser Colour
parseColour e = do
  ri <- float e
  gi <- float e
  bi <- float e
  return $ Colour ri gi bi

parseMono :: Endianness -> Parser Colour
parseMono e = Mono <$> float e

parser :: Parser Image
parser = do
  (w, h, e, i) <- header
  c <- P.many1 $ fun i e
  return $ Image w h c
  where
    fun i = case i of
      ColourImage -> parseColour
      MonoImage   -> parseMono

parse :: ByteString -> IO ()
parse s = case P.parseOnly parser s of
  Left str -> putStrLn str
  Right i  -> print i
