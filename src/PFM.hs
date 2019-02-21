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

module PFM
    ( PFMImage(..)
    , PPMImage(..)
    , PFMColour(..)
    , PPMColour(..)
    , parse
    , encode
    , encodePPM
    , revColour
    , gamma
    , module PFM.Vec
    )
where

import           Control.Applicative            ( (<|>) )
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as P
import           Data.Binary.Get                ( runGet )
import           Data.Binary.IEEE754            ( getFloat32be
                                                , getFloat32le
                                                , putFloat32le
                                                )
import           Data.Binary.Put                ( runPut )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import           Data.ByteString.Lazy           ( fromStrict )
import qualified Data.ByteString.Lazy          as BL
import           Data.Foldable                  ( fold )
import           Data.Functor                   ( (<$>) )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Word                      ( Word8 )
import           PFM.Vec

type PFMColours = Vector (Vector PFMColour)

type PPMColours = Vector (Vector PPMColour)

data PFMImage = PFMImage { pfmWidth  :: {-# UNPACK #-} !Int
                         , pfmHeight :: {-# UNPACK #-} !Int
                         , pfmColour :: {-# UNPACK #-} !PFMColours
                         } deriving (Show)

data PPMImage = PPMImage { ppmWidth  :: {-# UNPACK #-} !Int
                         , ppmHeight :: {-# UNPACK #-} !Int
                         , ppmColour :: {-# UNPACK #-} !PPMColours
                         } deriving (Show)

data PFMColour = PFMColour { getR :: {-# UNPACK #-} !Float
                           , getG :: {-# UNPACK #-} !Float
                           , getB :: {-# UNPACK #-} !Float
                           }
               | PFMMono {-# UNPACK #-} !Float
               deriving (Show)

data PPMColour = PPMColour { getRw :: {-# UNPACK #-} !Word8
                           , getGw :: {-# UNPACK #-} !Word8
                           , getBw :: {-# UNPACK #-} !Word8
                           }
               | PPMMono {-# UNPACK #-} !Word8
               deriving (Show)

data Endianness = Big | Little

data ImageType = MonoImage | ColourImage

matchText :: Text -> Parser ByteString
matchText = P.string . T.encodeUtf8

magicNum :: Parser ImageType
magicNum = do
    match <- T.decodeUtf8 <$> (matchText "Pf" <|> matchText "PF")
    if match == "Pf" then return MonoImage else return ColourImage


skipNewline :: Parser ()
skipNewline = P.skip isNewline where isNewline w = w == 13 || w == 10

skipSpace :: Parser ()
skipSpace = P.skip (== 32)

decode :: (Read a) => [Word8] -> a
decode = read . T.unpack . T.decodeUtf8 . B.pack

matchMult :: String -> Parser [Word8]
matchMult = P.many1 . P.satisfy . P.inClass

num :: Parser Int
num = decode <$> matchMult "0-9"

endianness :: Parser Endianness
endianness = getEnd . (< (0.0 :: Float)) . decode <$> matchMult "0-9.-"
  where
    getEnd True  = Little
    getEnd False = Big

float :: Endianness -> Parser Float
float e = runGet conv . fromStrict <$> P.take 4
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

parseColour :: Endianness -> Parser PFMColour
parseColour e = do
    ri <- float e
    gi <- float e
    bi <- float e
    return $ PFMColour ri gi bi

parseMono :: Endianness -> Parser PFMColour
parseMono e = PFMMono <$> float e

parser :: Parser PFMImage
parser = do
    (w, h, e, i) <- header
    c            <- V.fromList <$> (P.many1 . fmap V.fromList . P.count w) (fun i e)
    return $ PFMImage w h c
  where
    fun i = case i of
        ColourImage -> parseColour
        MonoImage   -> parseMono

magicNumPFM :: PFMColours -> Text
magicNumPFM v = case V.head $ V.head v of
    PFMColour{} -> "PF"
    PFMMono{}   -> "Pf"

tShow :: (Show a) => a -> Text
tShow = T.pack . show

encFloat :: Float -> BL.ByteString
encFloat = runPut . putFloat32le

encodeColourPFM :: PFMColour -> BL.ByteString
encodeColourPFM (PFMColour ri gi bi) = encFloat ri <> encFloat gi <> encFloat bi
encodeColourPFM (PFMMono m         ) = encFloat m

encodeColourPPM :: PPMColour -> BL.ByteString
encodeColourPPM (PPMColour ri gi bi) = BL.pack [ri, gi, bi]
encodeColourPPM (PPMMono m         ) = BL.pack [m, m, m]

-- | Encode as a PFM file. Returns a lazy ByteString with the encoded
-- result.
encode :: PFMImage -> BL.ByteString
encode (PFMImage w h c) = fromStrict (T.encodeUtf8 he) <> body
  where
    he   = magicNumPFM c <> "\n" <> tShow w <> " " <> tShow h <> "\n-1.0\n"
    body = fold . fold $ fmap encodeColourPFM <$> c

-- | Encode as a PPM file. Returns a lazy ByteString which contains the encoded
-- file.
encodePPM :: PPMImage -> BL.ByteString
encodePPM (PPMImage w h c) = fromStrict (T.encodeUtf8 he) <> body
  where
    he   = "P6" <> "\n" <> tShow w <> " " <> tShow h <> "\n255\n"
    body = fold . fold $ fmap encodeColourPPM <$> c

-- | Parse a 'ByteString' into a 'PFMImage'. These can be mono colour images or
-- RGB colour images.
parse :: ByteString -> PFMImage
parse s = case P.parseOnly parser s of
    Left  str -> error str
    Right i   -> i

revColour :: PFMImage -> PFMImage
revColour (PFMImage w h i) = PFMImage w h $ V.reverse i

gamma :: (Floating a) => a -> a -> a
gamma g m = m ** (1 / g)
