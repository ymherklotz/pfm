module Main where

import qualified Data.ByteString.Lazy          as BL
import           PFM
import           Test.Tasty
import           Test.Tasty.QuickCheck          ( (===) )
import qualified Test.Tasty.QuickCheck         as QC

newtype TestPFMImage = TestPFMImage { getPFMImage :: PFMImage }
                     deriving (Show)

newtype TestPFMColour = TestPFMColour { getPFMColour :: PFMColour }
                     deriving (Show)

newtype TestPPMImage = TestPPMImage  { getPPMImage :: PPMImage }
                     deriving (Show)

newtype TestPPMColour = TestPPMColour { getPPMColour :: PPMColour }
                      deriving (Show)

instance QC.Arbitrary TestPFMImage where
    arbitrary = TestPFMImage <$> (PFMImage <$> QC.arbitrary <*> QC.arbitrary
        <*> (QC.listOf1 . QC.listOf1) (getPFMColour <$> QC.arbitrary))

instance QC.Arbitrary TestPPMImage where
    arbitrary = TestPPMImage <$> (PPMImage <$> QC.arbitrary <*> QC.arbitrary
        <*> (QC.listOf1 . QC.listOf1) (getPPMColour <$> QC.arbitrary))

instance QC.Arbitrary TestPFMColour where
    arbitrary = TestPFMColour <$> (PFMColour <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary)

instance QC.Arbitrary TestPPMColour where
    arbitrary = TestPPMColour <$> (PPMColour <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary)

parserIdempotent' :: TestPFMImage -> QC.Property
parserIdempotent' (TestPFMImage v) = p i === (p . p) i
  where
    encStrict = BL.toStrict . encode
    i         = encStrict v
    p         = encStrict . parse

parserIdempotent :: TestTree
parserIdempotent = QC.testProperty "parser idempotent" parserIdempotent'

tests :: TestTree
tests = testGroup "Property" [parserIdempotent]

main :: IO ()
main = defaultMain tests
