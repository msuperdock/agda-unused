module Main where

import Agda.Unused
  (checkUnused)
import Agda.Unused.Name
  (Name (..), NamePart (..), QName (..))
import Agda.Unused.Range
  (Range, RangeInfo, rangeName)
import Agda.Unused.Root
  (Root (..))

import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map
import Test.Hspec
  (Expectation, describe, expectationFailure, hspec, it, shouldSatisfy)

hasName
  :: QName
  -> Map Range RangeInfo
  -> Bool
hasName n
  = elem n
  . fmap rangeName
  . Map.elems

used
  :: String
  -> QName
  -> Expectation
used f n
  = checkUnused "data/test" [Root (QName (Name [Id f])) []]
  >>= either (const (expectationFailure "")) (`shouldSatisfy` not . hasName n)

unused
  :: String
  -> QName
  -> Expectation
unused f n
  = checkUnused "data/test" [Root (QName (Name [Id f])) []]
  >>= either (const (expectationFailure "")) (`shouldSatisfy` hasName n)

main
  :: IO ()
main = hspec $ do
  describe "checkUnused" $ do
    it "handles record constructors within modules" $ do
      used "Record" (QName (Name [Id "c"]))

