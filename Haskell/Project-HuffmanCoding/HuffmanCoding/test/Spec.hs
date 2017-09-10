import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           HetList

main :: IO()
main = hspec $ do
  describe "Test Element" $ do
    it "returns correct representation of int" $ do
      let el = Element 5 in
          show el `shouldBe` "5"

    it "returns correct representation of string" $ do
      let el = Element "example" in
          show el `shouldBe` "\"example\""

    it "returns correct representation of bool" $ do
      let el = Element True in
          show el `shouldBe` "True"
