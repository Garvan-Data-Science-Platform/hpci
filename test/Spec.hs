import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "hpci-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
