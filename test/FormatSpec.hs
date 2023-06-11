module FormatSpec where

import Format (format)
import Test.Hspec

spec :: Spec
spec = do
  describe "format" $ do
    it "indents the content of interpolated strings with 2 spaces" $ do
      let input =
            unlines
              [ "[i|",
                "foo",
                "bar",
                "|]"
              ]
          expected =
            unlines
              [ "[i|",
                "  foo",
                "  bar",
                "|]"
              ]
      format input `shouldBe` expected

    it "doesn't modify code without interpolated strings" $ do
      let input =
            unlines
              [ "foo",
                "  bar"
              ]
          expected =
            unlines
              [ "foo",
                "  bar"
              ]
      format input `shouldBe` expected

    it "preserves indentation outside of interpolated strings" $ do
      pending

    it "preserves relative indentation in interpolated strings " $ do
      pending

    it "unindents when indentation is bigger than 2 spaces" $ do
      pending

    it "works for interpolated strings that don't have the opening string on a separate line" $ do
      pending
