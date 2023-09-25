module FormatSpec where

import Format (format, run)
import System.Environment
import System.Timeout (timeout)
import Test.Hspec
import Test.Mockery.Directory

wrapTest :: IO () -> IO ()
wrapTest test = do
  result <- timeout 200001 test
  case result of
    Nothing -> error "timeout"
    Just () -> return ()

spec :: Spec
spec = around_ wrapTest $ do
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
      format "<test-file>" input `shouldBe` expected

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
      format "<test-file>" input `shouldBe` expected

    it "formats interpolated strings that are indented as a whole" $ do
      let input =
            unlines
              [ "  [i|",
                "  foo",
                "  bar",
                "  |]"
              ]
          expected =
            unlines
              [ "  [i|",
                "    foo",
                "    bar",
                "  |]"
              ]
      format "<test-file>" input `shouldBe` expected

    it "preserves indentation outside of interpolated strings" $ do
      let input =
            unlines
              [ " foo",
                "   bar",
                "  [i|",
                "  foo",
                "  bar",
                "  |]"
              ]
          expected =
            unlines
              [ " foo",
                "   bar",
                "  [i|",
                "    foo",
                "    bar",
                "  |]"
              ]
      format "<test-file>" input `shouldBe` expected

    it "doesn't modify correctly indented interpolated strings" $ do
      let input =
            unlines
              [ "  [i|",
                "    foo",
                "    bar",
                "  |]"
              ]
          expected =
            unlines
              [ "  [i|",
                "    foo",
                "    bar",
                "  |]"
              ]
      format "<test-file>" input `shouldBe` expected

    it "preserves relative indentation in interpolated strings " $ do
      let input =
            unlines
              [ "[i|",
                "  foo",
                "    bar",
                "|]"
              ]
          expected =
            unlines
              [ "[i|",
                "  foo",
                "    bar",
                "|]"
              ]
      format "<test-file>" input `shouldBe` expected

    it "unindents when indentation is bigger than 2 spaces" $ do
      let input =
            unlines
              [ "[i|",
                "    foo",
                "|]"
              ]
          expected =
            unlines
              [ "[i|",
                "  foo",
                "|]"
              ]
      format "<test-file>" input `shouldBe` expected

    it "preserves empty unindented lines" $ do
      let input =
            unlines
              [ "[i|",
                "  foo",
                "",
                "  bar",
                "|]"
              ]
          expected =
            unlines
              [ "[i|",
                "  foo",
                "",
                "  bar",
                "|]"
              ]
      show (format "<test-file>" input) `shouldBe` show expected

    it "works for interpolated strings with just empty lines" $ do
      let input =
            unlines
              [ "[i|",
                "",
                "|]"
              ]
          foo =
            unlines
              [ "[i|",
                "",
                "|]"
              ]
      show (format "<test-file>" input) `shouldBe` show foo

    it "works for interpolated strings that don't have the opening string on a separate line" $ do
      pending

  describe "run" $ do
    it "applies the formatting to a given file" $ do
      inTempDirectory $ do
        writeFile "file" $
          unlines
            [ "[i|",
              "foo",
              "bar",
              "|]"
            ]
        withArgs ["file"] run
        let expected =
              unlines
                [ "[i|",
                  "  foo",
                  "  bar",
                  "|]"
                ]
        readFile "file" `shouldReturn` expected
