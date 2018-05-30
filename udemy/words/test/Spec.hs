import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
    describe "formatGrid" $ do
        it "Should concatenate everyline with a newline" $ do
            formatGrid grid `shouldBe` unlines grid

    describe "findWord" $ do
        it "Should find words that exist on the Grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"
            findWord grid "PERL" `shouldBe` Just "PERL"
        it "Should not find words that do not exist on the Grid" $ do
            findWord grid "SCALA" `shouldBe` Nothing

    describe "findWords" $ do
        it "Should find all the words that exist on the Grid" $ do
            findWords grid languages `shouldBe` languages
        it "Should not find the words that do not exist on the Grid" $ do
            findWords grid ["SCALA", "JAVA"] `shouldBe` []
