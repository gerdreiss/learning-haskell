import           Data
import           Lib
import           Test.Hspec


gwc = gridWithCoords grid

testFindWord word =
    let (Just result) = findWord gwc word
        string = map cell2char result
    in string `shouldBe` word


main :: IO ()
main = hspec $ do
    describe "formatGrid" $ it "Should concatenate everyline with a newline" $ formatGrid (gridWithCoords ["abc", "def"]) `shouldBe` "abc\ndef\n"

    describe "findWord" $ do
        it "Should find words that exist on the Grid" $ do
            testFindWord "HASKELL"
            testFindWord "PERL"
        it "Should not find words that do not exist on the Grid" $ findWord gwc "SCALA" `shouldBe` Nothing

    describe "findWords" $ do
        it "Should find all the words that exist on the Grid" $ do
            let found = findWords (gridWithCoords grid) languages
                asString = map (map cell2char) found
            asString `shouldBe` languages
        it "Should not find the words that do not exist on the Grid" $ findWords (gridWithCoords grid) ["SCALA", "JAVA"] `shouldBe` []
