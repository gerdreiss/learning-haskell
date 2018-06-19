{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable (for_)
import PersonParser (Error(..), Person(..), parsePerson)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "parsePerson" $ for_ parseCases parseTest
  where
    parseTest (description, str, expected) = it description assertion
      where
        assertion = expression `shouldBe` expected
        expression = parsePerson str

parseCases :: [(String, String, Either Error Person)]
parseCases =
  [ ("wrong Parse | empty string", t0, Left ParsingError)
  , ( "correct"
    , t1
    , Right Person {firstName = "John", lastName = "Connor", age = 30})
  , ( "correct | shiffled fields"
    , t18
    , Right Person {firstName = "John", lastName = "Connor", age = 30})
  , ( "correct | double name, minor fields ignored"
    , t2
    , Right Person {firstName = "John Smith", lastName = "Connor", age = 30})
  , ( "correct | double name, minor fields ignored"
    , t5
    , Left ParsingError)
  ]

-- wrong Parse | empty string
t0 = ""

-- correct
t1 = "firstName = John\nlastName = Connor\nage = 30"

-- correct | shiffled fields
t18 = "lastName = Connor\nfirstName = John\nage = 30"

-- wrong Parse | no spaces around = in minor fields
t2 = "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"

-- wrong Parse | no spaces around = on the left in minor fields
t5 = "firstName = John Smith\nlastName = Connor\nage = 30\nasde= "

-- wrong Parse | no spaces around = in major fields
t3 = "firstName=Barbarian\nlastName=Conn On\nage=30"

-- wrong Incorrect | age is non-numeric
t4 = "firstName = John\nlastName = Connor\nage = as30"

-- wrong Parse | no spaces around = in major fields, missing major field
t6 = "firstName=Barbarian\nlastName=Conn Or"

-- wrong Parse | no spaces around = in major fields, typo in major field
t7 = "firstNameee = John Smith\nlastName = Connor\nage = 30\nasde=as11"

-- correct | excessive fields
t8 = "firstName = John\nlastName = Connor\nfoo = bar\nage = 30"

-- wrong Incomplete | missing major field
t9 = "firstName = Barbarian\nlastName = Conn Or"

-- wrong Parse | empty major value
t10 = "firstName = John\nlastName = Connor\nage = "

-- wrong Parse | no spaces around = on the right in major field
t11 = "firstName = John\nlastName = Connor\nage ="

-- wrong Parse | empty key, missing major field
t12 = "firstName = John\nlastName = Connor\n = 30"

-- correct | spaces in major field value
t13 = "firstName = Barbarian\nlastName = Conn On\nage = 30"

-- correct | = in major field value
t14 = "firstName = John\nlastName = Con=nor\nage = 30"

-- wrong Parse | no spaces around =, missing value in minor field
t15 = "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd"

-- wrong Incomplete | major field key with whitespace, age is non-numeric
t17 = " firstName = John\nlastName = Connor\nage = 2f8 "
