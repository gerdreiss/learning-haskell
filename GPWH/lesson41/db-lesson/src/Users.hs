module Users where

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Utils

data User = User
  { userId   :: Int
  , userName :: String
  }

instance Show User where
  show user = mconcat
      [ show $ userId user
      , ".)  "
      , userName user
      ]

instance FromRow User where
  fromRow = User <$> field
                 <*> field


addUser :: String -> IO ()
addUser userName = withConn "tools.db" executeInsert >> print "user added"
  where
    executeInsert conn = execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)

printUsers :: IO ()
printUsers = withConn "tools.db" executeSelect
  where
    executeSelect conn = (query_ conn "SELECT * FROM users;" :: IO [User]) >>= mapM_ print

promptAndAddUser :: IO ()
promptAndAddUser = do
  putStrLn "Enter new user name"
  userName <- getLine
  addUser userName
