module Tools where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data Tool = Tool
  { toolId        :: Int
  , name          :: String
  , description   :: String
  , lastReturned  :: Day
  , timesBorrowed :: Int
  }

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

instance Show Tool where
  show tool = mconcat
      [ show $ toolId tool
      , ".) "
      , name tool
      , "\n  description: "
      , description tool
      , "\n  last returned: "
      , show $ lastReturned tool
      , "\n  times borrowed: "
      , show $ timesBorrowed tool
      , "\n"
      ]

instance FromRow User where
  fromRow = User <$> field
                 <*> field

instance FromRow Tool where
   fromRow = Tool <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field

instance ToRow Tool where
  toRow tool = [ SQLInteger $ fromIntegral $ toolId tool
               , SQLText $ T.pack $ name tool
               , SQLText $ T.pack $ description tool
               , SQLText $ T.pack $ show $ lastReturned tool
               , SQLInteger $ fromIntegral $ timesBorrowed tool
               ]


addUser :: String -> IO ()
addUser userName = withConn "tools.db" executeInsert >> print "user added"
  where
    executeInsert conn = execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" executeCheckout >> print "tool checked out"
  where
    executeCheckout conn = execute conn "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)" (userId, toolId)

printUsers :: IO ()
printUsers = withConn "tools.db" executeSelect
  where
    executeSelect conn = (query_ conn "SELECT * FROM users;" :: IO [User]) >>= mapM_ print

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery "SELECT * FROM tools where id not in (select tool_id from checkedout);"

printCheckedOut :: IO ()
printCheckedOut = printToolQuery "SELECT * FROM tools where id in (select tool_id from checkedout);"

printToolQuery :: Query -> IO ()
printToolQuery q =
  withConn "tools.db" $ \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
  return $ listToMaybe resp

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
   { lastReturned  = date
   , timesBorrowed = 1 + timesBorrowed tool
   }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "cannot persist Nothing"
updateOrWarn (Just tool) = withConn "tools.db" executeUpdate >> print "tool updated"
  where
    executeUpdate conn = execute conn "UPDATE TOOLS SET lastReturned = ?, timesBorrowed = ? WHERE ID = ?;" (lastReturned tool, timesBorrowed tool, toolId tool)

updateToolTable :: Int -> IO ()
updateToolTable toolId =
  withConn "tools.db" $ \conn -> do
    tool <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool <*> pure currentDay
    updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId = withConn "tools.db" executeDelete
  where
    executeDelete conn = execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId


promptAndAddUser :: IO ()
promptAndAddUser = do
  putStrLn "Enter new user name"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  putStrLn "Enter the id of the user"
  userId <- pure read <*> getLine
  putStrLn "Enter the id of the tool"
  toolId <- pure read <*> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  putStrLn "enter the id of tool"
  toolId <- pure read <*> getLine
  checkinAndUpdate toolId

