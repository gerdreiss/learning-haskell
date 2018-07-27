import           Control.Exception
import           Data.List
import           System.Directory
import           System.Environment
import           System.IO

main = do
    (command : argList) <- getArgs
    dispatch command argList

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "edit"   = edit
dispatch "view"   = view
dispatch "remove" = remove

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

edit :: [String] -> IO ()
edit [fileName, numberString, newText] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        reducedItems = delete (todoTasks !! number) todoTasks
        completeItems = sortOn fst $ (number, newText) : zip [0..] reducedItems
        newTodoItems = unlines $ map snd completeItems
    _override fileName newTodoItems

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    _override fileName newTodoItems

_override :: String -> String -> IO ()
_override fileName todoItems = do
    bracketOnError (openTempFile "." "temp")
      (\ (tempName, tempHandle) ->
         do hClose tempHandle
            removeFile tempName)
      (\ (tempName, tempHandle) ->
         do hPutStr tempHandle todoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
