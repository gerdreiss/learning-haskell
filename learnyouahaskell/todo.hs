import           Control.Exception
import           Data.List
import           System.Directory
import           System.Environment
import           System.IO

main = do
    mainArgs <- getArgs
    case mainArgs of
        (cmd:args) -> dispatch cmd args
        _          -> what []

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "edit"   = edit
dispatch "view"   = view
dispatch "remove" = remove
dispatch "help"   = help
dispatch _        = what

help :: [String] -> IO ()
help _ = putStrLn "Usage: \n\
                  \Add new TODO item  :  todo add <file> <text>\n\
                  \Edit a TODO item   :  todo edit <file> <num> <text>\n\
                  \View TODO items    :  todo view <file>\n\
                  \Remove a TODO item :  todo remove <file> <num>\n\
                  \Show this text     :  todo help\n"

what :: [String] -> IO ()
what _ = print "Um, no, I don't understand... Try with todo help"

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _                    = what []

edit :: [String] -> IO ()
edit [fileName, numberString, newText] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        reducedItems = delete (todoTasks !! (number - 1)) todoTasks
        completeItems = sortOn fst $ (number, newText) : zip [1..] reducedItems
        newTodoItems = unlines $ map snd completeItems
    _override fileName newTodoItems
edit _                    = what []

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [1..] todoTasks
    putStr $ unlines numberedTasks
view _                    = what []

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! (number - 1)) todoTasks
    _override fileName newTodoItems
remove _                    = what []

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
