import           Control.Exception
import           Data.List
import           System.Directory
import           System.Environment
import           System.IO

main :: IO ()
main = do
    mainArgs <- getArgs
    case mainArgs of
        (cmd:args) -> dispatch cmd args
        _          -> what []

{-
   invokes the function with the name given in the first parameter,
   and the rest parameters
-}
dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "edit"   = edit
dispatch "view"   = view
dispatch "remove" = remove
dispatch "help"   = help
dispatch _        = what

{-
   prints out help
-}
help :: [String] -> IO ()
help _ = putStrLn "Usage: \n\
                  \Add new TODO item  :  todo add <file> <text>\n\
                  \Edit a TODO item   :  todo edit <file> <num> <text>\n\
                  \View TODO items    :  todo view <file>\n\
                  \Remove a TODO item :  todo remove <file> <num>\n\
                  \Show this text     :  todo help\n"

{-
   prints out an error message
-}
what :: [String] -> IO ()
what _ = print "Um, no, I don't understand... Try with todo help"

{-
   appends a new line to the given file
-}
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _                    = what []

{-
   edits the line with the given number
-}
edit :: [String] -> IO ()
edit [fileName, numberString, newText] = do
    contents <- readFile fileName
    let (number, reducedItems) = _linesSkippingLine contents numberString
        newTodoItems = unlines . map snd . sortOn fst $ (number, newText) : zip [1..] reducedItems
    _override fileName newTodoItems
edit _ = what []

{-
   displays the contents of the file with the given name
-}
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [1..] todoTasks
    putStr $ unlines numberedTasks
view _ = what []

{-
   removes the line with the given number
-}
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let (_, reducedItems) = _linesSkippingLine contents numberString
        newTodoItems = unlines reducedItems
    _override fileName newTodoItems
remove _ = what []

{-
   reads lines from the given string,
   remove the line with the given number,
   and returns the removed line number and the resulting lines
-}
_linesSkippingLine :: String -> String -> (Int, [String])
_linesSkippingLine contents numberString = (number, reduced)
     where number = read numberString
           todos = lines contents
           reduced = delete (todos !! (number - 1)) todos

{-
   overrides the contents of the file with the given name with the given new contents
-}
_override :: String -> String -> IO ()
_override fileName todoItems =
    bracketOnError (openTempFile "." "temp")
      (\ (tempName, tempHandle) ->
         do hClose tempHandle
            removeFile tempName)
      (\ (tempName, tempHandle) ->
         do hPutStr tempHandle todoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
