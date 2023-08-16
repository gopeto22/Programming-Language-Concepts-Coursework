import Tokens
import Grammar 
import System.Environment
import Control.Exception
import System.IO
import qualified GHC as Main
import qualified GHC.CmmToAsm.AArch64.Instr as Token

{-
to execute this module
:l Main.hs
set: args ["name of file to be input"]
main
-}

{- 
The main function is defined as follows: 
it catches any exceptions that might be thrown during program execution and handles them using the "noLex" function (more on this later). 
Then, it calls the "main'" function.
-}
main :: IO ()
main = catch main' noPars --catch function takes two arguments: an IO action to run, and an exception handler function
                            -- If the IO action throws an exception during execution, the exception handler function is called with the exception object as its argument.


{-
getArgs:  returns a list of the command-line arguments passed to the program ,,  returns an I/O action that, when executed, produces a list of String
          any command-line arguments passed after the program name are collected by the operating system and made available to the program
example: 
In ghci -  ghci myprogram.hs
           :set args arg1 arg2
myprogram.hs contains -  
  args <- getArgs
  putStrLn ("The command-line arguments are: " ++ show args)
outputs - The command-line arguments are: ["arg1","arg2","arg3"]
------------
readFile:  used inside main , main :: IO() as part of a do block - reads the contents of a file into a String

putStrLn: print a String to the console, followed by a newline character
alexScansTokens: takes a String argument representing the input source code, and returns a list of tokens representing the lexical structure of the input.

-}
main' = do (fileName : _ ) <- getArgs -- a. It gets the first command-line argument (the name of the file to be lexed) using the "getArgs" function from the "System.Environment" module.  
           sourceText <- readFile fileName  -- b. It reads the contents of the file into a string variable called "sourceText" using the "readFile" function from the "System.IO" module.
           putStrLn ("Lexing : " ++ sourceText) -- c. It outputs a message to the console indicating that it is starting to lex the file.
           let lexedProg = alexScanTokens sourceText -- d. It lexes the contents of "sourceText" using the "alexScanTokens" function from the "Tokens" module, and stores the resulting tokens in a variable called "lexedProg".
           putStrLn ("Lexed as " ++ (show lexedProg)) -- e. It outputs a message to the console indicating that the file has been successfully lexed, along with the resulting tokens.
           let parsedProg = parseTiles (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg))

{-
The "noLex" function is called when an exception is caught by the "main" function. 
It takes an "ErrorCall" argument (which contains information about the exception) and outputs an error message to the console.
ErrorCall: an exception that is raised by a call to the error function
                                                        error function is a standard library function that is used to indicate an error condition
                                                        it raises an exception with a message string as its argument
                                                        When an exception of type ErrorCall is raised, it contains a String field that holds the error message that was passed to the error function-}
noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with Lexing : " ++ err)
             return ()


noPars :: ErrorCall -> IO ()
noPars e = do 
             let err =  show e
             hPutStr stderr ("Problem with Parsing: " ++ err)
             return ()
-- run alex Token.x
-- run happy Gramm.y
-- ghc Main.hs
-- ./Main.exe Test/<filename>