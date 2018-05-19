#!/usr/bin/env runhaskell
import Control.Monad (filterM, unless)
import System.Directory (getCurrentDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath (hasExtension, takeExtension, replaceBaseName, takeBaseName, (</>), takeFileName)
import Data.List (isPrefixOf, isInfixOf)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..))
import System.Exit (ExitCode(..), exitWith, exitSuccess, exitFailure)
import System.Posix.Temp (mkdtemp)
import GHC.IO.Handle (hGetContents)
import Data.Maybe (fromJust)

containsAnnotation :: String -> String -> Bool
containsAnnotation annotation line = ("//" `isPrefixOf` line) && ("@" ++ annotation) `isInfixOf` line

fileContainsAnnotation :: String -> FilePath -> IO Bool
fileContainsAnnotation annotation file = do
    content <- readFile file
    let linesOfFile = lines content
    return $ any (containsAnnotation annotation) linesOfFile

shouldCompile :: FilePath -> IO Bool
shouldCompile = fileContainsAnnotation "should_compile"

shouldNotCompile :: FilePath -> IO Bool
shouldNotCompile = fileContainsAnnotation "should_not_compile"

-- TODO(gtklocker): is this really the best way to accomplish this?
shouldBeTested :: FilePath -> IO Bool
shouldBeTested file = do
    should <- shouldCompile file
    shouldNot <- shouldNotCompile file
    return $ should || shouldNot

listDirectoryAbsolute :: FilePath -> IO [FilePath]
listDirectoryAbsolute dir = do
    contents <- listDirectory dir
    return $ map (\filename -> dir </> filename) contents

passesTest :: FilePath -> IO (Bool, Maybe String)
passesTest file = do
    cwd <- getCurrentDirectory
    let compiler = cwd </> "compiler.py"
    let cmd = unwords ["python3", compiler, file]
    tempDir <- mkdtemp "compiler-test"
    (_, stdout, _, ph) <- createProcess $ (shell cmd){ cwd = Just tempDir, std_out = CreatePipe, std_err = CreatePipe }
    exit <- waitForProcess ph
    output <- hGetContents $ fromJust stdout
    removeDirectoryRecursive tempDir
    should <- shouldCompile file
    shouldNot <- shouldNotCompile file
    case exit of
        ExitSuccess -> return (should, Nothing)
        ExitFailure _ -> return (shouldNot, Just output)

greenPutStrLn :: String -> IO ()
greenPutStrLn output = putStrLn $ "\x1b[32m" ++ output ++ "\x1b[0m"

redPutStrLn :: String -> IO ()
redPutStrLn output = putStrLn $ "\x1b[31m" ++ output ++ "\x1b[0m"

bluePutStrLn :: String -> IO ()
bluePutStrLn output = putStrLn $ "\x1b[36m" ++ output ++ "\x1b[0m"

testAnnotated :: FilePath -> IO Bool
testAnnotated file = do
    (passes, output) <- passesTest file

    if passes
    then greenPutStrLn "pass"
    else redPutStrLn "fail"

    case output of 
        Just out -> unless passes $ putStrLn out
        Nothing -> return ()
    return passes

test :: FilePath -> IO Bool
test file = do
    putStr $ takeFileName file ++ "... "
    hasAnnotations <- shouldBeTested file
    if hasAnnotations
    then testAnnotated file
    else bluePutStrLn "skip" >> return True

main :: IO ()
main = do
    currentDir <- getCurrentDirectory
    exampleFiles <- listDirectoryAbsolute $ currentDir </> "examples/"

    let testFiles = filter (\filename -> takeExtension filename == ".eel") exampleFiles
    oks <- mapM test testFiles

    if and oks then exitSuccess
    else exitFailure
