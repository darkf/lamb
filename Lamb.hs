-- Driver for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath (FilePath, splitExtension)
import Control.Monad.IO.Class (liftIO)
import Parser (parseProgram)
import Interp (evalFileV, evalProgram, initIO, interpret, InterpState, Value)

-- returns Nothing if all files exist, or Just path for the first one that doesn't
allExist :: [FilePath] -> IO (Maybe FilePath)
allExist [] = return Nothing
allExist ("-":xs) = allExist xs
allExist (x:xs) = do
	exists <- doesFileExist x
	if exists then allExist xs
	else return $ Just x

repl :: InterpState Value
repl = do
	liftIO $ putStr ">> "
	line <- liftIO getLine
	case parseProgram line of
		Left err -> do
			liftIO $ putStrLn $ "parse error: " ++ show err
			repl
		Right prg -> do
			ev <- evalProgram prg
			liftIO $ print ev
			repl

repl' :: IO ()
repl' = interpret repl >> return ()

main = do
	args <- getArgs
	case args of
		[] -> -- no arguments, launch REPL
			initIO >> repl'
		_ -> do
			exist <- allExist args
			case exist of
				Just file -> putStrLn $ "error: file " ++ file ++ " doesn't exist"
				Nothing ->
					initIO >>
					mapM_ evalFileV args