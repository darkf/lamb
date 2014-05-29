-- Driver for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath (FilePath, splitExtension)
import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Parser (parseProgram)
import Interp (evalFileV, evalProgram, initIO, interpret, InterpState, Value)

exists :: FilePath -> IO Bool
exists "-" = return True
exists path = not <$> doesFileExist path

findMissing :: [FilePath] -> IO [FilePath]
findMissing = filterM exists

repl :: InterpState Value
repl = do
	liftIO $ putStr ">> "
	line <- liftIO getLine
	case parseProgram line of
		Left err -> do
			liftIO $ putStrLn $ "parse error: " ++ show err
		Right prg -> do
			ev <- evalProgram prg
			liftIO $ print ev
	repl

repl' :: IO ()
repl' = interpret repl >> return ()

main = do
	args <- getArgs
	if null args
	then do -- no arguments, launch REPL
		initIO
		repl'
	else do
		missing <- findMissing args
		if null missing
		then do
			initIO
			mapM_ evalFileV args
		else do
			let reportMissing file = putStrLn $ "error: file " ++ file ++ " doesn't exist"
			mapM_ reportMissing missing
