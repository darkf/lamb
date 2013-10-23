-- Driver for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Interp (evalProgram, evalString, Value(UnitV))

-- returns Nothing if all files exist, or Just path for the first one that doesn't
allExist :: [String] -> IO (Maybe String)
allExist [] = return Nothing
allExist (x:xs) = do
	exists <- doesFileExist x
	if exists then allExist xs
	else return $ Just x

evalFile :: String -> IO Value
evalFile path = do
	contents <- readFile path
	evalString contents

main = do
	args <- getArgs
	exist <- allExist args
	case exist of
		Just file -> putStrLn $ "error: file " ++ file ++ " doesn't exist"
		Nothing ->
			mapM_ evalFile args
