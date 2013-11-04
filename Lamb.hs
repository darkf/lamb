-- Driver for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath (FilePath, splitExtension)
import Interp (evalFileV, initIO, Value(UnitV))

-- returns Nothing if all files exist, or Just path for the first one that doesn't
allExist :: [FilePath] -> IO (Maybe FilePath)
allExist [] = return Nothing
allExist ("-":xs) = allExist xs
allExist (x:xs) = do
	exists <- doesFileExist x
	if exists then allExist xs
	else return $ Just x

main = do
	args <- getArgs
	exist <- allExist args
	case exist of
		Just file -> putStrLn $ "error: file " ++ file ++ " doesn't exist"
		Nothing ->
			initIO >>
			mapM_ evalFileV args