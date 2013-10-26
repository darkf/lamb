-- Driver for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath (FilePath, splitExtension)
import Interp (evalProgram, evalString, Value(UnitV))

-- returns Nothing if all files exist, or Just path for the first one that doesn't
allExist :: [FilePath] -> IO (Maybe FilePath)
allExist [] = return Nothing
allExist ("-":xs) = allExist xs
allExist (x:xs) = do
	exists <- doesFileExist x
	if exists then allExist xs
	else return $ Just x

isLiterate :: FilePath -> Bool
isLiterate path = snd (splitExtension path) == ".lilamb"

-- Takes the lines of a literate program and returns the lines for a new executable program
-- from lines beginning with four spaces.
parseLiterate :: [String] -> [String]
parseLiterate lns = [drop 4 line | line <- lns, take 4 line == "    "]

evalFile :: String -> IO Value
evalFile path = do
	contents <- if path == "-" then getContents else readFile path
	if isLiterate path then
		evalString . unlines . parseLiterate . lines $ contents
	else evalString contents

main = do
	args <- getArgs
	exist <- allExist args
	case exist of
		Just file -> putStrLn $ "error: file " ++ file ++ " doesn't exist"
		Nothing ->
			mapM_ evalFile args
