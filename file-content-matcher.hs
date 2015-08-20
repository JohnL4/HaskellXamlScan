import System.Environment
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import System.IO
import Control.Monad
import System.Directory
import System.FilePath
import Text.Regex.TDFA

import qualified Data.Map.Lazy as Map

resourcesSectionRegex = "abc"


-- | Program option flag types
data OptFlag = DirOpt String
             deriving Show

-- | Program parameters
data PgmParms = PgmParms ([OptFlag], -- ^ Option flags (possibly with values) passed to program
                          [String])  -- ^ Non-option arguments passed to program

-- | Program option descriptors
options :: [OptDescr OptFlag]
options =
  [ Option ['d'] ["dir"] (OptArg dirp "DIRECTORY") "Directory in which to start walk"
  ]

-- | Main
main :: IO ()
main = do
  argv <- getArgs
  parms <- getProgramParameters argv
  dumpProgramParameters parms
  processFiles parms
  putStrLn "Done."

dirp :: Maybe String -> OptFlag
dirp = DirOpt . fromMaybe "."

-- | Transforms program arguments to options via getOpt
getProgramParameters :: [String] -> IO PgmParms
getProgramParameters argv = 
  case getOpt Permute options argv of
      (o, n, []) -> return (PgmParms (o,n))
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: " ++  "getProgName" ++ " [-d|--dir DIRECTORY]"
        
-- | Dump program parameters to stderr
dumpProgramParameters :: PgmParms  -> IO ()
dumpProgramParameters (PgmParms (optFlags, nonOptStrings)) = do
  hPutStr stderr ("Got options:\n" ++ thingPerLine "    " optFlags)
  hPutStr stderr ("Got non-options:\n" ++ thingPerLine "    " nonOptStrings)

-- | Transform a list of things deriving Show into a string, one thing per line, with leading indentation
thingPerLine :: (Show t) => String -> [t] -> String
thingPerLine indent things =
  foldl (\ str thing -> str ++ indent ++ (show thing) ++ "\n") "" things

----------------------------------------------------------------

processFiles :: PgmParms -> IO ()
processFiles (PgmParms (_, files)) = do
  contents <- forM files (\f -> do readFile f)
  forM_ (filter (\t -> snd t) (zip files (map fileContentsMatch contents))) $
    (\t -> putStrLn ((fst t) ++ " matches"))

{-
  c <- readFile (files !! 0)
  if (c =~ resourcesSectionRegex)
     then putStrLn "matches"
    else putStrLn "does not match"
-}

fileContentsMatch :: String -> Bool
fileContentsMatch s = (s =~ resourcesSectionRegex)
