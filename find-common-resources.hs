-- Find common resources in XAML files.

import System.Environment
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import System.IO

import qualified Data.Map.Lazy as Map

-- | Program option flag types
data OptFlag = DirOpt String
             deriving Show

-- | Program option descriptors
options :: [OptDescr OptFlag]
options =
  [ Option ['d'] ["dir"] (OptArg dirp "DIRECTORY") "Directory in which to start walk"
  ]

-- | Map from resource to files where it is used.
data ResourceToUsageMap = ResourceToUsageMap (Map.Map String [String])

-- | Main
main :: IO ()
main = do
  argv <- getArgs
  parms <- getProgramParameters argv
  dumpProgramParameters parms
  resources <- walkDir parms
  printCommonResources parms resources
  putStrLn "Done."

dirp :: Maybe String -> OptFlag
dirp = DirOpt . fromMaybe "."

-- | Transforms program arguments to options via getOpt
getProgramParameters :: [String] -> IO ([OptFlag], [String])
getProgramParameters argv = 
  case getOpt Permute options argv of
      (o, n, []) -> return (o,n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: " ++  "getProgName" ++ " [-d|--dir DIRECTORY]"
        
-- | Dump program parameters to stderr
dumpProgramParameters :: ([OptFlag], [String]) -> IO ()
dumpProgramParameters (optFlags, nonOptStrings) = do
  hPutStr stderr ("Got options:\n" ++ thingPerLine "    " optFlags)
  hPutStr stderr ("Got non-options:\n" ++ thingPerLine "    " nonOptStrings)

-- | Transform a list of things deriving Show into a string, one thing per line, with leading indentation
thingPerLine :: (Show t) => String -> [t] -> String
thingPerLine indent things =
  foldl (\ str thing -> str ++ indent ++ (show thing) ++ "\n") "" things

----------------------------------------------------------------

-- | Walk the given directory finding resources used by each file
walkDir :: ([OptFlag], [String]) -- ^ Option, non-option parameters
        -> IO ResourceToUsageMap
walkDir (optFlags, nonOptStrings) = do
  let dummy = nonOptStrings ++ (map dirName (filter isDirOpt optFlags))
  return (ResourceToUsageMap (Map.singleton "foo" ["bar"]))

-- | Return a list of tuples: (resource, filepath)
resourcesUsed :: String -> [(String, String)]
resourcesUsed filepath =
  -- let filecontents = readFile filepath
  []

-- | Return true iff given OptFlag is a DirOpt
isDirOpt :: OptFlag -> Bool
isDirOpt (DirOpt _) = True
isDirOpt _ = False

-- | Returns dir name out of DirOpt OptFlag
dirName :: OptFlag -> String
dirName (DirOpt dir) = dir
dirName _ = error "Unexpect arg"

-- | Print those resources in the given map that are used in two or more files
printCommonResources :: ([OptFlag], [String]) -> ResourceToUsageMap -> IO ()
printCommonResources (optFlags, nonOptStrings) resourceToUsageMap = do
  putStrLn "Will print common resources."
  
  
