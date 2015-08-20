-- Find common resources in XAML files.

import System.Environment
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import System.IO
import Control.Monad
import System.Directory
import System.FilePath
import Text.Regex.TDFA

import qualified Data.Map.Lazy as Map

-- | Program option flag types
data OptFlag = DirOpt String
             | VerboseOpt
             deriving (Show, Eq)

-- | Program option descriptors
options :: [OptDescr OptFlag]
options =
  [ Option ['d'] ["dir"]      (OptArg dirp "DIRECTORY")  "Directory in which to start walk",
    Option ['v'] ["verbose"]  (NoArg VerboseOpt)            "Be verbose in output to stderr"
  ]

-- | Map from resource to files where it is used.
data ResourceToUsageMap = ResourceToUsageMap (Map.Map String [String])

-- | Main
main :: IO ()
main = do
  argv <- getArgs
  parms <- getProgramParameters argv
  dumpProgramParameters parms
  filepaths <- getFilePaths parms
  hPutStr stderr ("Found " ++ (show (length filepaths)) ++ " files.")
  -- TODO: write "verb" function
  -- TODO: Find out how to format (pretty-print?) output more easily.
  when (elem (VerboseOpt) (fst parms)) $ do
    hPutStr stderr (fst (foldl (\ (msg, i) fp -> ((msg ++ "\n    " ++ (show (i+1)) ++ ": " ++ fp), i+1))
                         ("", 0) filepaths))
  hPutStrLn stderr  ""
  fileContentsList <- forM filepaths $ \fp -> do readFile fp
  hPutStrLn stderr  ("fileContentsList has " ++ (show (length fileContentsList)) ++ " entries.")
  let filesAndContents = zip filepaths fileContentsList
      allResources = findResources parms filesAndContents
    in when (elem VerboseOpt (fst parms)) $ do
         hPutStrLn stderr ("Found " ++ ((\(ResourceToUsageMap m) -> (show (length m))) allResources) ++ " usages of resources.")
      
  -- resources <- findResources parms filepaths
  -- printCommonResources parms resources
  hPutStrLn stderr  "Done."

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

-- ====================================================================================================

-- | Return a list of file paths to be scanned
getFilePaths :: ([OptFlag], [String]) -- ^ Program parameters
             -> IO [FilePath]
getFilePaths (optFlags, nonOptStrings) = do
  let cmdLineDirs = nonOptStrings ++ (map dirName (filter isDirOpt optFlags))
  files <- forM cmdLineDirs $ \dirName -> do
    isDirectory <- doesDirectoryExist dirName
    if isDirectory
      then getRecursiveContents dirName
      else return []
  return (concat files)
  -- return (concat (map getRecursiveContents cmdLineDirs))
  -- []

-- | Returns a list of contents of a directory (not including subdirectories)
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

-- | Return true iff given OptFlag is a DirOpt
isDirOpt :: OptFlag -> Bool
isDirOpt (DirOpt _) = True
isDirOpt _ = False

-- | Returns dir name out of DirOpt OptFlag
dirName :: OptFlag -> String
dirName (DirOpt dir) = dir
dirName _ = error "Unexpect arg"

-- | Walk the given directory finding resources used by each file
findResources :: ([OptFlag], [String]) -- ^ Option, non-option program parameters
              -> [(FilePath, String)]  -- ^ files to be scanned, along with their (lazy) contents
              -> ResourceToUsageMap
findResources (optFlags, nonOptStrings) filepathsAndContents =
  -- (ResourceToUsageMap Map.empty)
  foldl (addUsageToMap) (ResourceToUsageMap Map.empty) (usagesInFileContents filepathsAndContents)

-- | List of all usages in all given filepaths
usagesInFileContents :: [(FilePath,String)] -- ^ Filename and contents to be scanned
                     -> [(FilePath, String)] -- ^ List of usages in files
usagesInFileContents filepathsAndContents = (concat (map resourcesUsed filepathsAndContents))

-- | Add the given resource usage to the given map, returning the updated map
addUsageToMap :: ResourceToUsageMap -- ^ map to be updated
              -> (FilePath, String) -- ^ String is used in FilePath
              -> ResourceToUsageMap -- ^ updated map
addUsageToMap (ResourceToUsageMap inputMap) (filepath, usageOccurrence) =
  ResourceToUsageMap (Map.insertWith (++) filepath [usageOccurrence] inputMap)

-- | Return a list of tuples: (filepath, resource)
resourcesUsed :: (FilePath, String) -> [(FilePath, String)]
resourcesUsed (filepath, filecontents) = 
  -- [("foo", "bar")]
  resourcesUsed2 filepath (filecontents =~ resourcesSectionRegex :: (String, String, String, [String]))
  where resourcesSectionRegex =
          "<[ \t\n\r]*(Window|UserControl)\\.Resources[ \t\n\r]*>.*</[ \t\n\r]*(Window|UserControl)\\.Resources[ \t\n\r]*>"

-- | Transform subexpressions found in resources section to (filename,subexpression) pairs.
resourcesUsed2 :: FilePath      -- ^ The filepath searched
               -> (String,String,String,[String]) -- ^ Results of regexp match
               -> [(FilePath,String)]
resourcesUsed2 filepath (_,_,_,subexprs) =
  map (\ subexpr -> (filepath, subexpr)) subexprs
  
-- | Print those resources in the given map that are used in two or more files
printCommonResources :: ([OptFlag], [String]) -> ResourceToUsageMap -> IO ()
printCommonResources (optFlags, nonOptStrings) resourceToUsageMap = do
  hPutStrLn stderr  "Will print common resources."

