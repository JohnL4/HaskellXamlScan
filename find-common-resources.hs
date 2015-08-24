-- Find common resources in XAML files.

import Debug.Trace
import System.Environment
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import Data.List.Split
import Prelude hiding (readFile) -- Because we want the System.IO.Strict version
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Strict
import Control.Monad
import System.Directory
import System.FilePath
import Text.Regex.TDFA
import Text.Regex.TDFA.String
import Text.Printf

import qualified Data.Map.Lazy as Map

-- See http://stackoverflow.com/q/32149354/370611
toRegex = makeRegexOpts defaultCompOpt{multiline=False} defaultExecOpt

-- | A single usage of something within a Resources section.
usageRE =
  "<[ \t\n\r]*([^ \t\n\r>]+)[^>]*>"
  -- "<[^>]+>"
  -- "<[^>]*>"
  -- "comm*on"

-- | Something that looks like a usage (matches usageRE) but isn't
filterRegex = "(^!--)"       -- Comments
              ++ "|(^/[^>]*)"       -- End tags

resourcesSectionRegex =
  -- "<[ \t\n\r]*(Window|UserControl)\\.Resources[ \t\n\r]*>.*</[ \t\n\r]*(Window|UserControl)\\.Resources[ \t\n\r]*>"
  -- compile blankCompOpt blankExecOpt "<UserControl.Resources>"
  -- "<UserControl.Resources>"
  "<(UserControl|Window|Page).Resources>.*</(UserControl|Window|Page).Resources>"
  -- "{[ \t\n\r]*([ \t\n\r]*(comm*on))+[ \t\n\r]*}"

-- | Program parameters
data PgmParms = PgmParms ([OptFlag], -- ^ Option flags (possibly with values) passed to program
                          [String])  -- ^ Non-option arguments passed to program

-- | True if the given PgmParms contains the given OptFlag
hasOpt :: PgmParms -> OptFlag -> Bool
hasOpt (PgmParms (flags, _)) flag = elem flag flags

-- | True iff the given PgmParams have a filename OptFlag
hasFilenameOpt :: PgmParms -> Bool
hasFilenameOpt (PgmParms (flags, _)) = foldl (||) False (map isFilenameOpt flags)

filenameOptValue :: PgmParms -> [String]
filenameOptValue ((PgmParms (flags, _))) = concat $ map fnov flags
  where fnov (FilenameOpt fns) = splitOn ":" fns
        fnov _ = []

-- | True iff the given PgmParams have a prune OptFlag
hasPruneOpt :: PgmParms -> Bool
hasPruneOpt (PgmParms (flags, _)) = foldl (||) False (map isPruneOpt flags)

pruneOptValue :: PgmParms -> [String]
pruneOptValue ((PgmParms (flags, _))) = concat $ map pov flags
  where pov (PruneOpt dirnames) = splitOn ":" dirnames
        pov _ = []

{-
-- | The value of the given program option
optValue :: PgmParms -> OptFlag -> Maybe OptFlag
optValue (PgmParms (flags, _)) flag = lookup flag flags
-}

-- | Program option flag types
-- <p>Sample usage:
-- <p><code>find-common-resources -p bin:binRel -f xaml`$ -d . -v </code>
-- <p>(Backtic (`) is PowerShell's escape character.)
data OptFlag = DirOpt String    -- ^ Which directory to search (could also specify w/out argument)
             | VerboseOpt       -- ^ Dump verbose logging to stderr
             | FilenameOpt String -- ^ Restrict file select to those matching the given regex
             | PruneOpt String  -- ^ Do not look inside directories matching the given regex
             deriving (Show, Eq)

isFilenameOpt (FilenameOpt _) = True
isFilenameOpt _ = False

isPruneOpt (PruneOpt _) = True
isPruneOpt _ = False

-- | Program option descriptors. Yes, colons is a hack, chosen because ":" isn't a valid Windows filename character.
options :: [OptDescr OptFlag]
options =
  [ Option ['d'] ["dir"]      (ReqArg DirOpt "DIRECTORY")   "Directory in which to start walk",
    Option ['v'] ["verbose"]  (NoArg VerboseOpt)            "Be verbose in output to stderr",
    Option ['f'] ["filename"] (ReqArg FilenameOpt "REGEX")  "Select files whose names match the given regex. Use colons to delimit multiple regexes.",
    Option ['p'] ["prune"]    (ReqArg PruneOpt "REGEX")     "Do not traverse into directories whose names match the given regex. Use colons to delimit multiple regexes."
  ]

-- | Map from resource to files where it is used.
data ResourceToUsageMap = ResourceToUsageMap (Map.Map String [String])

-- | Turn a ResourceToUsageMap into a list of keys and values (each value is itself a list of strings)
resourceEntries :: ResourceToUsageMap -> [(String, [String])]
resourceEntries (ResourceToUsageMap map) = Map.assocs map

-- | Main
main :: IO ()
main = do
  argv <- getArgs
  parms <- getProgramParameters argv
  when (hasOpt parms VerboseOpt) $ do
    dumpProgramParameters parms

  when (hasOpt parms VerboseOpt) $ do
    putStrLn ("Testing printf: " ++ (printf "%s" "hi"))
    printf "%s\n" "hi, there"
    printf "%d\n" (length "hi, there")

  filepaths <- getFilePaths parms
  when (hasOpt parms VerboseOpt) $ do
    hPutStrLn stderr ("Found " ++ (show (length filepaths)) ++ " files.")
  -- TODO: write "verb" function
  -- TODO: Find out how to format (pretty-print?) output more easily.
  when (hasOpt parms VerboseOpt) $ do
    hPutStrLn stderr (fst (foldl (\ (msg, i) fp -> ((msg ++ "\n    " ++ (show (i+1)) ++ ": " ++ fp), i+1))
                         ("", 0) filepaths))
  fileContentsList <- forM filepaths $ \fp -> do readFile fp
  when (hasOpt parms VerboseOpt) $ do
    hPutStrLn stderr  ("fileContentsList has " ++ (show (length fileContentsList)) ++ " entries.")
  let filesAndContents = zip filepaths fileContentsList
      allResources = findUsages parms filesAndContents
    in do (when (hasOpt parms VerboseOpt) $ do
              hPutStr stderr ("Found " ++ ((\(ResourceToUsageMap m) -> (show (length m))) allResources) ++ " usages of resources.")
              hPutStr stderr (foldl (\s (f, rs) -> s ++ "\n    " ++ (show (f, rs))) "" (resourceEntries allResources))
              hPutStrLn stderr "")
          printCommonResources parms allResources
  hPutStrLn stderr  "Done."

dirp :: Maybe String -> OptFlag
dirp = DirOpt . fromMaybe "."

-- | Transforms program arguments to options via getOpt
getProgramParameters :: [String] -> IO PgmParms
getProgramParameters argv = 
  case getOpt Permute options argv of
      (o, n, []) -> return $ PgmParms (o,n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: "
        
-- | Dump program parameters to stderr
dumpProgramParameters :: PgmParms -> IO ()
dumpProgramParameters (PgmParms (optFlags, nonOptStrings)) = do
  hPutStr stderr ("Got options:\n" ++ thingPerLine "    " optFlags)
  hPutStr stderr ("Got non-options:\n" ++ thingPerLine "    " nonOptStrings)
  when (hasFilenameOpt (PgmParms (optFlags, nonOptStrings))) $ do
    hPutStrLn stderr $ "Has filename opt: " ++ (show (filenameOptValue (PgmParms (optFlags, nonOptStrings))))
  when (hasPruneOpt (PgmParms (optFlags, nonOptStrings))) $ do
    hPutStrLn stderr "Has prune opt"
{-
  when (hasOpt (PgmParms (optFlags, nonOptStrings)) (FilenameOpt "x")) $ do
    hPutStrLn stderr "Has filename opt"
-}

-- | Transform a list of things deriving Show into a string, one thing per line, with leading indentation
thingPerLine :: (Show t) => String -> [t] -> String
thingPerLine indent things =
  foldl (\ str thing -> str ++ indent ++ (show thing) ++ "\n") "" things

-- ====================================================================================================

-- | Return a list of file paths to be scanned
getFilePaths :: PgmParms
             -> IO [FilePath]
getFilePaths (PgmParms (optFlags, nonOptStrings)) = do
  let cmdLineDirs = nonOptStrings ++ (map dirName (filter isDirOpt optFlags))
  files <- forM cmdLineDirs $ \dirName -> do
    isDirectory <- doesDirectoryExist dirName
    if isDirectory
      then getRecursiveContents (PgmParms (optFlags, nonOptStrings)) dirName
      else return []
  return (concat files)
  -- return (concat (map getRecursiveContents cmdLineDirs))
  -- []

-- | Returns a list of contents of a directory (not including subdirectories)
getRecursiveContents :: PgmParms -> FilePath -> IO [FilePath]
getRecursiveContents parms topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then (if hasPruneOpt parms
              then (if (foldl (||) False (map (\p -> path =~ p) (pruneOptValue parms)))
                    then return []
                    else getRecursiveContents parms path)
              else getRecursiveContents parms path)
      else (if hasFilenameOpt parms
               then (if (foldl (||) False (map (\p -> path =~ p) (filenameOptValue parms)))
                     then return [path]
                     else return [])
               else return [path])
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
findUsages :: PgmParms
              -> [(FilePath, String)]  -- ^ files to be scanned, along with their (lazy) contents
              -> ResourceToUsageMap
findUsages parms filepathsAndContents =
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
  ResourceToUsageMap (Map.insertWith (++) usageOccurrence [filepath] inputMap)

-- | Return a list of tuples: (filepath, resource)
resourcesUsed :: (FilePath, String) -> [(FilePath, String)]
resourcesUsed (filepath, filecontents) = 
  -- [("foo", "bar")]
  if (match (toRegex resourcesSectionRegex) filecontents )
  then resourcesUsed2 filepath (match (toRegex resourcesSectionRegex) filecontents :: (String, String, String))
  else []

-- | Transform subexpressions found in resources section to (filename,subexpression) pairs.
resourcesUsed2 :: FilePath      -- ^ The filepath searched
               -> (String,String,String) -- ^ Results of regexp match
               -> [(FilePath,String)]
resourcesUsed2 filepath (_, foundSubstring, _) =
  -- trace ("\n>>>>>>>> Possible hit for file " ++ filepath ++ ":\n" ++ foundSubstring)
  foldl (\usages usage -> usage : usages) []
  (map (\ss -> (filepath, ss))
   (filter (\ss -> not (match (toRegex filterRegex) ss))
    (chop (\s -> matchRest (match (toRegex ("[ \t\n\r]*(" ++ usageRE ++ ")")) s :: (String, String, String, [String])))
     foundSubstring)
   )
  )
  where matchRest (_, _, rest, subs) =
          -- usageRE is assumed to contain a single subexpressoin
          (subs !! 1, if (match (toRegex usageRE) rest)
                      then rest
                      else "")

-- | Print those resources in the given map that are used in two or more files
printCommonResources :: PgmParms -> ResourceToUsageMap -> IO ()
printCommonResources parms (ResourceToUsageMap map) = do
  when (hasOpt parms VerboseOpt) $ do
    hPutStrLn stderr "Will print common resources."
    hPutStrLn stderr ("Map has " ++ (show (Map.size map)) ++ " entries.")
    hPutStrLn stderr $ show (length $ filter isCommon (Map.assocs map)) ++ " common usages"
  putStr $ foldl (\string (usage, files) -> string ++ "    " ++ usage ++ "\t" ++ show files ++ "\n") ""
    (filter isCommon (Map.assocs map))
  when ((length $ filter isCommon (Map.assocs map)) == 0) $ do
    putStrLn ""
  when (hasOpt parms VerboseOpt) $ do
    hPrintf stderr "%d total usages\n" (length (Map.assocs map))
    hPrintf stderr "%d common usages\n" (length $ filter isCommon (Map.assocs map))
  where isCommon (u, fs) = length fs > 1
