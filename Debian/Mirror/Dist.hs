{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module Debian.Mirror.Dist where

import Control.Exception as E
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup)
import Data.Time
import Debian.Mirror
import Prelude hiding ((<>))
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
#if MIN_VERSION_time(1,5,0)
import System.Locale hiding (defaultTimeLocale)
#else
import System.Locale
#endif
import System.Posix.Files
import System.Unix.FilePath (dirName, realpath)
import System.Unix.Files
import Text.Help as H
import Text.PrettyPrint.HughesPJ (Doc)
import qualified Text.PrettyPrint.HughesPJ as D

newtype Repository 
    = Repository { unRepository :: FilePath }
      deriving (Eq, Show, Read, Ord, Semigroup, Monoid)
    
newtype Dist = Dist { unDist :: String }
      deriving (Eq, Show, Read, Ord, Semigroup, Monoid)

data Arch 
    = I386
    | AMD64
      deriving (Eq, Show, Read)

ppArch :: Arch -> Doc
ppArch I386 = D.text "i386"
ppArch AMD64 = D.text "amd64"

-- note: we could make the type be (Repository, [(Arch, Dist)]), which
-- is strictly more flexible, but probably not useful in practice. So,
-- we opt for the easier to use interface.
makeDist :: (Repository, [Arch], [Dist]) -> FilePath -> IO ()
makeDist ((Repository repository), arches, dists) destDir =
     do -- checkIfExistsAlready
        allFiles <- mapM (makeDistFileList (archFilter (map (show . ppArch) arches)) repository) (map unDist dists)
        let indices   = concatMap fst allFiles
            poolFiles = id $ concatMap snd allFiles
            directories = nub' $ map (\(_,_,fp) -> dirName fp) (indices ++ poolFiles)
            numDirs = length directories
            files = nub' $ map (\(_,_,fp) -> fp) (indices ++ poolFiles)
            numFiles = length files
        -- mapM_ print indices
        -- mapM_ print poolFiles
        -- mapM_ print directories

        mapM_ (\(i, fp) -> putStrLn ("Creating directory (" ++ show i ++ " of " ++ show numDirs ++ "): " ++ fp) >> 
                          createDirectoryIfMissing True (destDir </> fp) ) (zip [1..numDirs] directories)
        res <- mapM (\(i, fp) -> putStrLn ("Hardlinking file (" ++ show i ++ " of " ++ show numFiles ++"): " ++ fp) >> 
                                makeHardLink repository destDir fp) (zip [1..numFiles] files)
        case catMaybes res of
          [] -> return ()
          missing -> 
              do hPutStrLn stderr $ "The following files where referenced but did not exist in the source repository:"
                 mapM_ (hPutStrLn stderr) missing
    where
      makeHardLink oldDir newDir file = 
          (realpath (oldDir </> file) >>= \realOldFp ->
               createLink realOldFp (newDir </> file) >> return Nothing)
          `E.catch` 
          (\e -> if isDoesNotExistError e 
                then return (Just (oldDir </> file))
                else ioError e)
          
      nub' :: (Ord a) => [a] -> [a]
      nub' = map head . group . sort


data Target
    = Target { targetName :: String
             , basePath :: FilePath
             , dateFormat :: String
             , sourceSpecs :: [SourceSpec]
             }
      deriving (Read, Show, Eq)


data SourceSpec 
    = SourceSpec Status (Repository, [Arch], [Dist]) String
      deriving (Read, Show, Eq)

data Status
    = Active
    | Frozen
      deriving (Read, Show, Eq)

isActive :: SourceSpec -> Bool
isActive (SourceSpec Active _ _) = True
isActive _ = False

-- TODO: the new directory should have the suffix .in-progress, until
-- the update is done. Then do a rename and update the symlink.
updateTarget :: Target -> IO ()
updateTarget (Target targetName basePath dateFormat sources)  =
    do zt <- getZonedTime
       let timestampFP = (targetName ++ "-snapshots") </> (targetName ++"-"++ formatTime defaultTimeLocale dateFormat zt)
           nextDir     = basePath </> timestampFP
           nextDirIP   = nextDir ++ ".in-progress"
           currentDir  = basePath </> targetName
       e <- exists nextDir
       when e (error $ nextDir ++ " already exists.")
       createDirectoryIfMissing True nextDirIP
       let (active, frozen) = partition isActive sources
       mapM_ (makeDist' currentDir nextDirIP) active
       mapM_ (makeDist' currentDir nextDirIP) frozen
       rename nextDirIP nextDir
       forceSymbolicLink timestampFP currentDir
    where
      makeDist' _ nextDir (SourceSpec Frozen dist name) =
          makeDist dist (nextDir </> name)
      makeDist' currentDir nextDir (SourceSpec Active dist@(Repository repoFP,_,_) name) =
          do let currDistDir = (currentDir </> name)
             (do status <- getSymbolicLinkStatus currDistDir
                 if System.Posix.Files.isSymbolicLink status
                  then do let inProgress = (currentDir </> name ++ ".in-progress")
                          makeDist dist inProgress -- perhaps the Repository should be found by running realpath on the symlink ?
                          removeLink currDistDir
                          rename inProgress currDistDir
                  else hPutStrLn stderr $ (currDistDir ++ " is not a symbolic-link, assuming already frozen from previous run.")) 
                `E.catch`
                     (\e -> if isDoesNotExistError e
                           then hPutStrLn stderr $ (currDistDir ++ " does not exist, assuming it is new dist.")
                           else ioError e)
             createSymbolicLink repoFP (nextDir </> name)

exists :: FilePath -> IO Bool
exists fp =
    do existsDir <- doesDirectoryExist fp
       existsFile <- doesFileExist fp
       return (existsDir || existsFile)

       -- when (existsDir || existsFile) (error $ destDir ++ " already exists.")
    


showTargets :: [Target] -> Elements
showTargets = foldr (<>) mempty . map showTarget

showTarget :: Target -> Elements
showTarget (Target name basePath dateFormat sourceSpecs) =
    text name <> text ":" <> 
         rs Nothing (text "base path: " <> text basePath <> br <>
                     text "date format: " <> text dateFormat <> br <>
                     showSourceSpecs sourceSpecs)

showSourceSpecs :: [SourceSpec] -> Elements
showSourceSpecs = foldr (<>) mempty . map showSourceSpec

showSourceSpec :: SourceSpec -> Elements
showSourceSpec (SourceSpec status (Repository repository, arches, dists) name) =
    (text "dist: " <> text name <> 
         rs Nothing (text "status: " <> ppStatus status) <>
         rs Nothing (text "src: " <> text repository) <>
         rs Nothing (text "arches: " <> text (concat (intersperse ( ", ") $ map (show . ppArch) arches))))

ppStatus Active = text "active"
ppStatus Frozen = text "frozen"
