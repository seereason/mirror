{-# OPTIONS_GHC -fglasgow-exts #-}
module Debian.Mirror.Dist where

import Control.Monad
import Data.Monoid
import Data.List
import Debian.Mirror
import Linspire.Unix.FilePath
import Text.PrettyPrint.HughesPJ
import System.Directory
import System.Posix.Files


newtype Repository 
    = Repository { unRepository :: FilePath }
      deriving (Eq, Show, Read, Ord, Monoid)
    
newtype Dist = Dist { unDist :: String }
      deriving (Eq, Show, Read, Ord, Monoid)

data Arch 
    = I386
    | AMD64
      deriving (Eq, Show, Read)

ppArch :: Arch -> Doc
ppArch I386 = text "i386"
ppArch AMD64 = text "amd64"

-- note: we could make the type be (Repository, [(Arch, Dist)]), which
-- is strictly more flexible, but probably not useful in practice. So,
-- we opt for the easier to use interface.
makeDist :: (Repository, [Arch], [Dist]) -> FilePath -> IO ()
makeDist ((Repository repository), arches, dists) destDir =
     do -- checkIfExistsAlready
        distFiles <- mapM (makeDistFileList (archFilter (map (show . ppArch) arches)) repository) (map unDist dists)
        let indices   = concatMap fst distFiles
            poolFiles = nub $ concatMap snd distFiles
            directories = nub $ map (\(_,_,fp) -> dirName fp) (indices ++ poolFiles)
            files = nub $ map (\(_,_,fp) -> fp) (indices ++ poolFiles)
        mapM_ print indices
        mapM_ print poolFiles
        mapM_ print directories
        mapM_ (\fp -> createDirectoryIfMissing True (destDir +/+ fp)) directories
        mapM_ (makeHardLink repository destDir) files
    where
      makeHardLink oldDir newDir file = createLink (oldDir +/+ file) (newDir +/+ file)

data SourceSpec 
    = SourceSpec (Repository, [Arch], [Dist]) String
      deriving (Read, Show, Eq)

makeRelease :: [SourceSpec] -> FilePath -> IO ()
makeRelease sources destDir =
    do e <- exists destDir
       when e (error $ destDir ++ " already exists")
       mapM_ makeDist' sources
    where
      makeDist' (SourceSpec dist name) =
          makeDist dist (destDir +/+ name)

exists :: FilePath -> IO Bool
exists fp =
    do existsDir <- doesDirectoryExist fp
       existsFile <- doesFileExist fp
       return (existsDir || existsFile)

       -- when (existsDir || existsFile) (error $ destDir ++ " already exists.")
    
