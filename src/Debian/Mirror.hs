{-# LANGUAGE CPP, FlexibleInstances #-}
module Debian.Mirror
    (pushLocalRelease
    , remoteCommand
    , makeDistFileList
    , makePackageFileListIO
    , makeSourceFileListIO
    , archFilter
    , md5sumField
    )
    where

import Control.Arrow
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Function
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debian.Apt.Index
import Debian.Control.Text
import Network.URI
import System.Directory
import System.Exit
import System.FilePath
import System.IO
#if MIN_VERSION_time(1,5,0)
import System.Locale hiding (defaultTimeLocale)
#else
import System.Locale
#endif
import System.Posix.Files
import System.Process
import System.Unix.FilePath
import qualified System.Unix.Misc as M
import Distribution.Pretty (prettyShow)
import Text.Regex.Posix

{-

 + Mirror from a remote machine to the local machine.
 + Mirror from the local machine to a remote machine.
 + Mirror only part of an archive
 + Wayback machine functionality
 + Make sure the mirror is never live in an inconsintant state
 + Verify that all the files transfered have valid sums/signatures
 + Resume gracefully from aborted mirror
 + Use a specific Packages/Sources file to make the mirror

-}
{-
mirrorTo :: [(SourcesListLine, [Arches])] -> Target -> IO Result
mirrorTo = undefined
-}
{-
When we mirror contents, we need to:
 + update the Release and Release.gpg files
   - but updating would involving reading in existing stuff, which is lame
-}

archFilter :: [String] -> (FilePath -> Bool)
archFilter arches =
    (=~ (".*/" ++ (concat (intersperse "|" (map (\arch -> "binary-" ++ arch) arches))) ++ "|source/Sources.*"))

-- ".*/binary-i386/(Packages|Release)|source/Sources.*"

pushLocalRelease :: Bool
                 -> (FilePath -> Bool)
                 -> FilePath
                 -> FilePath
                 -> URI
                 -> IO ()
pushLocalRelease updateSymLink filterp sourceDistFP sourcePoolFP destURI =
    mirrorRelease updateSymLink filterp (fromJust $ parseURI ("file:" ++ sourceDistFP)) (fromJust $ parseURI ("file:" ++ sourcePoolFP)) destURI

-- |mirror a specific Packages \/ Sources file to a remote server
mirrorContentsTo :: Control -- ^ control file used as source of packages\/versions
                 -> URI -- ^ where to look for files
                 -> URI -- ^ where to upload the files
                 -> IO () -- ^ result
mirrorContentsTo control source destination = undefined

-- |we can mirror releases (also known as dists)
-- independantly. Because there is nothing above a release that
-- records an sums\/signatures the difficulty is that the pool is
-- shared between releases, so we need a tool that picks out only the
-- files needed by a release. We allow the dist files and pool files
-- to be located at different base URIs. This is so you can make a
-- stripped down release on your local system, and mirror the files
-- from a different location.
--
--
-- TODO: should have option to ignore missing indexes in Release so
-- that we do not have to hack the Release thus invalidating
-- Release.gpg
--
-- TODO: use bzlib\/zlib bindings to read compressed Packages index files
mirrorRelease :: Bool
              -> (FilePath -> Bool)
              -> URI -- ^ base URI of release (for dist files)
              -> URI -- ^ base URI of release source (for pool files)
              -> URI -- ^ base URI of release dest
              -> IO ()
mirrorRelease updateSymLink' filterp sourceDist sourcePool dest
    | (uriScheme sourceDist) == "file:" && (uriScheme sourcePool) == "file:" && (uriScheme dest == "rsync:") =
        do when (uriAuthority sourceDist /= Nothing) (error $ "file:/ should only have one slash.")
           putStrLn "Creating list of files in Release"
           (distFiles, poolFiles) <- makeFileList filterp (uriPath sourceDist)
           putStrLn "Creating destination directory on remote machine"
           dest' <- createDestDir dest
           putStrLn "rsync'ing index files."
           let (root, files, dest'') = fudgePath (uriPath sourceDist) distFiles dest'
           rsync root files dest''
           putStrLn "rsync'ing pool files."
           let (root, files, dest'') = fudgePath (uriPath sourcePool) poolFiles dest'
           rsync root files dest''
           when (updateSymLink') (updateSymLink dest' dest)
           return ()
    | otherwise = error $ "currently the source dist and pool must be on the local file system, and files must be transfered to the remote system via rsync, sorry :("

fudgePath :: FilePath -> [(CheckSums, Integer, FilePath)] -> URI -> (FilePath, [FilePath], URI)
fudgePath fp [] _ = error $ "no files to transfer, can't fudge path"
fudgePath fp files uri =
    let files' = map (\(_,_,fp) -> fp) files
        prefix = takeWhile (/= '/') (head files')
        prefixSlash = prefix ++ "/"
        plength = length prefixSlash
        invalid = filter (not . (prefixSlash `isPrefixOf`)) files'
    in
      if null invalid
      then ((fp </> prefix), map (drop plength) files', uri { uriPath = escapeURIString isUnescapedInURI $ (uriPath uri) </> prefix })
      else error ("These files do not have the correct prefix, " ++ prefix ++ "\n" ++ unlines invalid)

rsync :: FilePath -> [FilePath] -> URI -> IO ()
rsync srcDir files remote =
    let auth = maybe (error $ show remote ++ " is missing authority information.") id (uriAuthority remote)
        remote' = uriUserInfo auth ++ uriRegName auth ++ ":" ++ uriPath remote
    in
      do (inh, outh, errh, ph) <- runInteractiveProcess "rsync" ["-a","--progress",{- "--delete", -} "--files-from","-", srcDir, remote'] Nothing Nothing -- add delete option
         forkIO $ hGetContents outh >>= hPutStr stdout >> hFlush stdout
         forkIO $ hGetContents errh >>= hPutStr stderr >> hFlush stderr
         forkIO $ hPutStr inh (unlines files)
         ec <- waitForProcess ph
         when (ec /= ExitSuccess) (error $ "rsync failed.")

updateSymLink :: URI -> URI -> IO ()
updateSymLink dest' dest
    | (uriAuthority dest) == (uriAuthority dest') =
        let basename  = baseName (escapeShell (unEscapeString (uriPath dest)))
            basename' = baseName (escapeShell (unEscapeString (uriPath dest')))
            dirname   = dirName (escapeShell (unEscapeString (uriPath dest)))
            parent    = dest { uriPath = dirname }
        in
          do ec <- remoteCommand parent $ "ln -snf " ++ basename' ++ " " ++ basename
             when (ec /= ExitSuccess) (error "Remote symlink failed.")
    | otherwise = error $ "Can't symlink across authorities: " ++ show (dest, dest')

createDestDir :: URI -> IO URI
createDestDir dest =
    do let basename = baseName (escapeShell (unEscapeString (uriPath dest)))
           dirname  = dirName (escapeShell (unEscapeString (uriPath dest)))
           parent = dest { uriPath = dirname }
       dateStamp <- getCurrentTime >>= return . formatTime defaultTimeLocale "%Y%m%d_%H:%M:%S"
       ec <- remoteCommand parent $ unlines [ "if [ -h " ++ escapeShell basename  ++ " ] ; then"
                                            , " echo Making new copy of target directory $(readlink " ++ escapeShell  basename ++ ") -\\> " ++ basename ++ "-" ++ dateStamp ++ " ;"
                                            , "  cp -al $(readlink " ++ escapeShell basename ++ ") " ++ escapeShell basename ++ "-" ++ dateStamp ++ " ; "
                                            , "else"
                                            , " echo " ++ basename ++ " is not a symlink ;"
                                            , " exit 1 ;"
                                            , "fi"
                                            ]
       when (ec /= ExitSuccess) (error $ "Failed to create directory on destination server: " ++ show dest)
       return $ (dest { uriPath = escapeURIString isUnescapedInURI $ (uriPath dest) ++ "-" ++ dateStamp })


-- | left list is list of files from dists directory
--   right list is list of files from pool directory
-- NOTE: duplicates are not remove if a file appears in more than one dist
makeFileList :: (FilePath -> Bool) -> FilePath -> IO ([(CheckSums, Integer, FilePath)], [(CheckSums, Integer, FilePath)])
makeFileList filterp repoDir  =
    do releases <- findReleases repoDir
       liftM (\l -> (concatMap fst l, concatMap snd l)) $ mapM (makeDistFileList filterp repoDir) releases

mergeLists :: [([a],[a])] -> ([a],[a])
mergeLists l = (concatMap fst l, concatMap snd l)

findReleases :: FilePath -> IO [String]
findReleases repoDir =
    do let distsDir = repoDir </> "dists"
       e <- fileExist distsDir
       if not e
          then error (distsDir ++ " does not exist.") -- return []
          else do contents <- getDirectoryContents distsDir
                  dirs <- filterM (isRealDir distsDir) $ filter (\d -> (d /= ".") && (d /= "..")) contents
                  release <- filterM (\dir -> fileExist (distsDir </> dir </> "Release")) dirs
                  return release
    where
      isRealDir base fp =
          do isDir <- liftM isDirectory $ getFileStatus (base </> fp)
             if isDir
                then liftM not $ isSymLink (base </> fp)
                else return False

-- TODO: move to unix utils
isSymLink path = getSymbolicLinkStatus path >>= return . System.Posix.Files.isSymbolicLink

data IndexFile =
    IndexFile { uncompressed	:: Maybe (CheckSums, Integer, FilePath)
              , gz 		:: Maybe (CheckSums, Integer, FilePath)
              , bzip2		:: Maybe (CheckSums, Integer, FilePath)
              }

-- TODO: check GPG signatures
-- TODO: include all sums for control files
-- returns:
-- the left list is the file paths relative to repoDir for the dist files
-- the right list is the file paths relative to repoDir for the pool files
-- they are returned seperately in case the parent of the dist and pool directories are different.
-- JAS: btw, this code is horrible, sorry about that.
makeDistFileList :: (FilePath -> Bool) -> FilePath -> String -> IO ([(CheckSums, Integer, FilePath)], [(CheckSums, Integer, FilePath)])
makeDistFileList filterP repoDir distName =
    do let distDir = repoDir </> "dists" </> distName
           releaseFP = distDir </> "Release"
       release <- parseControlFromFile releaseFP
       case release of
         (Left e) -> error (show e)
         (Right control) ->
             do let indexFiles = indexesInRelease filterP control
                packages <- findIndexes distDir "Packages" indexFiles
                sources  <- findIndexes distDir "Sources"  indexFiles
                packageFiles <- mapM (makePackageFileListIO distDir) packages
                sourceFiles  <- mapM (makeSourceFileListIO  distDir) sources
                cf <- findContentsFiles filterP distDir
                otherFiles <- mapM (tupleFromFilePath distDir) ("Release" : "Release.gpg" : cf) >>= return . catMaybes
                return $ (map (\(c,s,fp) -> (c,s,"dists" </> distName </>fp)) $ otherFiles ++ indexFiles, concat (packageFiles ++ sourceFiles))
    where
      makeTuple :: [T.Text] -> (CheckSums, Integer, FilePath)
      makeTuple [md5sum, size, fp] = (CheckSums { md5sum = Just (T.unpack md5sum), sha1 = Nothing, sha256 = Nothing }, read (T.unpack size), T.unpack fp)


      -- this is monoid or monadplus ?
      -- preferred :: (FilePath, Compression) -> (FilePath, Compression) -> (FilePath, Compression)
{-
      preferred (fp1, t1) (fp2, t2)
          | t1 == t2 = (fp1, t1) -- shouldn't happen, perhaps an error is needed  ?
          | t1 == Uncompressed = (fp1, t1)
          | t2 == Uncompressed = (fp2, t2)
          | t1 == BZ2 = (fp1, t1)
          | t2 == BZ2 = (fp2, t2)
          | otherwise = (fp1, t1)
-}


-- |TODO: check sums \/ filesizes
makePackageFileListIO :: FilePath -> ((CheckSums, Integer, FilePath), Compression) -> IO [(CheckSums, Integer, FilePath)]
makePackageFileListIO distDir ((checkSums, size, fp), compression) =
     (controlFromIndex' compression (distDir </> fp)) >>= either (error . show) (return . makePackageFileList)

-- |TODO: improve error message
makePackageFileList :: Control' T.Text -> [(CheckSums, Integer, FilePath)]
makePackageFileList (Control paragraphs) =
    map makeParagraphTuple paragraphs
    where
      makeParagraphTuple p =
          let fp     = maybe (error $ "Paragraph missing Filename field:\n" ++ prettyShow p) T.unpack (fieldValue "Filename" p)
              size   = maybe (error $ "Paragraph missing Size field") (read . T.unpack) (fieldValue "Size" p)
              md5sum = fmap T.unpack $ md5sumField p
              sha1   = fmap T.unpack $ fieldValue "SHA1" p
              sha256 = fmap T.unpack $ fieldValue "SHA256" p
          in (CheckSums { md5sum = md5sum, sha1 = sha1, sha256 = sha256 }, size, fp)

-- |look up the md5sum file in a paragraph
-- Tries several different variations:
--  MD5Sum:
--  Md5Sum:
--  MD5sum:
md5sumField :: (ControlFunctions a) => Paragraph' a -> Maybe a
md5sumField p =
    case fieldValue "MD5Sum" p of
      m@(Just _) -> m
      Nothing ->
          case fieldValue "Md5Sum" p of
            m@(Just _) -> m
            Nothing -> fieldValue "MD5sum" p

-- |TODO: check sums \/ filesizes
makeSourceFileListIO :: FilePath -> ((CheckSums, Integer, FilePath), Compression) -> IO [(CheckSums, Integer, FilePath)]
makeSourceFileListIO distDir ((checkSums, size, fp), compression) =
     (controlFromIndex' compression (distDir </> fp)) >>= either (error . show) (return . makeSourceFileList)

makeSourceFileList :: Control -> [(CheckSums, Integer, FilePath)]
makeSourceFileList (Control paragraphs) =
    concatMap makeParagraphTuple paragraphs
    where
      makeParagraphTuple p =
          let files = maybe (error $ "Paragraph missing Files field") id (fieldValue "Files" p)
              directory = maybe (error $ "Paragraph missing Directory field") T.unpack (fieldValue "Directory" p)
          in map (makeTuple directory . T.words) $ filter (not . T.null) $ T.lines $ files
      makeTuple :: FilePath -> [T.Text] -> (CheckSums, Integer, FilePath)
      makeTuple directory [md5sum, size, fp] = (CheckSums { md5sum = Just (T.unpack md5sum), sha1 = Nothing, sha256 = Nothing }, read (T.unpack size), directory </> T.unpack fp)


-- |only public key based, ssh access currently supported
remoteCommand :: URI -> String -> IO ExitCode
remoteCommand uri cmd =
    case (uriScheme uri, uriAuthority uri) of
      (scheme,Just auth) | scheme == "ssh:" || scheme == "rsync:" ->
          do let port = case uriPort auth of "" -> "22"; n -> show n
             let dest = uriUserInfo auth ++ uriRegName auth
                 path = escapeShell (unEscapeString (uriPath uri))
             (inh, outh, errh, ph) <- runInteractiveProcess "ssh" ["-o","PreferredAuthentications=hostbased,publickey","-T","-p",port, dest] Nothing Nothing
             hSetBuffering inh NoBuffering
             hSetBuffering outh NoBuffering
             hSetBuffering errh NoBuffering
             forkIO $ hGetContents outh >>= hPutStr stdout >> hFlush stdout
             forkIO $ hGetContents errh >>= hPutStr stderr >> hFlush stderr
             forkIO $ hPutStr inh ("cd " ++ path ++ " && " ++ cmd)
             ec <- waitForProcess ph
             return ec
      _ -> error $ "Invalid argument to remoteCommand (only ssh is supported): " ++ show uri

hPutField :: Handle -> Field' T.Text -> IO ()
hPutField h (Field (a, v)) =
    T.hPutStr h a >> hPutStr h ":" >> T.hPutStr h v

hPutParagraph :: Handle -> Paragraph' T.Text -> IO ()
hPutParagraph h (Paragraph fields) =
    mapM_ (\f -> hPutField h f >> hPutStrLn h "") fields

hPutControl :: Handle -> Control' T.Text -> IO ()
hPutControl h (Control paragraphs) =
    mapM_ (\p -> hPutParagraph h p >> hPutStrLn h "") paragraphs

-- |function returns true if character is needs escaping
escapeWithBackslash :: (Char -> Bool) -> String -> String
escapeWithBackslash p str = concatMap escapeChar str
    where
      escapeChar c
          | p c       = ['\\', c]
          | otherwise = [c]

-- what about '!'
isSpecialInShell c = c `elem` " \"'\\$;[]()&?*"

-- does not escape \/
escapeShell = escapeWithBackslash isSpecialInShell

