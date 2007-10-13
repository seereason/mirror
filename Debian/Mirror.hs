{-# LANGUAGE TypeSynonymInstances #-}
module Debian.Mirror
    (pushLocalRelease
    )
    where

import Control.Concurrent
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List
import Data.Time
import qualified Data.ByteString.Char8 as B
import Debian.Control.ByteString
import Linspire.Unix.FilePath
import qualified Linspire.Unix.Misc as M
import Network.URI
import System.Directory
import System.IO
import System.Process
import System.Posix.Files
import System.Exit
import System.Locale

type ByteString = B.ByteString

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

pushLocalRelease :: FilePath
                 -> FilePath
                 -> URI
                 -> IO ()
pushLocalRelease sourceDistFP sourcePoolFP destURI =
    mirrorRelease (fromJust $ parseURI ("file:" ++ sourceDistFP)) (fromJust $ parseURI ("file:" ++ sourcePoolFP)) destURI

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
mirrorRelease :: URI -- ^ base URI of release (for dist files)
              -> URI -- ^ base URI of release source (for pool files)
              -> URI -- ^ base URI of release dest
              -> IO ()
mirrorRelease sourceDist sourcePool dest
    | (uriScheme sourceDist) == "file:" && (uriScheme sourcePool) == "file:" && (uriScheme dest == "rsync:") =
        do when (uriAuthority sourceDist /= Nothing) (error $ "file:/ should only have one slash.")
           putStrLn "Creating list of files in Release"
           (distFiles, poolFiles) <- makeFileList (uriPath sourceDist)
           dest' <- createDestDir dest
           putStrLn "rsync'ing index files."
           let (root, files, dest'') = fudgePath (uriPath sourceDist) distFiles dest'
           rsync root files dest''
           putStrLn "rsync'ing pool files."
           let (root, files, dest'') = fudgePath (uriPath sourcePool) poolFiles dest'
           rsync root files dest''
           updateSymLink dest' dest
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
      then ((fp +/+ prefix), map (drop plength) files', uri { uriPath = escapeURIString isUnescapedInURI $ (uriPath uri) +/+ prefix })
      else error ("These files do not have the correct prefix, " ++ prefix ++ "\n" ++ unlines invalid)
          
        


rsync :: FilePath -> [FilePath] -> URI -> IO ()
rsync srcDir files remote =
    let auth = maybe (error $ show remote ++ " is missing authority information.") id (uriAuthority remote)
        remote' = uriUserInfo auth ++ uriRegName auth ++ ":" ++ uriPath remote
    in
      do (inh, outh, errh, ph) <- runInteractiveProcess "rsync" ["-a","--progress","--delete","--files-from","-", srcDir, remote'] Nothing Nothing -- add delete option
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
                                            , "	echo Making new copy of target directory $(readlink " ++ escapeShell  basename ++ ") -\\> " ++ basename ++ "-" ++ dateStamp ++ " ;" 
                                            , "  cp -al $(readlink " ++ escapeShell basename ++ ") " ++ escapeShell basename ++ "-" ++ dateStamp ++ " ; "
                                            , "else"
                                            , "	echo " ++ basename ++ " is not a symlink ;"
                                            , "	exit 1 ;"
                                            , "fi"
                                            ]
       when (ec /= ExitSuccess) (error $ "Failed to create directory on destination server: " ++ show dest)
       return $ (dest { uriPath = escapeURIString isUnescapedInURI $ (uriPath dest) ++ "-" ++ dateStamp })

data CheckSums 
    = CheckSums { md5sum :: Maybe String
                , sha1 :: Maybe String
                , sha256 :: Maybe String
                }
      deriving (Read, Show, Eq)

-- | left list is list of files from dists directory
--   right list is list of files from pool directory
makeFileList :: FilePath -> IO ([(CheckSums, Integer, FilePath)], [(CheckSums, Integer, FilePath)])
makeFileList repoDir =
    do releases <- findReleases repoDir
       liftM (\l -> (concatMap fst l, concatMap snd l)) $ mapM (makeDistFileList repoDir) releases

mergeLists :: [([a],[a])] -> ([a],[a])
mergeLists l = (concatMap fst l, concatMap snd l)

findReleases :: FilePath -> IO [String]
findReleases repoDir =
    do let distsDir = repoDir +/+ "dists"
       e <- fileExist distsDir
       if not e
          then error (distsDir ++ " does not exist.") -- return []
          else do 
                  contents <- getDirectoryContents distsDir
                  dirs <- filterM (isRealDir distsDir) $ filter (\d -> (d /= ".") && (d /= "..")) contents
                  release <- filterM (\dir -> fileExist (distsDir +/+ dir +/+ "Release")) dirs
                  return release
    where
      isRealDir base fp =
          do isDir <- liftM isDirectory $ getFileStatus (base +/+ fp)
             if isDir
                then liftM not $ isSymLink (base +/+ fp)
                else return False
                        

-- TODO: move to unix utils
isSymLink path = getSymbolicLinkStatus path >>= return . isSymbolicLink

-- TODO: check GPG signatures
-- TODO: include all sums for control files
-- TODO: don't assume Packages file exists, (possibly only .gz or .bz2 exists)
-- TODO: include Release Release.gpg, Contents, etc in control file list
-- returns:
-- the left list is the file paths relative to repoDir for the dist files
-- the right list is the file paths relative to repoDir for the pool files
-- they are returned seperately in case the parent of the dist and pool directories are different.
makeDistFileList :: FilePath -> String -> IO ([(CheckSums, Integer, FilePath)], [(CheckSums, Integer, FilePath)])
makeDistFileList repoDir distName =
    do let distDir = repoDir +/+ "dists" +/+ distName
           releaseFP = distDir +/+ "Release"
       release <- parseControlFromFile releaseFP
       case release of
         (Left e) -> error (show e)
         (Right (Control [p])) -> 
             let md5sums =
                     case fieldValue "MD5Sum" p of
                       (Just md5) -> md5
                       Nothing ->
                           case fieldValue "Md5Sum" p of
                             (Just md5) -> md5
                             Nothing -> error $ "Did not find MD5Sum field in " ++ releaseFP
             in
                   do let controlFiles = map (makeTuple . B.words) $ filter (not . B.null) (B.lines md5sums)
                          packages = filter (\(_,_,fp) -> ("Packages" `isSuffixOf` fp)) controlFiles
                          sources = filter (\(_,_,fp) -> ("Sources" `isSuffixOf` fp)) controlFiles
                      -- this does not get the .gz and .bz2 files ?
                      packageFiles <- mapM (makePackageFileListIO distDir) packages
                      sourceFiles <- mapM (makeSourceFileListIO distDir) sources
                      cf <- contentsFiles distDir
                      distFiles <- mapM (makeOther distDir) ("Release" : "Release.gpg" : cf) >>= return . catMaybes
                      -- mapM_ print packages
                      -- mapM_ print sources
                      -- mapM_ print otherFiles
                      -- mapM_ print (concat packageFiles)
                      -- mapM_ print (concat sourceFiles)
                      return $ (map (\(c,s,fp) -> (c,s,"dists" +/+ distName +/+fp)) $ distFiles ++ controlFiles, concat (packageFiles ++ sourceFiles))
         (Right _) -> error $ "Did not find exactly one paragraph in " ++ releaseFP
    where
      makeTuple :: [B.ByteString] -> (CheckSums, Integer, FilePath)
      makeTuple [md5sum, size, fp] = (CheckSums { md5sum = Just (B.unpack md5sum), sha1 = Nothing, sha256 = Nothing }, read (B.unpack size), B.unpack fp)
      makeOther :: FilePath -> FilePath -> IO (Maybe (CheckSums, Integer, FilePath))
      makeOther basePath fp =
          do e <- fileExist (basePath +/+ fp)
             if not e
              then return Nothing
              else do size <- getFileStatus (basePath +/+ fp) >>= return . fromIntegral . fileSize
                      md5 <- M.md5sum (basePath +/+ fp)
                      return $ Just (CheckSums { md5sum = Just md5, sha1 = Nothing, sha256 = Nothing }, size, fp)
      contentsFiles :: FilePath -> IO [FilePath]
      contentsFiles distDir =
          do files <- getDirectoryContents distDir
--             print files
--             print $ filter (isPrefixOf "Contents-" . baseName) files
             return $ filter (isPrefixOf "Contents-" . baseName) files


-- |TODO: check sums \/ filesizes
makePackageFileListIO :: FilePath -> (CheckSums, Integer, FilePath) -> IO [(CheckSums, Integer, FilePath)]
makePackageFileListIO distDir (checkSums, size, fp) =
     (parseControlFromFile (distDir +/+ fp)) >>= either (error . show) (return . makePackageFileList)

-- |TODO: improve error message
makePackageFileList :: Control -> [(CheckSums, Integer, FilePath)]
makePackageFileList (Control paragraphs) =
    map makeParagraphTuple paragraphs
    where
      makeParagraphTuple p =
          let fp     = maybe (error $ "Paragraph missing Filename field:\n" ++ show p) B.unpack (fieldValue "Filename" p)
              size   = maybe (error $ "Paragraph missing Size field") (read . B.unpack) (fieldValue "Size" p)
              md5sum = fmap B.unpack $ case fieldValue "MD5Sum" p of 
                                         m@(Just _) -> m
                                         Nothing -> fieldValue "Md5Sum" p
              sha1   = fmap B.unpack $ fieldValue "SHA1" p
              sha256 = fmap B.unpack $ fieldValue "SHA256" p
          in (CheckSums { md5sum = md5sum, sha1 = sha1, sha256 = sha256 }, size, fp)
                  

-- |TODO: check sums \/ filesizes
makeSourceFileListIO :: FilePath -> (CheckSums, Integer, FilePath) -> IO [(CheckSums, Integer, FilePath)]
makeSourceFileListIO distDir (checkSums, size, fp) =
     (parseControlFromFile (distDir +/+ fp)) >>= either (error . show) (return . makeSourceFileList)

makeSourceFileList :: Control -> [(CheckSums, Integer, FilePath)]
makeSourceFileList (Control paragraphs) =
    concatMap makeParagraphTuple paragraphs
    where
      makeParagraphTuple p =
          let files = maybe (error $ "Paragraph missing Files field") id (fieldValue "Files" p)
              directory = maybe (error $ "Paragraph missing Directory field") B.unpack (fieldValue "Directory" p)
          in map (makeTuple directory . B.words) $ filter (not . B.null) $ B.lines $ files
      makeTuple :: FilePath -> [B.ByteString] -> (CheckSums, Integer, FilePath)
      makeTuple directory [md5sum, size, fp] = (CheckSums { md5sum = Just (B.unpack md5sum), sha1 = Nothing, sha256 = Nothing }, read (B.unpack size), directory +/+ B.unpack fp)


-- |only public key based, ssh access currently supported
remoteCommand :: URI -> String -> IO ExitCode
remoteCommand uri cmd =
    case (uriScheme uri, uriAuthority uri) of
      (scheme,Just auth) | scheme == "ssh:" || scheme == "rsync:" ->
          do let port = case uriPort auth of "" -> "22"; n -> show n
             let dest = uriUserInfo auth ++ uriRegName auth
                 path = escapeShell (unEscapeString (uriPath uri))
             (inh, outh, errh, ph) <- runInteractiveProcess "ssh" ["-o","PreferredAuthentications hostbased,publickey","-T","-p",port, dest] Nothing Nothing
             forkIO $ hGetContents outh >>= hPutStr stdout >> hFlush stdout
             forkIO $ hGetContents errh >>= hPutStr stderr >> hFlush stderr
             forkIO $ hPutStr inh ("cd " ++ path ++ " && " ++ cmd)
             ec <- waitForProcess ph
             return ec
      _ -> error $ "Invalid argument to remoteCommand (only ssh is supported): " ++ show uri

hPutField :: Handle -> Field' ByteString -> IO ()
hPutField h (Field (a, v)) =
    B.hPutStr h a >> hPutStr h ":" >> B.hPutStr h v

hPutParagraph :: Handle -> Paragraph' ByteString -> IO ()
hPutParagraph h (Paragraph fields) =
    mapM_ (\f -> hPutField h f >> hPutStrLn h "") fields

hPutControl :: Handle -> Control' ByteString -> IO ()
hPutControl h (Control paragraphs) =
    mapM_ (\p -> hPutParagraph h p >> hPutStrLn h "") paragraphs

-- |This may have bad performance issues 
instance Show (Control' ByteString) where
    show (Control paragraph) = concat (intersperse "\n" (map show paragraph))

instance Show (Paragraph' ByteString) where
    show (Paragraph fields) = unlines (map show fields)

instance Show (Field' ByteString) where
    show (Field (name,value)) = (B.unpack name) ++":"++ (B.unpack value)
    show (Comment text) = B.unpack text


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

