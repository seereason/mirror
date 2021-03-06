module Debian.Mirror.DistMain where

import Control.Monad
import Debian.Mirror.Dist
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Time
import Extra.HughesPJ
import System.Unix.FilePath
import System.Locale
import System.Environment
import System.IO
import System.Exit
import Text.Help as H
import Text.Help.GetOpt

manpage :: String -> [Target] -> Manpage Opts
manpage progName targets =
    Manpage { name		= progName 
            , sectionNum	= General
            , shortDesc		= text "tool to keep mirrors of various repositories up to date."
            , synopsis		= text (progName ++ " TARGET...")
            , description	= text "update the mirrors named on the command line."
            , H.options		= Just opts
            , extraSections	= Just [targetSection]
            , files		= Nothing
            , environment	= Nothing
            , diagnostics	= Nothing
            , bugs		= Nothing
            , authors		= Just [("Jeremy Shaw", "jeremy.shaw@linspire.com")]
            , seeAlso		= Nothing
            }
        where
          targetSection :: (ShowIn, Text, Elements)
          targetSection = (InBoth, (text "TARGETS"), (showTargets targets))

data Opts 
    = DumpManPage
    | Help

opts :: [OptDescr Opts]
opts =
    [ Option []    ["dump-man-page"] (NoArg DumpManPage) 
                 (text "dump the manpage for this program on stdout and exit immediately. Use groff -mandoc to process the output.")
    , Option ['h'] ["help"]          (NoArg Help) 
                 (text "show this help text.")
    ]

{-
updateTarget :: Target -> IO ()
updateTarget (Target targetName basePath dateFormat sourceSpecs) =
    do zt <- getZonedTime
       let timestampFP =  (targetName ++ "-snapshots") +/+ (targetName ++"-"++ formatTime defaultTimeLocale dateFormat zt)
       makeRelease sourceSpecs (basePath +/+ timestampFP)
       forceSymbolicLink timestampFP (basePath +/+ targetName)
-}

-- TODO: verify that the targetNames are unique
distMain :: [Target] -> IO ()
distMain targets =
    do args <- getArgs
       progName <- getProgName
       when ("--dump-man-page" `elem` args) (dumpManPage (manpage progName targets))
       when ("--help" `elem` args) (usage (manpage progName targets) >>= putStrLn >> exitWith ExitSuccess)
       case (partition (isTargetName targets) args) of
         (tgts, unknown)
             | not (null unknown) ->
                 do hPutStrLn stderr $ (if singleton unknown then "unknown target: " else "unknown targets: ") ++ show unknown
                    hPutStrLn stderr =<< usage (manpage progName targets)
             | null tgts ->
                 do hPutStrLn stderr $ "You must specify at least one target."
                    hPutStrLn stderr =<< usage (manpage progName targets)
             | otherwise -> mapM_ (updateTarget . lookupTarget targets) tgts
    where
      singleton [_] = True
      singleton _   = False
      isTargetName targets name = any (\t -> name == targetName t) targets
      lookupTarget targets name = fromJust $ find (\t -> name == targetName t) targets

