module Debian.Mirror.DistMain where

import Control.Monad
import Debian.Mirror.Dist
import Data.Monoid
import Data.List
import Data.Time
import Extra.Help as H
import Extra.HughesPJ
import Linspire.Unix.FilePath
import Linspire.Unix.Files
import System.Console.GetOpt
import System.Locale
import System.Environment
import System.IO

data Target
    = Target { targetName :: String
             , basePath :: FilePath
             , dateFormat :: String
             , sourceSpecs :: [SourceSpec]
             }
      deriving (Read, Show, Eq)

manpage :: String -> [Target] -> Manpage a
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

opts :: [OptDescr a]
opts =
    [ Option [] ["dump-man-page"] (NoArg undefined) "dump the manpage for this program on stdout and exit immediately. Use groff -mandoc to process the output."
    ]

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
showSourceSpec (SourceSpec (Repository repository, arches, dists) name) =
    (text "dist: " <> text name <> 
         rs Nothing (text "src: " <> text repository) <>
         rs Nothing (text "arches: " <> text (concat (intersperse ( ", ") $ map (show . ppArch) arches))))

updateTarget :: Target -> IO ()
updateTarget (Target name basePath dateFormat sourceSpecs) =
    do zt <- getZonedTime
       let timestampFP =  (name ++ "-snapshots") +/+ (name ++"-"++ formatTime defaultTimeLocale dateFormat zt)
       makeRelease sourceSpecs (basePath +/+ timestampFP)
       forceSymbolicLink timestampFP (basePath +/+ name)

distMain :: [Target] -> IO ()
distMain targets =
    do args <- getArgs
       progName <- getProgName
       when ("--dump-man-page" `elem` args) (dumpManPage (manpage progName targets))
       hPutStrLn stderr =<< usage (manpage progName targets)

