module Main where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List
import Network.URI
import System.Environment
import System.Exit
import System.IO
import Text.PrettyPrint.HughesPJ

import Debian.Mirror

data Config = Config { name :: String
                     , distDir :: FilePath 
                     , poolDir :: FilePath 
                     , destURI :: URI
                     }

configs =
    [ Config "skipjack-feisty" 
             "/var/www/CNRUbuntu" 
             "/var/www/CNRUbuntu" 
             (fromJust $ parseURI "rsync://apt@apt.freespire.org/freespire/live/public/freespire.org/apt2/htdocs/CNRUbuntu" )
    , Config "feisty-extra" 
             "/var/www/CNRUbuntuExtra" 
             "/var/www/CNRUbuntuExtra" 
             (fromJust $ parseURI "rsync://apt@apt.freespire.org/freespire/live/public/freespire.org/apt2/htdocs/CNRUbuntuExtra" )
    ]

exitWithHelp =
    do progName <- getProgName
       columns <- return . either (const 80) read =<< try (getEnv "COLUMNS")
       putStrLn $ renderStyle (Style PageMode columns 1.0) $ 
                text "Usage:" <+> text progName <+> text "<config>" $+$ 
                text [] $+$ 
                text "where <config> is one of:" $+$ text [] $+$
                vcat (map ppConfig configs)
       exitFailure

main =
    do args <- getArgs
       when (not $ singleton args) exitWithHelp
       let configName = head args
           mConfig = find ((==) configName . name) configs
       when (isNothing mConfig) $ do hPutStrLn stderr ("Could not find config named " ++ configName)
                                     exitWithHelp
       let config = fromJust mConfig
       print configName
       print $ ppConfig config




ppConfig :: Config -> Doc
ppConfig (Config name distDir poolDir destURI) =
    hang (text name <> colon) 4 (vcat [ sep [text "parent directory of dists directory:", (nest 4 (text distDir))]
                                      , sep [text "parent directory of pool directory: ", (nest 4 (text poolDir))]
                                      , sep [text "destination uri:", nest 4 (text (show destURI))]
                                      ])

singleton :: [a] -> Bool
singleton [_] = True
singleton _ = False



{-
test = 
    let localDist  = fromJust (parseURI "file:/var/www/ubuntu.apt2")
        localPool = fromJust (parseURI "file:/var/www/ubuntu")
        -- remote = fromJust (parseURI "rsync://root@noir/disks/hdb1/test")
        remote    = fromJust (parseURI "rsync://apt@apt.freespire.org/freespire/live/public/freespire.org/apt2/htdocs/ubuntu-archives/current")
    in mirrorRelease localDist localPool remote

-}
