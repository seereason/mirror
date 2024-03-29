{-# LANGUAGE CPP, ForeignFunctionInterface, LambdaCase #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List
import Network.URI
import Prelude hiding ((<>))
import System.Environment hiding (getEnv)
import System.Posix.Env (getEnv)
import System.Exit
import System.IO
import Extra.Terminal

import Text.PrettyPrint.HughesPJ

import Debian.Mirror

data Config = Config { name :: String
                     , distDir :: FilePath 
                     , poolDir :: FilePath 
                     , destURI :: URI
                     }

configs =
    [ Config "freespire-addons" 
             "/var/www/CNRUbuntu" 
             "/var/www/CNRUbuntu" 
             (fromJust $ parseURI "rsync://apt@apt.freespire.org/freespire/live/public/freespire.org/apt2/htdocs/CNRUbuntu" )
    , Config "ubuntu-extra" 
             "/var/www/CNRUbuntuExtra" 
             "/var/www/CNRUbuntuExtra" 
             (fromJust $ parseURI "rsync://apt@apt.freespire.org/freespire/live/public/freespire.org/apt2/htdocs/CNRUbuntuExtra" )
    ]

exitWithHelp =
    do progName <- getProgName
       columns <- return . fromMaybe 80 =<< getWidth
       putStrLn $ renderStyle (Style PageMode columns 1.0) $ 
                text "Usage:" <+> text progName <+> text "<config>" $+$ 
                text [] $+$ 
                text "where <config> is one of:" $+$ text [] $+$
                vcat (map ppConfig configs)
       exitFailure

main =
  getArgs >>= \case
     [configName] -> do
       let mConfig = find ((==) configName . name) configs
       when (isNothing mConfig) $ do hPutStrLn stderr ("Could not find config named " ++ configName)
                                     exitWithHelp
       let config = fromJust mConfig
       pushLocalRelease True (const True) (distDir config) (poolDir config) (destURI config)
     _ -> exitWithHelp

ppConfig :: Config -> Doc
ppConfig (Config name distDir poolDir destURI) =
    hang (text name <> colon) 4 (vcat [ sep [text "parent directory of dists directory:", (nest 4 (text distDir))]
                                      , sep [text "parent directory of pool directory: ", (nest 4 (text poolDir))]
                                      , sep [text "destination uri:", nest 4 (text (show destURI))]
                                      ])


{-
test = 
    let localDist  = fromJust (parseURI "file:/var/www/ubuntu.apt2")
        localPool = fromJust (parseURI "file:/var/www/ubuntu")
        -- remote = fromJust (parseURI "rsync://root@noir/disks/hdb1/test")
        remote    = fromJust (parseURI "rsync://apt@apt.freespire.org/freespire/live/public/freespire.org/apt2/htdocs/ubuntu-archives/current")
    in mirrorRelease localDist localPool remote

-}
