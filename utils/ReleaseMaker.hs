#!/usr/bin/runhaskell
module Main where

import Debian.Mirror.Dist
import Debian.Mirror.DistMain

main = distMain targets

targets :: [Target]
targets = 
    [ skipjackFeistyTarget
    , ubuntuFeistyTarget
    , ubuntuGutsyTarget
    ]

-- * Repositories

ubuntuRepo		= Repository "/srv/mirrors/ubuntu.com/ubuntu"

-- * Dists

-- ** Ubuntu Dists

-- *** Feisty
feisty 			= Dist "feisty"
feisty_backports 	= Dist "feisty-backports"
feisty_updates 		= Dist "feisty-updates"
feisty_security 	= Dist "feisty-security"
feisty_all 		= [feisty, feisty_backports, feisty_updates, feisty_security]

-- *** Gutsy
gutsy 			= Dist "gutsy"
gutsy_backports 	= Dist "gutsy-backports"
gutsy_updates 		= Dist "gutsy-updates"
gutsy_security 		= Dist "gutsy-security"
gutsy_all 		= [gutsy, gutsy_backports, gutsy_updates, gutsy_security]

gutsy_cnb		= Dist "cnb-gutsy"
gutsy_extra		= Dist "gutsy-extra"
gutsy_extra_proposed	= Dist "gutsy-extra-proposed"

-- ** Freespire/Linspire Dists

-- *** Skipjack Feisty
skipjack_feisty		= Dist "skipjack-feisty"
ms_feisty		= Dist "ms-feisty"
cnb_feisty		= Dist "cnb-feisty"
feisty_extra		= Dist "feisty-extra"
feisty_extra_proposed	= Dist"feisty-extra-proposed"

-- * SourceSpecs

-- ** Mirrors

ubuntu arches dists = SourceSpec Frozen (ubuntuRepo, arches, dists) "ubuntu"

-- ** Addons

-- *** Freespire

freespireCnrUbuntuExtra arches dists = SourceSpec Active (Repository "/srv/addons/freespire/CNRUbuntuExtra", arches, dists) "CNRUbuntuExtra"
freespireCnrUbuntu      arches dists = SourceSpec Active (Repository "/srv/addons/freespire/CNRUbuntu",      arches, dists) "CNRUbuntu"
freespireMsUbuntu       arches dists = SourceSpec Active (Repository "/srv/addons/freespire/MSUbuntu",       arches, dists) "MSUbuntu"
freespireCnbUbuntu      arches dists = SourceSpec Active (Repository "/srv/addons/freespire/CNBUbuntu",      arches, dists) "CNBUbuntu"

-- *** Ubuntu

ubuntuCnrUbuntuExtra arches dists = SourceSpec Active (Repository "/srv/addons/ubuntu/CNRUbuntuExtra", arches, dists) "CNRUbuntuExtra"
-- ubuntuCnrUbuntu      arches dists = SourceSpec Active (Repository "/srv/addons/ubuntu/CNRUbuntu",      arches, dists) "CNRUbuntu"
-- ubuntuMsUbuntu       arches dists = SourceSpec Active (Repository "/srv/addons/ubuntu/MSUbuntu",       arches, dists) "MSUbuntu"
ubuntuCnbUbuntu      arches dists = SourceSpec Active (Repository "/srv/addons/ubuntu/CNBUbuntu",      arches, dists) "CNBUbuntu"

-- * Targets

dateAndTime :: String
dateAndTime = "%Y-%m-%d_%H:%M:%S"

skipjackFeistyTarget :: Target
skipjackFeistyTarget =
    Target { targetName = "skipjack-feisty"
           , basePath = "/srv/dists/freespire"
           , dateFormat = dateAndTime
           , sourceSpecs =
               [ ubuntu			 arches [feisty, feisty_backports, feisty_updates, feisty_security]
               , freespireCnrUbuntuExtra arches [feisty_extra, feisty_extra_proposed]
               , freespireCnrUbuntu      arches [skipjack_feisty]
               , freespireMsUbuntu       arches [ms_feisty]
               , freespireCnbUbuntu	 arches [cnb_feisty]
               ]
           }
    where
      arches = [I386]

ubuntuFeistyTarget :: Target
ubuntuFeistyTarget =
    Target { targetName = "ubuntu-feisty"
           , basePath = "/srv/dists/ubuntu"
           , dateFormat = dateAndTime
           , sourceSpecs =
               [ ubuntu         	arches [feisty, feisty_backports, feisty_updates, feisty_security]
               , ubuntuCnrUbuntuExtra	arches [feisty_extra, feisty_extra_proposed]
               , ubuntuCnbUbuntu	arches [cnb_feisty] 
               ]
           }
    where
      arches = [I386]

ubuntuGutsyTarget :: Target
ubuntuGutsyTarget =
    Target { targetName = "ubuntu-gutsy"
           , basePath = "/srv/dists/ubuntu"
           , dateFormat = dateAndTime
           , sourceSpecs =
               [ ubuntu         	arches [gutsy, gutsy_backports, gutsy_updates, gutsy_security]
               , ubuntuCnrUbuntuExtra	arches [gutsy_extra, gutsy_extra_proposed]
               , ubuntuCnbUbuntu	arches [gutsy_cnb] 
               ]
           }
    where
      arches = [I386]
