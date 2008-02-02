#!/usr/bin/runhaskell
module Main where

import Debian.Mirror.Dist
import Debian.Mirror.DistMain

main = distMain targets

targets :: [Target]
targets = 
    [ skipjackFeistyTarget
    ]

-- * Repositories

ubuntuRepo		= Repository "/srv/mirrors/ubuntu-archives/current"
cnrUbuntuExtraRepo 	= Repository "/srv/mirrors/CNRUbuntuExtra"
cnrUbuntuRepo 		= Repository "/srv/mirrors/CNRUbuntu"
cnbUbuntuRepo 		= Repository "/srv/mirrors/CNBUbuntu"
msUbuntuRepo 		= Repository "/srv/mirrors/MSUbuntu"

-- * Dists

-- ** Ubuntu Dists
feisty 			= Dist "feisty"
feisty_backports 	= Dist "feisty-backports"
feisty_updates 		= Dist "feisty-updates"
feisty_security 	= Dist "feisty-security"
feisty_all 		= [feisty, feisty_backports, feisty_updates, feisty_security]

-- ** Freespire/Linspire Dists

skipjack_feisty		= Dist "skipjack-feisty"
ms_feisty		= Dist "ms-feisty"
cnb_feisty		= Dist "cnb-feisty"
feisty_extra		= Dist "feisty-extra"
feisty_extra_proposed	= Dist"feisty-extra-proposed"

-- * SourceSpecs


ubuntu arches dists         = SourceSpec Frozen (ubuntuRepo        , arches, dists) "ubuntu"
cnrUbuntuExtra arches dists = SourceSpec Active (cnrUbuntuExtraRepo, arches, dists) "CNRUbuntuExtra"
cnrUbuntu arches dists      = SourceSpec Active (cnrUbuntuRepo     , arches, dists) "CNRUbuntu"
msUbuntu arches dists       = SourceSpec Active (msUbuntuRepo      , arches, dists) "MSUbuntu"
cnbUbuntu arches dists      = SourceSpec Active (cnbUbuntuRepo     , arches, dists) "CNBUbuntu"

-- * Targets

skipjackFeistyTarget :: Target
skipjackFeistyTarget =
    Target { targetName = "skipjack-feisty"
           , basePath = "/srv/dists/freespire"
           , dateFormat = "%Y-%m-%d_%H:%M:%S"
           , sourceSpecs =
               [ ubuntu         arches [feisty, feisty_backports, feisty_updates, feisty_security]
               , cnrUbuntuExtra arches [feisty_extra, feisty_extra_proposed]
               , cnrUbuntu      arches [skipjack_feisty]
               , msUbuntu       arches [ms_feisty]
               , cnbUbuntu	arches [cnb_feisty]
               ]
           }
    where
      arches = [I386]
