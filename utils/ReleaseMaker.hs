module Main where

import Dist
import DistMain


main = distMain [target]

repo = Repository "/root/tmp/MSUbuntu/MSUbuntu-current/MSUbuntu"
arches = [I386,AMD64]
dists = [Dist "ms-feisty"]
dest = "/root/tmp/ms-feisty-today"
sourceSpecs' = 
     [ SourceSpec (repo, arches, dists) "msubuntu1" 
     , SourceSpec (repo, arches, dists) "msubuntu2"
     ]

target :: Target
target = 
    Target { targetName = "msubuntu"
           , basePath = "/root/tmp/msubuntu"
           , dateFormat = "%Y-%m-%d"
           , sourceSpecs =      [ SourceSpec (repo, arches, dists) "msubuntu1" 
                                , SourceSpec (repo, arches, dists) "msubuntu2"
                                ]
           }

