{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_chapter16 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "chapter16"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = ""
copyright :: String
copyright = ""
homepage :: String
homepage = ""
