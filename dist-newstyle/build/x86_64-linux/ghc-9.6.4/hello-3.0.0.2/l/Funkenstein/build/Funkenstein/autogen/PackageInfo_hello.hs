{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_hello (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "hello"
version :: Version
version = Version [3,0,0,2] []

synopsis :: String
synopsis = "Hello World, an example package"
copyright :: String
copyright = "(c) Simon Marlow"
homepage :: String
homepage = "http://www.haskell.org/hello/"
