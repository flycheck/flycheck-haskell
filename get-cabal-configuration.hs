-- Copyright (C) 2014 Sebastian Wiesner <lunaryorn@gmail.com>
-- Copyright (C) 2014 Gracjan Polak <gracjanpolak@gmail.com>

-- This file is not part of GNU Emacs.

-- This program is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.

-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.

-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad (liftM)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Distribution.Package (PackageName(..),Dependency(..))
import Distribution.PackageDescription (PackageDescription(..),allBuildInfo
                                       ,BuildInfo(..)
                                       ,usedExtensions,allLanguages
                                       ,exeName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.BuildPaths (defaultDistPref)
import Distribution.Verbosity (silent)
import Language.Haskell.Extension (Extension(..),Language(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>),dropFileName,normalise)

data Sexp = SList [Sexp]
          | SString String
          | SSymbol String

sym :: String -> Sexp
sym = SSymbol

instance Show Sexp where
  show (SSymbol s) = s
  show (SString s) = show s     -- Poor man's escaping
  show (SList s) = "(" ++ unwords (map show s) ++ ")"

class ToSexp a where
  toSexp :: a -> Sexp

instance ToSexp String where
  toSexp = SString

instance ToSexp Extension where
  toSexp (EnableExtension ext) = toSexp (show ext)
  toSexp (DisableExtension ext) = toSexp ("No" ++ show ext)
  toSexp (UnknownExtension ext) = toSexp ext

instance ToSexp Language where
  toSexp (UnknownLanguage lang) = toSexp lang
  toSexp lang = toSexp (show lang)

instance ToSexp Dependency where
  toSexp (Dependency (PackageName dependency) _) = toSexp dependency

instance ToSexp Sexp where
  toSexp = id

cons :: (ToSexp a, ToSexp b) => a -> [b] -> Sexp
cons h t = SList (toSexp h : map toSexp t)

getBuildDirectories :: PackageDescription -> FilePath -> [String]
getBuildDirectories pkgDesc cabalDir = case library pkgDesc of
                                         Just _ -> buildDir : buildDirs
                                         Nothing -> buildDirs
  where distDir = cabalDir </> defaultDistPref
        buildDir = distDir </> "build"
        autogenDir = buildDir </> "autogen"
        executableBuildDir e = buildDir </> exeName e </> (exeName e ++ "-tmp")
        buildDirs = autogenDir : map executableBuildDir (executables pkgDesc)

getSourceDirectories :: [BuildInfo] -> FilePath -> [String]
getSourceDirectories buildInfo cabalDir =
  map (cabalDir </>) (concatMap hsSourceDirs buildInfo)

dumpPackageDescription :: PackageDescription -> FilePath -> Sexp
dumpPackageDescription pkgDesc cabalFile = SList [
                         cons (sym "build-directories") buildDirs
                       , cons (sym "source-directories") sourceDirs
                       , cons (sym "extensions") exts
                       , cons (sym "languages") langs
                       , cons (sym "dependencies") deps
                       ]
  where cabalDir = dropFileName cabalFile
        buildInfo = allBuildInfo pkgDesc
        buildDirs = map normalise (getBuildDirectories pkgDesc cabalDir)
        sourceDirs = map normalise (getSourceDirectories buildInfo cabalDir)
        exts = nub (concatMap usedExtensions buildInfo)
        langs = nub (concatMap allLanguages buildInfo)
        deps = nub (buildDepends pkgDesc)

dumpCabalConfiguration :: String -> IO ()
dumpCabalConfiguration cabalFile = do
  pkgDesc <- liftM flattenPackageDescription
                   (readPackageDescription silent cabalFile)
  print (dumpPackageDescription pkgDesc cabalFile)

main :: IO ()
main = do
  args <- getArgs
  let cabalFile = listToMaybe args
  maybe exitFailure dumpCabalConfiguration cabalFile
