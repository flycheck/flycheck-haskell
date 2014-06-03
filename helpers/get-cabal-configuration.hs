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

import Control.Monad (liftM,(>=>))
import Data.List (nub, intercalate)
import Data.Maybe (listToMaybe)
import Distribution.Verbosity (silent)
import Distribution.Simple.BuildPaths (defaultDistPref)
import Distribution.PackageDescription (allBuildInfo, PackageDescription,allBuildInfo,usedExtensions,allLanguages,hsSourceDirs,executables,exeName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Language.Haskell.Extension (Extension(..),Language(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>),dropFileName)

showLanguage :: Language -> String
showLanguage Haskell98 = "Haskell98"
showLanguage Haskell2010 = "Haskell2010"
showLanguage (UnknownLanguage lang) = lang

showExtension :: Extension -> String
showExtension (EnableExtension ext) = show ext
showExtension (DisableExtension ext) = "No" ++ show ext
showExtension (UnknownExtension ext) = ext

getLanguagesAndExtensions :: PackageDescription -> [String]
getLanguagesAndExtensions desc =
  let languages = map showLanguage (nub (concatMap allLanguages (allBuildInfo desc)))
      extensions = map showExtension (nub (concatMap usedExtensions (allBuildInfo desc)))
  in languages ++ extensions

getBuildDirectories :: FilePath -> PackageDescription -> [String]
getBuildDirectories cabalFile desc = buildDirs
  where buildDirs = autogenDir : map executableBuildDir (executables desc)
        distDir = dropFileName cabalFile </> defaultDistPref
        buildDir = distDir </> "build"
        autogenDir = buildDir </> "autogen"
        executableBuildDir executable = buildDir </>
                                        exeName executable </>
                                        (exeName executable ++ "-tmp")

getSourceDirectories :: FilePath -> PackageDescription -> [FilePath]
getSourceDirectories cabalFile desc =
  map (cabalDir </>) (concatMap hsSourceDirs (allBuildInfo desc))
  where cabalDir = dropFileName cabalFile

getCabalConfiguration :: FilePath -> IO [String]
getCabalConfiguration cabalFile = do
  desc <- liftM flattenPackageDescription (readPackageDescription silent cabalFile)
  let languagesAndExtensions = getLanguagesAndExtensions desc
  let buildDirectories = getBuildDirectories cabalFile desc
  let sourceDirectories = getSourceDirectories cabalFile desc
  return [ "((source-directories " ++ intercalate " " (map show sourceDirectories) ++ ")",
           " (language-extensions " ++ intercalate " " (map show languagesAndExtensions) ++ ")",
           " (build-directories " ++ intercalate " " (map show buildDirectories) ++ "))"]

main :: IO ()
main = getArgs >>=
       maybe exitFailure (getCabalConfiguration >=> mapM_ putStrLn) . listToMaybe
