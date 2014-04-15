-- Copyright (C) 2014 Sebastian Wiesner <lunaryorn@gmail.com>

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

import Control.Monad (liftM,filterM,(>=>))
import Data.Maybe (listToMaybe)
import Distribution.Verbosity (silent)
import Distribution.Simple.BuildPaths (defaultDistPref)
import Distribution.PackageDescription (executables,exeName,library)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>),dropFileName)

getBuildDirectories :: FilePath -> IO [String]
getBuildDirectories cabalFile = do
  desc <- liftM flattenPackageDescription (readPackageDescription silent cabalFile)
  let buildDirs = autogenDir : map executableBuildDir (executables desc)
  filterM doesDirectoryExist $ case library desc of
    Just _ -> buildDir : buildDirs
    Nothing -> buildDirs
  where distDir = dropFileName cabalFile </> defaultDistPref
        buildDir = distDir </> "build"
        autogenDir = buildDir </> "autogen"
        executableBuildDir executable = buildDir </>
                                        exeName executable </>
                                        (exeName executable ++ "-tmp")

main :: IO ()
main = getArgs >>=
       maybe exitFailure (getBuildDirectories >=> mapM_ putStrLn) . listToMaybe
