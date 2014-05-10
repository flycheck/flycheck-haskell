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
import Data.Maybe (listToMaybe)
import Data.List (group,head,sort)
import Distribution.Verbosity (silent)
import Distribution.PackageDescription (buildDepends)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Package (PackageName(..),Dependency(..))
import Language.Haskell.Extension (Extension(..),KnownExtension)
import System.Environment (getArgs)
import System.Exit (exitFailure)

showDependency :: Dependency -> String
showDependency (Dependency (PackageName dependency) _) = dependency

getDependencies :: FilePath -> IO [String]
getDependencies cabalFile = do
  desc <- liftM flattenPackageDescription (readPackageDescription silent cabalFile)
  return (map showDependency (buildDepends desc))

main :: IO ()
main = getArgs >>=
       maybe exitFailure (getDependencies >=> mapM_ putStrLn . map head . group . sort) . listToMaybe
