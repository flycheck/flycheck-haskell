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
import Distribution.Verbosity (silent)
import Distribution.PackageDescription (allBuildInfo,allExtensions)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Language.Haskell.Extension (Extension(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)

showExtension :: Extension -> String
showExtension (EnableExtension ext) = show ext
showExtension (DisableExtension ext) = "No" ++ show ext
showExtension (UnknownExtension ext) = ext

getExtensions :: FilePath -> IO [FilePath]
getExtensions cabalFile = do
  desc <- liftM flattenPackageDescription (readPackageDescription silent cabalFile)
  return (map showExtension (concatMap allExtensions (allBuildInfo desc)))

main :: IO ()
main = getArgs >>=
       maybe exitFailure (getExtensions >=> mapM_ putStrLn) . listToMaybe
