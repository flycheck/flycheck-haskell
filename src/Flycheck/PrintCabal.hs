-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flycheck.PrintCabal where

import qualified Data.AttoLisp as L
import qualified Data.ByteString.Lazy as BS
import Distribution.Verbosity (silent)
import Distribution.PackageDescription (PackageDescription
                                       ,allBuildInfo
                                       ,hsSourceDirs)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Environment (getArgs)
import System.Exit (exitFailure)


instance L.ToLisp PackageDescription where
    toLisp pkgdesc = L.mkStruct "package-description"
                     [ field "version" (1 :: Int)
                     , field "source-directories"  (collectSourceDirs pkgdesc)]
        where field n v = L.DotList [L.Symbol n] (L.toLisp v)


collectSourceDirs :: PackageDescription -> [FilePath]
collectSourceDirs = concatMap hsSourceDirs . allBuildInfo

printToLisp :: L.ToLisp a => a -> IO ()
printToLisp = BS.putStr.L.encode


dumpCabalFile :: FilePath -> IO ()
dumpCabalFile filename = do
  genericDesc <- readPackageDescription silent filename
  printToLisp (flattenPackageDescription genericDesc)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> exitFailure
    filename : _ -> dumpCabalFile filename
