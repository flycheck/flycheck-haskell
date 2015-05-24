-- Copyright (C) Michael Alan Dorman <mdorman@ironicdesign.com>

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

import Data.Version (Version (Version))
import Distribution.Simple.Utils (cabalVersion)

legacyFlags :: [String]
legacyFlags = ["-DUSE_COMPILER_ID"]

isLegacyCabal :: Bool
isLegacyCabal = cabalVersion < Version [1,22] []

main :: IO ()
main = mapM_ putStrLn flags
  where flags =
          if isLegacyCabal
             then legacyFlags
             else []

-- Local Variables:
-- hindent-style: "chris-done"
-- End:
