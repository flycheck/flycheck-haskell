import Data.Version (Version (Version))
import Distribution.Simple.Utils (cabalVersion)

main :: IO ()
main =
  putStrLn $ if cabalVersion >= Version [1,22] []
             then "(\"-DuseCompilerInfo\")"
             else "()"
