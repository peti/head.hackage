#! /usr/bin/env runhaskell

import qualified Data.ByteString as BS
import Data.List
import Distribution.Nixpkgs.Hashes
import Distribution.Package
import Distribution.Text
import OpenSSL.Digest
import System.Directory
import System.Environment
import System.FilePath

generateOverrides :: FilePath -> IO String
generateOverrides patchDir = do
  patches <- listDirectory patchDir
  overrides <- mapM (generateOverride patchDir) patches
  return $ intercalate "\n" overrides

generateOverride :: FilePath -> FilePath -> IO String
generateOverride patchDir patch = do
  let pid' = simpleParse (takeBaseName patch)
  pid <- maybe (fail ("invalid patch file name: " ++ show patch)) return pid'
  sha256 <- fmap (digest (digestByName "sha256")) (BS.readFile (patchDir </> patch))
  let url = downloadUrl pid
      pname = display (packageName pid)
  return $ unlines
    [ unwords [ " ", pname, "=", "appendPatch", "super."++pname, "(pkgs.fetchurl {" ]
    , unwords [ "    name", "=", show patch ++ ";" ]
    , unwords [ "    url", "=", url ++ ";" ]
    , unwords [ "    sha256", "=", show (printSHA256 sha256) ++ ";" ]
    , "  });"
    ]

downloadUrl :: PackageId -> String
downloadUrl pid = "https://raw.githubusercontent.com/hvr/head.hackage/master/patches/" ++ display pid ++ ".patch"

main :: IO ()
main = do
  args <- getArgs
  patchDir <- case args of
                []    -> return "patches"
                [dir] -> return dir
                _     -> fail "Usage: generate-nix-overrides [patchdir]"
  overrides <- generateOverrides patchDir
  putStrLn "self: super: {\n"
  putStrLn overrides
  putStrLn "}"
