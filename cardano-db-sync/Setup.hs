module Main where

import           Cardano.Crypto.Hash
import           Control.Monad (forM)

import qualified Data.ByteString.Char8 as BS

import           Data.List (intercalate)

import           Distribution.PackageDescription (extraSrcFiles)
import           Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import           Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import           Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFileEx)
import           Distribution.Verbosity (normal)

import           System.FilePath (takeExtension)

main :: IO ()
main = defaultMainWithHooks generateHooks
  where
    generateHooks :: UserHooks
    generateHooks =
      simple { buildHook   = \p l h f -> generate l >> buildHook simple p l h f
             , haddockHook = \p l h f -> generate l >> haddockHook simple p l h f
             , replHook    = \p l h f args -> generate l >> replHook simple p l h f args
             }

    simple = simpleUserHooks

    generate :: LocalBuildInfo -> IO ()
    generate locInfo =
      generateMigrations locInfo "schema" (autogenPackageModulesDir locInfo)

generateMigrations :: LocalBuildInfo -> FilePath -> FilePath -> IO ()
generateMigrations locInfo srcDir outDir = do
    createDirectoryIfMissingVerbose normal True "gen"
    sqls <- forM (collectMigrationSql srcDir) build
    buildMigrationModule sqls
  where

    collectMigrationSql :: FilePath -> [FilePath]
    collectMigrationSql path =
      filter ((== ".sql") . takeExtension) (extraSrcFiles $ localPkgDescr locInfo)

    hashAs :: ByteString -> Hash Blake2b_256 ByteString
    hashAs = hashWith id

    build :: FilePath -> IO (String, FilePath)
    build filepath = do
      file <- BS.readFile filepath
      pure (hashToStringAsHex . hashAs $ file, filepath)


    buildMigrationModule :: [(String, FilePath)] -> IO ()
    buildMigrationModule sqls =
      let buildLine (hashedFile, filepath) = "    KnownMigration \"" ++ hashedFile ++ "\" \"" ++ filepath ++ "\"" in

      rewriteFileEx normal "gen/MigrationValidations.hs" $
        unlines
          [ "{-# LANGUAGE OverloadedStrings #-}",
            "module MigrationValidations where",
            "",
            "import Prelude",
            "import Data.Text",
            "data KnownMigration = KnownMigration { md5 :: Text, filepath :: Text } deriving (Eq, Show)",
            "",
            "knownMigrations :: [KnownMigration]",
            "knownMigrations = [ ",
            (intercalate ",\n" . fmap buildLine $ sqls) ++ "]"
          ]




