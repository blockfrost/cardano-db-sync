{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-missing-signatures #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text

import qualified Cardano.Binary as CBOR
import qualified Cardano.Api.Shelley as Api
import qualified Plutus.V1.Ledger.Api as Plutus
import           Codec.Serialise.Class (Serialise (..))
import Cardano.Db

import Database.Esqueleto.Experimental

-- Due to pending api fix
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import qualified Data.Scientific as Scientific
import qualified Data.Text.Encoding as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

main :: IO ()
main = do
  putStrLn "Yolo"
  todoLen <- runDbStdoutLogging $ do
    maybe (0 :: Int) unValue . listToMaybe <$>
      (select $ do
        datum <- from (table @Datum)
        where_ (isNothing (datum ^. DatumBytes))
        pure countRows
      )

  liftIO
    $ putStrLn
    $ "Found " ++ show todoLen ++ " datums to backfill"

  backfillAll 0 todoLen
  putStrLn "All done"

backfillAll :: Int -> Int -> IO ()
backfillAll n total = do
  backfillSome >>= \case
    0 -> pure ()
    x -> do
      liftIO $ putStrLn $ "- " ++ show (n + x) ++ "/" ++ show total
      liftIO $ threadDelay 1000000
      backfillAll (n + x) total

backfillSome :: IO Int
backfillSome = do
  --runDbStdoutLogging $ do
  runDbNoLogging $ do
    dats <- select $ do
      datum <- from (table @Datum)
      where_ (isNothing (datum ^. DatumBytes))
      limit 10000
      pure datum

    forM_ dats $ \d -> do
      case decodeData <$> datumValue (entityVal d) of
        Just (Right dat) -> do
          update $ \p -> do
            let bytes = Api.serialiseToCBOR dat
            set p [ DatumBytes =. just (val bytes)  ]
            where_ $ (p ^. DatumId) ==. val (entityKey d)

        Just (Left err) -> error
          $ "Decoding failed " ++ show err ++ " input was " ++ show (datumValue (entityVal d))
        Nothing -> error "absurd"

    pure (length dats)


-- check that previous datum CBOR bytes = new computed from JSON
checkRoundtrip :: IO ()
checkRoundtrip = do
  runDbStdoutLogging $ do
    dats <- select $ do
      datum <- from (table @Datum)
      where_ (not_ $ isNothing (datum ^. DatumBytes))
      pure datum

    forM_ dats $ \d -> do
      let
          Just olddat = datumBytes (entityVal d)

      liftIO $ case decodeData <$> datumValue (entityVal d) of
        Just (Right dat) -> do
          unless (Api.serialiseToCBOR dat == olddat) $ do
            error $ "No match" ++ show dat
        Just (Left err) -> print ("Errord" :: String, err)
        Nothing -> error "absurd"

decodeData :: Text -> Either Api.ScriptDataJsonSchemaError Api.ScriptData
decodeData x =
      scriptDataFromJsonDetailedSchema
    $ fromMaybe (error "absurd aeson decode")
    $ Aeson.decode @Aeson.Value
    $ LBS.fromStrict
    $ BS.pack
    $ Data.Text.unpack x

encodeData dt = LBS.toStrict $ Aeson.encode $
  Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema $ Api.fromAlonzoData dt

instance Api.SerialiseAsCBOR Api.ScriptData where
  serialiseToCBOR = CBOR.serialize'

instance CBOR.ToCBOR Api.ScriptData where
  toCBOR = encode @Plutus.Data . Api.toPlutusData

instance CBOR.FromCBOR Api.ScriptData where
  fromCBOR = Api.fromPlutusData <$> decode @Plutus.Data

-- ouch
-- due to pending api fix
scriptDataFromJsonDetailedSchema :: Aeson.Value
                                 -> Either Api.ScriptDataJsonSchemaError
                                           Api.ScriptData
scriptDataFromJsonDetailedSchema = conv
  where
    conv :: Aeson.Value
         -> Either Api.ScriptDataJsonSchemaError Api.ScriptData
    conv (Aeson.Object m) =
      case List.sort $ HashMap.toList m of
        [("int", Aeson.Number d)] ->
          case Scientific.floatingOrInteger d :: Either Double Integer of
            Left  n -> Left (Api.ScriptDataJsonNumberNotInteger n)
            Right n -> Right (Api.ScriptDataNumber n)

        [("bytes", Aeson.String s)]
          | Right bs <- Base16.decode (Text.encodeUtf8 s)
          -> Right (Api.ScriptDataBytes bs)

        [("list", Aeson.Array vs)] ->
            fmap Api.ScriptDataList
          . traverse conv
          $ Vector.toList vs

        [("map", Aeson.Array kvs)] ->
            fmap Api.ScriptDataMap
          . traverse convKeyValuePair
          $ Vector.toList kvs

        [("constructor", Aeson.Number d),
         ("fields",      Aeson.Array vs)] ->
          case Scientific.floatingOrInteger d :: Either Double Integer of
            Left  n -> Left (Api.ScriptDataJsonNumberNotInteger n)
            Right n -> fmap (Api.ScriptDataConstructor n)
                     . traverse conv
                     $ Vector.toList vs

        (key, v):_ | key `elem` ["int", "bytes", "list", "map", "constructor"] ->
            Left (Api.ScriptDataJsonTypeMismatch key v)

        kvs -> Left (Api.ScriptDataJsonBadObject kvs)

    conv v = Left (Api.ScriptDataJsonNotObject v)

    convKeyValuePair :: Aeson.Value
                     -> Either Api.ScriptDataJsonSchemaError
                               (Api.ScriptData, Api.ScriptData)
    convKeyValuePair (Aeson.Object m)
      | HashMap.size m == 2
      , Just k <- HashMap.lookup "k" m
      , Just v <- HashMap.lookup "v" m
      = (,) <$> conv k <*> conv v
    convKeyValuePair _ = error "absurd convKeyValuePair"
