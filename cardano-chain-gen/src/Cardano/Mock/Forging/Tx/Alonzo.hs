{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Tx.Alonzo (
  consTxBody,
  addValidityInterval,
  consPaymentTxBody,
  consCertTxBody,
  mkPaymentTx,
  mkPaymentTx',
  mkLockByScriptTx,
  mkUnlockScriptTx,
  mkScriptInp,
  mkScriptMint,
  mkScriptMint',
  mkMAssetsScriptTx,
  mkDCertTx,
  mkSimpleDCertTx,
  mkDCertPoolTx,
  mkScriptDCertTx,
  mkDepositTxPools,
  mkDCertTxPools,
  mkSimpleTx,
  consPoolParams,
  consPoolParamsTwoOwners,
  mkScriptTx,
  mkWitnesses,
  mkUTxOAlonzo,
  emptyTxBody,
  emptyTx,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude hiding (sum, (.))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe.Strict as Strict
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (AlonzoEra, LedgerState)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Prelude hiding (map)

type AlonzoUTxOIndex = UTxOIndex AlonzoEra

type AlonzoLedgerState = LedgerState (ShelleyBlock TPraosStandard AlonzoEra)

consTxBody ::
  Set TxIn ->
  Set TxIn ->
  StrictSeq (AlonzoTxOut AlonzoEra) ->
  Coin ->
  MultiAsset ->
  [ShelleyTxCert AlonzoEra] ->
  Withdrawals ->
  AlonzoTxBody AlonzoEra
consTxBody ins cols outs fees minted certs wdrl =
  AlonzoTxBody
    ins
    cols
    outs
    (StrictSeq.fromList certs)
    wdrl
    fees
    (ValidityInterval Strict.SNothing Strict.SNothing)
    Strict.SNothing
    mempty
    minted
    Strict.SNothing
    Strict.SNothing
    (Strict.SJust Testnet)

addValidityInterval ::
  AlonzoEraTxBody era =>
  SlotNo ->
  AlonzoTx era ->
  AlonzoTx era
addValidityInterval slotNo tx =
  tx {body = txBody'}
  where
    interval = ValidityInterval Strict.SNothing (Strict.SJust slotNo)
    txBody' = set vldtTxBodyL interval (body tx)

consPaymentTxBody ::
  Set TxIn ->
  Set TxIn ->
  StrictSeq (AlonzoTxOut AlonzoEra) ->
  Coin ->
  MultiAsset ->
  AlonzoTxBody AlonzoEra
consPaymentTxBody ins cols outs fees minted = consTxBody ins cols outs fees minted mempty (Withdrawals mempty)

consCertTxBody :: [ShelleyTxCert AlonzoEra] -> Withdrawals -> AlonzoTxBody AlonzoEra
consCertTxBody = consTxBody mempty mempty mempty (Coin 0) mempty

mkPaymentTx ::
  AlonzoUTxOIndex ->
  AlonzoUTxOIndex ->
  Integer ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkPaymentTx inputIndex outputIndex amount fees sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta
  addr <- resolveAddress outputIndex sta

  let input = Set.singleton $ fst inputPair
      output = AlonzoTxOut addr (valueFromList (Coin amount) []) Strict.SNothing
      AlonzoTxOut addr' (MaryValue inputValue _) _ = snd inputPair
      change = AlonzoTxOut addr' (valueFromList (Coin $ unCoin inputValue - amount - fees) []) Strict.SNothing
  Right $ mkSimpleTx True $ consPaymentTxBody input mempty (StrictSeq.fromList [output, change]) (Coin fees) mempty

mkPaymentTx' ::
  AlonzoUTxOIndex ->
  [(AlonzoUTxOIndex, MaryValue)] ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkPaymentTx' inputIndex outputIndex sta = do
  inputPair <- fst <$> resolveUTxOIndex inputIndex sta
  outps <- mapM mkOuts outputIndex

  let inps = Set.singleton $ fst inputPair
      AlonzoTxOut addr' (MaryValue inputValue _) _ = snd inputPair
      outValue = sum ((\(MaryValue vl _) -> unCoin vl) . snd <$> outputIndex)
      change = AlonzoTxOut addr' (valueFromList (Coin (unCoin inputValue - outValue)) []) Strict.SNothing
  Right $ mkSimpleTx True $ consPaymentTxBody inps mempty (StrictSeq.fromList $ outps ++ [change]) (Coin 0) mempty
  where
    mkOuts (outIx, vl) = do
      addr <- resolveAddress outIx sta
      Right $ AlonzoTxOut addr vl Strict.SNothing

mkLockByScriptTx ::
  AlonzoUTxOIndex ->
  [Bool] ->
  Integer ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkLockByScriptTx inputIndex spendable amount fees sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta

  let input = Set.singleton $ fst inputPair
      outs = mkOut <$> spendable
      AlonzoTxOut addr' (MaryValue inputValue _) _ = snd inputPair
      change = AlonzoTxOut addr' (valueFromList (Coin (unCoin inputValue - amount - fees)) []) Strict.SNothing
  -- No witnesses are necessary when the outputs is a script address. Only when it's consumed.
  Right $ mkSimpleTx True $ consPaymentTxBody input mempty (StrictSeq.fromList $ outs <> [change]) (Coin fees) mempty
  where
    datahash = hashData @AlonzoEra plutusDataList
    mkOut sp =
      let outAddress = if sp then alwaysSucceedsScriptAddr else alwaysFailsScriptAddr
       in AlonzoTxOut outAddress (valueFromList (Coin amount) []) (Strict.SJust datahash)

mkUnlockScriptTx ::
  [AlonzoUTxOIndex] ->
  AlonzoUTxOIndex ->
  AlonzoUTxOIndex ->
  Bool ->
  Integer ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkUnlockScriptTx inputIndex colInputIndex outputIndex succeeds amount fees sta = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
  (colInputPair, _) <- resolveUTxOIndex colInputIndex sta
  addr <- resolveAddress outputIndex sta

  let inpts = Set.fromList $ fst <$> inputPairs
      colInput = Set.singleton $ fst colInputPair
      output = AlonzoTxOut addr (valueFromList (Coin amount) []) Strict.SNothing
  Right
    $ mkScriptTx
      succeeds
      (mapMaybe mkScriptInp' $ zip [0 ..] inputPairs)
    $ consPaymentTxBody inpts colInput (StrictSeq.fromList [output]) (Coin fees) mempty

mkScriptInp' ::
  (Word64, (TxIn, Core.TxOut AlonzoEra)) ->
  Maybe (AlonzoPlutusPurpose AsIx era, Maybe (ScriptHash, Core.Script AlonzoEra))
mkScriptInp' = map (second Just) . mkScriptInp

mkScriptInp ::
  (Word64, (TxIn, Core.TxOut AlonzoEra)) ->
  Maybe (AlonzoPlutusPurpose AsIx era, (ScriptHash, Core.Script AlonzoEra))
mkScriptInp (n, (_txIn, txOut))
  | addr == alwaysFailsScriptAddr =
      Just
        (AlonzoSpending (AsIx $ fromIntegral n), (alwaysFailsScriptHash, alwaysFailsScript))
  | addr == alwaysSucceedsScriptAddr =
      Just
        (AlonzoSpending (AsIx $ fromIntegral n), (alwaysSucceedsScriptHash, alwaysSucceedsScript))
  | addr == alwaysMintScriptAddr =
      Just (AlonzoSpending (AsIx $ fromIntegral n), (alwaysMintScriptHash, alwaysMintScript))
  | otherwise = Nothing
  where
    addr = txOut ^. Core.addrTxOutL

mkScriptMint' ::
  AlonzoEraScript era =>
  MultiAsset ->
  [(AlonzoPlutusPurpose AsIx era, Maybe (ScriptHash, Script era))]
mkScriptMint' = fmap (first $ AlonzoMinting . AsIx) . mkScriptMint

mkScriptMint ::
  AlonzoEraScript era =>
  MultiAsset ->
  [(Word32, Maybe (ScriptHash, Script era))]
mkScriptMint (MultiAsset mp) = mapMaybe f $ zip [0 ..] (Map.keys mp)
  where
    f (n, policyId)
      | policyID policyId == alwaysFailsScriptHash =
          Just (n, Just (alwaysFailsScriptHash, alwaysFailsScript))
      | policyID policyId == alwaysSucceedsScriptHash =
          Just
            (n, Just (alwaysSucceedsScriptHash, alwaysSucceedsScript))
      | policyID policyId == alwaysMintScriptHash =
          Just (n, Just (alwaysMintScriptHash, alwaysMintScript))
      | otherwise = Nothing

mkMAssetsScriptTx ::
  [AlonzoUTxOIndex] ->
  AlonzoUTxOIndex ->
  [(AlonzoUTxOIndex, MaryValue)] ->
  MultiAsset ->
  Bool ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkMAssetsScriptTx inputIndex colInputIndex outputIndex minted succeeds fees sta = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
  colInput <- Set.singleton . fst . fst <$> resolveUTxOIndex colInputIndex sta
  outps <- mapM mkOuts outputIndex
  let inpts = Set.fromList $ fst <$> inputPairs

  Right
    $ mkScriptTx
      succeeds
      ( mapMaybe mkScriptInp' (zip [0 ..] inputPairs)
          ++ mkScriptMint' minted
      )
    $ consPaymentTxBody inpts colInput (StrictSeq.fromList outps) (Coin fees) minted
  where
    mkOuts (outIx, vl) = do
      addr <- resolveAddress outIx sta
      Right $ AlonzoTxOut addr vl (Strict.SJust (hashData @AlonzoEra plutusDataList))

mkDCertTx ::
  [ShelleyTxCert AlonzoEra] ->
  Withdrawals ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkDCertTx certs wdrl = Right $ mkSimpleTx True $ consCertTxBody certs wdrl

mkSimpleDCertTx ::
  [(StakeIndex, StakeCredential -> ShelleyTxCert AlonzoEra)] ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkSimpleDCertTx consDert st = do
  dcerts <- forM consDert $ \(stakeIndex, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure $ mkDCert cred
  mkDCertTx dcerts (Withdrawals mempty)

mkDCertPoolTx ::
  [ ( [StakeIndex]
    , PoolIndex
    , [StakeCredential] -> KeyHash 'StakePool -> ShelleyTxCert AlonzoEra
    )
  ] ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkDCertPoolTx consDert st = do
  dcerts <- forM consDert $ \(stakeIxs, poolIx, mkDCert) -> do
    stakeCreds <- forM stakeIxs $ \stix -> resolveStakeCreds stix st
    let poolId = resolvePool poolIx st
    pure $ mkDCert stakeCreds poolId
  mkDCertTx dcerts (Withdrawals mempty)

mkScriptDCertTx ::
  [(StakeIndex, Bool, StakeCredential -> ShelleyTxCert AlonzoEra)] ->
  Bool ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkScriptDCertTx consDert valid st = do
  dcerts <- forM consDert $ \(stakeIndex, _, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure $ mkDCert cred
  Right $
    mkScriptTx valid (mapMaybe (map (second Just) . prepareRedeemer) $ zip [0 ..] consDert) $
      consCertTxBody dcerts (Withdrawals mempty)
  where
    prepareRedeemer (n, (StakeIndexScript bl, addRedeemer, _)) =
      if not addRedeemer
        then Nothing
        else
          Just $
            if bl
              then (AlonzoCertifying (AsIx n), (alwaysFailsScriptHash, alwaysFailsScript))
              else (AlonzoCertifying (AsIx n), (alwaysSucceedsScriptHash, alwaysSucceedsScript))
    prepareRedeemer _ = Nothing

mkDepositTxPools ::
  AlonzoUTxOIndex ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkDepositTxPools inputIndex deposit sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta

  let input = Set.singleton $ fst inputPair
      AlonzoTxOut addr' (MaryValue inputValue _) _ = snd inputPair
      change = AlonzoTxOut addr' (valueFromList (Coin (unCoin inputValue - deposit)) []) Strict.SNothing
  Right $ mkSimpleTx True $ consTxBody input mempty (StrictSeq.fromList [change]) (Coin 0) mempty (allPoolStakeCert sta) (Withdrawals mempty)

mkDCertTxPools ::
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx AlonzoEra)
mkDCertTxPools sta = Right $ mkSimpleTx True $ consCertTxBody (allPoolStakeCert sta) (Withdrawals mempty)

mkSimpleTx :: Bool -> AlonzoTxBody AlonzoEra -> AlonzoTx AlonzoEra
mkSimpleTx valid txBody =
  AlonzoTx
    { body = txBody
    , wits = mempty
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

consPoolParamsTwoOwners ::
  [StakeCredential] ->
  KeyHash 'StakePool ->
  ShelleyTxCert AlonzoEra
consPoolParamsTwoOwners [rwCred, KeyHashObj owner0, KeyHashObj owner1] poolId =
  ShelleyTxCertPool $ RegPool $ consPoolParams poolId rwCred [owner0, owner1]
consPoolParamsTwoOwners _ _ = panic "expected 2 pool owners"

mkScriptTx ::
  forall era.
  ( Core.Script era ~ AlonzoScript era
  , Core.TxWits era ~ AlonzoTxWits era
  , AlonzoEraScript era
  ) =>
  Bool ->
  [(PlutusPurpose AsIx era, Maybe (ScriptHash, Core.Script era))] ->
  Core.TxBody era ->
  AlonzoTx era
mkScriptTx valid rdmrs txBody =
  AlonzoTx
    { body = txBody
    , wits = witnesses
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
  where
    witnesses =
      mkWitnesses
        rdmrs
        [(hashData @era plutusDataList, plutusDataList)]

mkWitnesses ::
  ( Script era ~ AlonzoScript era
  , AlonzoEraScript era
  ) =>
  [(PlutusPurpose AsIx era, Maybe (ScriptHash, Core.Script era))] ->
  [(DataHash, Data era)] ->
  AlonzoTxWits era
mkWitnesses rdmrs datas =
  AlonzoTxWits
    mempty
    mempty
    (Map.fromList $ mapMaybe snd rdmrs)
    (TxDats $ Map.fromList datas)
    (Redeemers $ Map.fromList redeemers)
  where
    redeemers =
      fmap
        (,(plutusDataList, ExUnits 100 100))
        (fst <$> rdmrs)

mkUTxOAlonzo ::
  (Core.EraTx era, Core.Tx era ~ AlonzoTx era) =>
  AlonzoTx era ->
  [(TxIn, Core.TxOut era)]
mkUTxOAlonzo tx =
  [ (TxIn transId idx, out)
  | (out, idx) <- zip (toList (tx ^. outputsL)) (TxIx <$> [0 ..])
  ]
  where
    transId = txIdTx tx
    outputsL = Core.bodyTxL . Core.outputsTxBodyL

emptyTxBody :: AlonzoTxBody AlonzoEra
emptyTxBody =
  AlonzoTxBody
    mempty
    mempty
    mempty
    mempty
    (Withdrawals mempty)
    (Coin 0)
    (ValidityInterval Strict.SNothing Strict.SNothing)
    Strict.SNothing
    mempty
    mempty
    Strict.SNothing
    Strict.SNothing
    (Strict.SJust Testnet)

emptyTx :: AlonzoTx AlonzoEra
emptyTx =
  AlonzoTx
    { body = emptyTxBody
    , wits = mempty
    , isValid = IsValid True
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
