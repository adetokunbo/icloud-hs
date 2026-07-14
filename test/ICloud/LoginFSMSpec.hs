{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module ICloud.LoginFSMSpec
  ( spec
  )
where

import Network.ICloud.Internal.LoginFSM
import Test.Hspec (Spec, describe, it, shouldBe)


newtype TestState s = TestState ()


data Script = Script
  { scriptCreds :: !Bool
  , scriptDir :: !Bool
  , scriptMkDir :: !Bool
  , scriptHasSavedSession :: !Bool
  , scriptSessionValid :: !Bool
  , scriptSrpInvalidKey :: !Bool
  , scriptAcct :: !Bool
  , scriptTwoFa :: ![Bool]
  , scriptTwoSa :: ![Bool]
  , scriptNoTrustedDevices :: !Bool
  , scriptTwoFaLocked :: !Bool
  }


allTrue :: Script
allTrue =
  Script
    { scriptCreds = True
    , scriptDir = True
    , scriptMkDir = True
    , scriptHasSavedSession = False
    , scriptSessionValid = False
    , scriptSrpInvalidKey = False
    , scriptAcct = True
    , scriptTwoFa = [True]
    , scriptTwoSa = [True]
    , scriptNoTrustedDevices = False
    , scriptTwoFaLocked = False
    }


popTwoFa :: TestM Bool
popTwoFa = TestM $ \s -> case scriptTwoFa s of
  (b : bs) -> (b, s{scriptTwoFa = bs})
  [] -> (True, s)


popTwoSa :: TestM Bool
popTwoSa = TestM $ \s -> case scriptTwoSa s of
  (b : bs) -> (b, s{scriptTwoSa = bs})
  [] -> (True, s)


instance LoginEvent TestM where
  type State TestM = TestState


  initial = pure (TestState ())


  ratifyCreds (TestState ()) = asksScript $ \s ->
    if scriptCreds s
      then GotCreds (TestState ())
      else NoCreds (TestState ())


  ratifyArtifactDir (TestState ()) = asksScript $ \s ->
    if scriptDir s
      then DirPresent (TestState ())
      else DirAbsent (TestState ())


  mkArtifactDir (TestState ()) = asksScript $ \s ->
    if scriptMkDir s
      then DirMade (TestState ())
      else NotMade (TestState ())


  loadSession (TestState ()) = asksScript $ \s ->
    if scriptHasSavedSession s
      then HasPriorSession (TestState ())
      else HasClientId (TestState ())


  validateSession (TestState ()) = asksScript $ \s ->
    if scriptSessionValid s
      then SessionStillValid (TestState ())
      else SessionStale (TestState ())


  srpInit (TestState ()) = pure (TestState ())


  srpComplete (TestState ()) = asksScript $ \s ->
    if scriptSrpInvalidKey s
      then SrpCompleteInvalidKey (TestState ())
      else SrpCompleteOk (TestState ())


  acctLogin (TestState ()) = asksScript $ \s ->
    if scriptAcct s
      then AcctLoginOk (TestState ())
      else AcctLogin2SA (TestState ())


  listTwoSaDevices (TestState ()) = pure (TestState ())


  beginTwoFa (TestState ()) _cfg = pure (TestState ())


  doTrust (TestState ()) = pure (TestState ())


  verifyTwoFa (TestState ()) _cfg = do
    result <- popTwoFa
    if result
      then pure $ TwoFaOk (TestState ())
      else asksScript $ \s ->
        if scriptTwoFaLocked s
          then TwoFaLocked (TestState ())
          else TwoFaRetry (TestState ())


  beginTwoSa (TestState ()) _cfg = pure (TestState ())


  verifyTwoSa (TestState ()) _cfg = do
    result <- popTwoSa
    pure $ if result then TwoSaOk (TestState ()) else TwoSaRetry (TestState ())


data Outcome
  = Authenticated
  | TwoFa
  | TwoSa
  | HaltCreds
  | HaltMkDir
  | HaltSrp
  | LockedByTwoFa
  deriving (Eq, Show)


outcomeOf :: LoginOutcome TestState -> Outcome
outcomeOf = \case
  LoginAuthenticated _ -> Authenticated
  LoginNeedsTwoFa _ -> TwoFa
  LoginNeedsTwoSa _ -> TwoSa
  LoginHaltCreds _ -> HaltCreds
  LoginHaltDir _ -> HaltMkDir
  LoginHaltSrp _ -> HaltSrp
  LoginHaltTwoFaLocked _ -> LockedByTwoFa


completionOutcomeOf :: CompletionOutcome TestState -> Outcome
completionOutcomeOf = \case
  CompletionAuthenticated _ -> Authenticated
  CompletionNeedsTwoFa _ -> TwoFa
  CompletionNeedsTwoSa _ -> TwoSa
  CompletionTwoFaLocked _ -> LockedByTwoFa


runScript :: Script -> Outcome
runScript s = outcomeOf $ runTestM loginProcess s


runTwoFaScript :: Script -> Outcome
runTwoFaScript s = completionOutcomeOf $ runTestM (twoFaProcess (TestState ()) dummyTwoFaConfig) s


runTwoSaScript :: Script -> Outcome
runTwoSaScript s = completionOutcomeOf $ runTestM (twoSaProcess (TestState ()) dummyTwoSaConfig) s


dummyTwoFaConfig :: TwoFaConfig
dummyTwoFaConfig = TwoFaConfig{tfcPickPhone = \_ -> pure Nothing, tfcReadCode = pure ""}


dummyTwoSaConfig :: TwoSaConfig
dummyTwoSaConfig = TwoSaConfig{tscPickDevice = pure . head, tscReadCode = pure ""}


spec :: Spec
spec = do
  describe "LoginFSM.loginProcess" $ do
    it "halts when credentials are missing" $
      runScript (allTrue{scriptCreds = False}) `shouldBe` HaltCreds

    it "halts when the artifact directory cannot be created" $
      runScript (allTrue{scriptDir = False, scriptMkDir = False}) `shouldBe` HaltMkDir

    it "halts with invalid SRP key when the server public value is bad" $
      runScript (allTrue{scriptSrpInvalidKey = True}) `shouldBe` HaltSrp

    it "reaches Authenticated on the happy path" $
      runScript allTrue `shouldBe` Authenticated

    it "reaches Requires2SA when account login signals 2SA required" $
      runScript (allTrue{scriptAcct = False}) `shouldBe` TwoSa

    it "creates the artifact directory when absent then reaches Authenticated" $
      runScript (allTrue{scriptDir = False}) `shouldBe` Authenticated

    it "returns Authenticated immediately when the saved session is still valid" $
      runScript (allTrue{scriptHasSavedSession = True, scriptSessionValid = True}) `shouldBe` Authenticated

    it "falls through to SRP when the saved session is stale" $
      runScript (allTrue{scriptHasSavedSession = True, scriptSessionValid = False}) `shouldBe` Authenticated

  describe "LoginFSM.twoFaProcess" $ do
    it "reaches Authenticated when 2FA verification succeeds on the first attempt" $
      runTwoFaScript allTrue `shouldBe` Authenticated

    it "retries and reaches Authenticated after a failed 2FA verification" $
      runTwoFaScript (allTrue{scriptTwoFa = [False, True]}) `shouldBe` Authenticated

    it "reaches Requires2SA when account login signals 2SA required after 2FA" $
      runTwoFaScript (allTrue{scriptAcct = False}) `shouldBe` TwoSa

    it "still reaches Authenticated when noTrustedDevices is True" $
      runTwoFaScript (allTrue{scriptNoTrustedDevices = True}) `shouldBe` Authenticated

    it "halts with TwoFaLocked when the code is rejected and the server signals the account is locked" $
      runTwoFaScript (allTrue{scriptTwoFa = [False], scriptTwoFaLocked = True}) `shouldBe` LockedByTwoFa

  describe "LoginFSM.twoSaProcess" $ do
    it "reaches Authenticated when 2SA verification succeeds on the first attempt" $
      runTwoSaScript allTrue `shouldBe` Authenticated

    it "retries and reaches Authenticated after a failed 2SA verification" $
      runTwoSaScript (allTrue{scriptTwoSa = [False, True]}) `shouldBe` Authenticated

    it "reaches Requires2SA when account login signals 2SA required after 2SA" $
      runTwoSaScript (allTrue{scriptAcct = False}) `shouldBe` TwoSa


newtype TestM a = TestM (Script -> (a, Script))


instance Functor TestM where
  fmap f (TestM m) = TestM $ \s -> let (a, s') = m s in (f a, s')


instance Applicative TestM where
  pure a = TestM (a,)
  TestM mf <*> TestM ma = TestM $ \s ->
    let (f, s') = mf s
        (a, s'') = ma s'
     in (f a, s'')


instance Monad TestM where
  return = pure
  TestM ma >>= f = TestM $ \s ->
    let (a, s') = ma s
        TestM mb = f a
     in mb s'


runTestM :: TestM a -> Script -> a
runTestM (TestM m) s = fst (m s)


asksScript :: (Script -> a) -> TestM a
asksScript f = TestM $ \s -> (f s, s)
