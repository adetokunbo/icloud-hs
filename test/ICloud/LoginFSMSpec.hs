{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module ICloud.LoginFSMSpec
  ( spec
  )
where

import Network.ICloud.Internal.LoginFSM
import Test.Hspec (Spec, describe, it, shouldBe)


newtype TestState s = TestState ()


data Script = Script
  { scriptCreds :: Bool
  , scriptDir :: Bool
  , scriptMkDir :: Bool
  , scriptLoad :: Bool
  , scriptSrp :: Bool
  , scriptAcct :: Bool
  }


allTrue :: Script
allTrue =
  Script
    { scriptCreds = True
    , scriptDir = True
    , scriptMkDir = True
    , scriptLoad = True
    , scriptSrp = True
    , scriptAcct = True
    }


newtype TestM a = TestM {runTestM :: Script -> a}
  deriving (Functor, Applicative, Monad)


asksScript :: (Script -> a) -> TestM a
asksScript = TestM


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
    if scriptLoad s
      then HasClientId (TestState ())
      else NeedsClientId (TestState ())


  mkClientId (TestState ()) = pure (TestState ())


  srpInit (TestState ()) = pure (TestState ())


  srpDone (TestState ()) = asksScript $ \s ->
    if scriptSrp s
      then SrpDoneOk (TestState ())
      else SrpDone2FA (TestState ())


  acctLogin (TestState ()) = asksScript $ \s ->
    if scriptAcct s
      then AcctLoginOk (TestState ())
      else AcctLogin2SA (TestState ())


  end _ = pure Halted


data Outcome
  = Authenticated
  | TwoFa
  | TwoSa
  | HaltCreds
  | HaltMkDir
  | HaltSrp
  deriving (Eq, Show)


outcomeOf :: BeforeEnd TestState -> Outcome
outcomeOf = \case
  EndedAuthenticated _ -> Authenticated
  EndedNeedsTwoFa _ -> TwoFa
  EndedNeedsTwoSa _ -> TwoSa
  EndedAfterCredentials _ -> HaltCreds
  EndedAfterMkArtifactDir _ -> HaltMkDir
  EndedHaltInvalidSrp _ -> HaltSrp


runScript :: Script -> Outcome
runScript s = outcomeOf $ runTestM loginProcess s


spec :: Spec
spec = describe "LoginFSM.loginProcess" $ do
  it "halts when credentials are missing" $
    runScript (allTrue{scriptCreds = False}) `shouldBe` HaltCreds

  it "halts when the artifact directory cannot be created" $
    runScript (allTrue{scriptDir = False, scriptMkDir = False}) `shouldBe` HaltMkDir

  it "reaches Requires2FA when SRP completes with a 2FA challenge" $
    runScript (allTrue{scriptSrp = False}) `shouldBe` TwoFa

  it "reaches Authenticated on the happy path" $
    runScript allTrue `shouldBe` Authenticated

  it "reaches Requires2SA when account login signals 2SA required" $
    runScript (allTrue{scriptAcct = False}) `shouldBe` TwoSa

  it "passes through mkClientId when the session has no client ID" $
    runScript (allTrue{scriptLoad = False}) `shouldBe` Authenticated

  it "creates the artifact directory when absent then reaches Authenticated" $
    runScript (allTrue{scriptDir = False}) `shouldBe` Authenticated
