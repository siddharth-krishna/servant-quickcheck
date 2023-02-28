{-# LANGUAGE OverloadedStrings, DataKinds, ScopedTypeVariables, TypeOperators #-}
module Main (main) where

import Servant
import Servant.QuickCheck
import Test.Hspec
import Data.Text (Text)
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp (defaultSettings, setLogger)
import Servant.QuickCheck.Internal.HasGenRequest (runGenRequest)
import Test.QuickCheck.Monadic (monadicIO, forAllM, run, assert)
-- import Test.QuickCheck.Parallel (pRunWithNum) --, pDet)
import Servant.QuickCheck.Internal.QuickCheck (defManager, noCheckStatus)
import Servant.QuickCheck.Internal.Predicates (finishPredicates)
import Test.QuickCheck (quickCheckWithResult, Result (..))

main :: IO ()
main = hspec spec

-- Change to String to reproduce
-- https://github.com/haskell-servant/servant-quickcheck/issues/41
type API = Get '[JSON] Int :<|> Post '[JSON] ()

api :: Proxy API
api = Proxy

-- 2 MVars, get reads sequentially with delay and 500s if sum not even
-- post increments sequentially with delay
server :: IO (Server API)
server = do
  mvar1 <- newMVar 0
  mvar2 <- newMVar 0
  let getFn :: Handler Int
      getFn = do
        x1 <- liftIO $ readMVar mvar1
        liftIO $ threadDelay 1000000
        x2 <- liftIO $ readMVar mvar2
        liftIO $ putStrLn $ "getFn: returning " <> show (x1 + x2)
        when (odd (x1 + x2)) $ do
          liftIO $ putStrLn "ERROR: not even"
          throwError err500 {errBody = "Not even"}
        return $ x1 + x2
  let postFn :: Handler ()
      postFn = do
        liftIO $ putStrLn "postFn"
        x1 <- liftIO $ takeMVar mvar1
        liftIO $ putMVar mvar1 (x1 + 1)
        liftIO $ threadDelay 1000000
        x2 <- liftIO $ takeMVar mvar2
        liftIO $ putMVar mvar2 (x2 + 1)
        return ()
  return $ getFn :<|> postFn

spec :: Spec
spec = describe "example server" $ do
  let maxTries = 20
  let args = defaultArgs { maxSuccess = maxTries }

  it "concurrent test" $ withStdoutLogger $ \appLogger -> do
    -- let settings = setLogger appLogger defaultSettings
    let settings = defaultSettings
    let preds = not500 <%> mempty

    withServantServerAndSettings api settings server $ \burl -> do
      -- serverSatisfies api burl args (not500 <%> mempty)
      deetsMVar <- newEmptyMVar
      let reqs = ($ burl) <$> runGenRequest api
      let prop = monadicIO $ forAllM reqs $ \req -> do
            v <- run $ finishPredicates preds (noCheckStatus req) defManager
            _ <- run $ tryPutMVar deetsMVar v
            case v of
              Just _ -> assert False
              _ -> return ()
      (res1, res2) <- concurrently
        (quickCheckWithResult args {chatty = False} prop)
        (quickCheckWithResult args {chatty = False} prop)
      -- TODO need a way to kill other threads when property violated in one thread
      case (res1, res2) of
        (Success {}, Success {}) -> return ()
        _ -> do
          mx <- tryReadMVar deetsMVar
          case mx of
            Just x ->
              expectationFailure $ "Failed:\n" ++ show x
            Nothing ->
              expectationFailure $ "We failed to record a reason for failure: " <> show (res1, res2)

  -- TODO pqc doesn't exit early when property violated -- need to use async?
  -- TODO pqc shouldn't ExitCode, it should do what normal quickchecks do
  -- it "concurrent test using pqc" $ do
  --   -- let settings = setLogger appLogger defaultSettings
  --   let settings = defaultSettings
  --   let preds = not500 <%> mempty

  --   withServantServerAndSettings api settings server $ \burl -> do
  --     -- serverSatisfies api burl args (not500 <%> mempty)
  --     let reqs = ($ burl) <$> runGenRequest api
  --     let prop = monadicIO $ forAllM reqs $ \req -> do
  --           v <- run $ finishPredicates preds (noCheckStatus req) defManager
  --           case v of
  --             Just _ -> assert False
  --             _ -> return ()
  --     let test n = quickCheckWithResult args {chatty = False, maxSuccess = n} prop
  --     pRunWithNum 4 maxTries [("thread " ++ show i, test) | i :: Integer <- [1..4]]
  --     return ()

  -- NOTE: this is not a concurrent test, so it passes
  -- it "no 500s" $ withStdoutLogger $ \appLogger -> do
  --     let settings = setLogger appLogger defaultSettings
  --     withServantServerAndSettings api settings server $ \burl ->
  --         serverSatisfies api burl args (not500 <%> mempty)
