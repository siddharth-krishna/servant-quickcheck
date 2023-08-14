{-# LANGUAGE OverloadedStrings, DataKinds, ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Servant
import Servant.QuickCheck
import Test.Hspec
import Data.List (intercalate)
import Data.String.Conversions (cs)
import Data.Text (Text)
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, forConcurrently)
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forM_, forM)
import Network.HTTP.Client (Request, RequestBody (..), host, method, path, port, queryString, requestBody, requestHeaders, secure, defaultRequest, newManager, defaultManagerSettings)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp (defaultSettings, setLogger)
import Servant.QuickCheck.Internal.HasGenRequest (runGenRequest)
import Test.QuickCheck.Monadic (monadicIO, forAllM, run, assert)
-- import Test.QuickCheck.Parallel (pRunWithNum) --, pDet)
import Servant.QuickCheck.Internal.QuickCheck (defManager, noCheckStatus)
import Servant.QuickCheck.Internal.Predicates (finishPredicates)
import Test.QuickCheck (quickCheckWithResult, Result (..))
import Servant.Client (client, ClientM, runClientM, mkClientEnv)
import Data.Either (partitionEithers)
import Data.Sequence (mapWithIndex)

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
        -- liftIO $ threadDelay 1000000
        x2 <- liftIO $ readMVar mvar2
        -- liftIO $ putStrLn $ "getFn: returning " <> show (x1 + x2)
        when (odd (x1 + x2)) $ do
          liftIO $ putStrLn "ERROR: not even"
        --   throwError err500 {errBody = "Not even"}
        return $ x1 + x2
  let postFn :: Handler ()
      postFn = do
        -- liftIO $ putStrLn "postFn"
        x1 <- liftIO $ takeMVar mvar1
        liftIO $ putMVar mvar1 (x1 + 1)
        -- liftIO $ threadDelay 1000000
        x2 <- liftIO $ takeMVar mvar2
        liftIO $ putMVar mvar2 (x2 + 1)
        return ()
  return $ getFn :<|> postFn

getClient :: ClientM Int
postClient :: ClientM ()
getClient :<|> postClient = client api

-- | Generate all possible interleavings of the input lists.
-- From https://stackoverflow.com/a/41929156
-- NOTE: is there a more efficient way to do this?
interleavings :: [[a]] -> [[a]]
interleavings = go . filter (not . null) where
    go [] = [[]]
    go xss = do
        (xssl, x:xs, xssr) <- zippers xss
        (x:) <$> interleavings ([xs | not (null xs)] ++ xssl ++ xssr)
    zippers :: [a] -> [([a], a, [a])]
    zippers = go' [] where
        go' l (h:r) = (l,h,r) : go' (h:l) r
        go' _ [] = []

-- | Convert a list of tuples to a list of lists, using the integers as indexes
-- in the returned list
-- TODO test that gather is the inverse of interleavings
gather :: [(Int, a)] -> [[a]]
gather [] = []
gather ((i, x) : xs) = insert i x (gather xs)

-- | Insert an element into the list at a given index
insert :: Int -> a -> [[a]] -> [[a]]
-- If the index is zero, prepend the element to the first sublist or create a new sublist if the list is empty
insert 0 x [] = [[x]]
insert 0 x (l : ls) = (x : l) : ls
-- If the index is negative, return the original list
insert i x ls | i < 0 = ls
-- If the index is positive, recurse on the tail of the list and decrement the index
insert i x (l : ls) = l : insert (i - 1) x ls
-- If the index is larger than the length of the list, append the element to the last sublist or create a new sublist if the list is empty
insert i x [] = replicate i [] ++ [[x]]
-- insert i x ls@(l : _) | i >= length ls = init ls ++ [last l ++ [x]]
-- Otherwise, return the original list
-- insert _ _ ls = ls

spec :: Spec
spec = describe "example server" $ do
  let maxTries = 20
  let args = defaultArgs { maxSuccess = maxTries }
  
  it "naive lincheck" $ do
    let settings = defaultSettings
    manager <- newManager defaultManagerSettings
    
    -- Define the concurrent execution to be tested
    let getClientFn = show <$> getClient
    let postClientFn = show <$> postClient
    let exec :: [[ClientM String]] = [[postClientFn, postClientFn], [getClientFn]]

    -- Generate all sequential interleavings and compute expected results
    let seqExecs :: [[(Int, ClientM String)]] =
          -- Add the thread ID to each invocation before interleaving:
          interleavings $ map (\(t, es) -> zip (repeat t) es) $ zip [0..] exec
    seqResults <- forM seqExecs $ \seqExec -> do
      withServantServerAndSettings api settings server $ \burl -> do
        let runFnWithThreadID (t, f) = (t,) <$> f
        let seqClient = sequence $ map runFnWithThreadID seqExec
        runClientM seqClient (mkClientEnv manager burl) >>= \case
          Left err -> error $ "Error computing expected results:\n" ++ show err
          Right res -> return res
    -- Gather the results and stringify for easy comparison
    let stringifyResults results =
          intercalate "|" $ map (intercalate ",") results
    let expectedResults = map (stringifyResults . gather) seqResults
    print expectedResults -- TODO remove

    -- Run concurrently and check results are as expected
    let thread1 = do
          postClient
          postClient
          return []
    let thread2 = do
          i <- getClient
          return [show i]
    forM_ [1..1000 :: Int] $ \i -> do
      -- Can replace withServantServerAndSettings with its def
      withServantServerAndSettings api settings server $ \burl -> do
        res <- forConcurrently exec $ \thredFns ->
          runClientM (sequence thredFns) $ mkClientEnv manager burl
        print i
        -- (res1, res2) <- concurrently
        --   (runClientM thread1 $ mkClientEnv manager burl)
        --   (runClientM thread2 $ mkClientEnv manager burl)
        -- TODO debug Connection refused socket does not exist 
        -- TODO if the below is commented out it doesn't even show ERROR
        -- case partitionEithers res of
        --   ([], results) ->
        --     shouldContain expectedResults [stringifyResults results]
        --     -- return ()
        --   (errs, _) ->
        --     expectationFailure $ "There was an error:\n" ++ show errs
        return ()


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
