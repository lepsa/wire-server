module Wire.API.FederationUpdate
  ( FedUpdateCallback,
    getAllowedDomainsInitial,
    getAllowedDomainsLoop,
    getAllowedDomainsLoop',
  )
where

import Imports
import Servant.Client (ClientEnv, ClientError, runClientM)
import Servant.Client.Internal.HttpClient (ClientM)
import qualified System.Logger as L
import Wire.API.Routes.FederationDomainConfig (FederationDomainConfigs (updateInterval))
import qualified Wire.API.Routes.Internal.Brig as IAPI
import Wire.API.Routes.Named (namedClient)

getFedRemotes :: ClientM FederationDomainConfigs
getFedRemotes = namedClient @IAPI.API @"get-federation-remotes"

-- Initial function for getting the set of domains from brig, and an update interval
getAllowedDomainsInitial :: L.Logger -> ClientEnv -> IO FederationDomainConfigs
getAllowedDomainsInitial logger clientEnv =
  let oneSec = 1000000 -- microsends
      go :: IO FederationDomainConfigs
      go = do
        getAllowedDomains clientEnv >>= \case
          Right s -> pure s
          Left e -> do
            L.log logger L.Info $
              L.msg (L.val "Could not retrieve an initial list of federation domains from Brig.")
                L.~~ "error" L..= show e
            threadDelay oneSec -- TODO: use retry instead.
            go
   in go

getAllowedDomains :: ClientEnv -> IO (Either ClientError FederationDomainConfigs)
getAllowedDomains = runClientM getFedRemotes

type FedUpdateCallback = FederationDomainConfigs -> FederationDomainConfigs -> IO ()

-- The callback takes the previous and the new values of the federation domain configs
-- and runs a given action. This function is not called if a new config value cannot be fetched.
getAllowedDomainsLoop :: L.Logger -> ClientEnv -> IORef FederationDomainConfigs -> FedUpdateCallback -> IO ()
getAllowedDomainsLoop logger clientEnv env callback = forever $ do
  getAllowedDomains clientEnv >>= \case
    Left e ->
      L.log logger L.Fatal $
        L.msg (L.val "Could not retrieve an updated list of federation domains from Brig; I'll keep trying!")
          L.~~ "error" L..= show e
    Right cfg -> do
      old <- readIORef env
      callback old cfg
      atomicWriteIORef env cfg
  delay <- updateInterval <$> readIORef env
  threadDelay delay

-- A version where the callback isn't needed. Most of the services don't care about
-- when the list changes, just that they have the new list and can use it as-is
getAllowedDomainsLoop' :: L.Logger -> ClientEnv -> IORef FederationDomainConfigs -> IO ()
getAllowedDomainsLoop' logger c r = getAllowedDomainsLoop logger c r $ \_ _ -> pure ()
