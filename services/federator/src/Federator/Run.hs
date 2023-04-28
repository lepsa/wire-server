{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Federator.Run
  ( run,

    -- * App Environment
    newEnv,
    closeEnv,

    -- * Re-exports
    mkTLSSettingsOrThrow,
    FederationSetupError (..),
  )
where

import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Lens ((^.), (%~))
import Data.Default (def)
import qualified Data.Metrics.Middleware as Metrics
import Federator.Env
import Federator.ExternalServer (serveInward)
import Federator.InternalServer (serveOutward)
import Federator.Monitor
import Federator.Options as Opt
import Imports
import qualified Network.DNS as DNS
import qualified Network.HTTP.Client as HTTP
import qualified System.Logger.Class as Log
import qualified System.Logger.Extended as LogExt
import Util.Options
import Wire.API.Federation.Component
import qualified Wire.Network.DNS.Helper as DNS
import qualified Wire.API.Routes.Internal.Brig as IAPI
import Servant.Client
import Wire.API.Routes.FederationDomainConfig
import Network.HTTP.Client
import Data.Text
import Wire.API.Routes.Named

------------------------------------------------------------------------------
-- run/app

-- FUTUREWORK(federation): Add metrics and status endpoints
run :: Opts -> IO ()
run opts = do
  let resolvConf = mkResolvConf (optSettings opts) DNS.defaultResolvConf
  DNS.withCachingResolver resolvConf $ \res ->
    bracket (newEnv opts res) closeEnv $ \env -> do
      -- Build a new TVar holding the state we want for the initial environment.
      -- This needs to contact Brig before accepting other requests
      manager <- newManager defaultManagerSettings
      let Endpoint host port = brig opts
          baseUrl = BaseUrl Http (unpack host) (fromIntegral port) ""
          clientEnv = ClientEnv manager baseUrl Nothing defaultMakeClientRequest
          -- Loop the request until we get an answer. This is helpful during integration
          -- tests where services are being brought up in parallel.
          getInitialFedDomains = do
            runClientM getFedRemotes clientEnv >>= \case
              Right s -> pure s
              Left e -> do
                print $ "Could not retrieve the latest list of federation domains from Brig: " <> show e
                threadDelay $ domainUpdateInterval opts
                getInitialFedDomains
      fedStrat <- getInitialFedDomains
      tEnv <- newTVarIO $ updateFedStrat fedStrat env
      let
          callback :: FederationDomainConfigs -> IO ()
          callback strat = do
            atomically $ modifyTVar tEnv $ updateFedStrat strat
            print strat
      -- We need a watcher/listener for updating this TVar to flow values through to the handlers.
      let externalServer = serveInward tEnv portExternal
          internalServer = serveOutward tEnv portInternal
      withMonitor (env ^. applog) (onNewSSLContext env) (optSettings opts) $ do
        envUpdateThread <- async $ updateDomains clientEnv callback
        internalServerThread <- async internalServer
        externalServerThread <- async externalServer
        void $ waitAnyCancel [envUpdateThread, internalServerThread, externalServerThread]
  where
    updateFedStrat :: FederationDomainConfigs -> Env -> Env
    updateFedStrat fedDomConfigs = Federator.Env.runSettings %~ \s -> s { federationStrategy = AllowList $ AllowedDomains $ domain <$> fromFederationDomainConfigs fedDomConfigs }

    endpointInternal = federatorInternal opts
    portInternal = fromIntegral $ endpointInternal ^. epPort

    endpointExternal = federatorExternal opts
    portExternal = fromIntegral $ endpointExternal ^. epPort

    mkResolvConf :: RunSettings -> DNS.ResolvConf -> DNS.ResolvConf
    mkResolvConf settings conf =
      case (dnsHost settings, dnsPort settings) of
        (Just host, Nothing) ->
          conf {DNS.resolvInfo = DNS.RCHostName host}
        (Just host, Just port) ->
          conf {DNS.resolvInfo = DNS.RCHostPort host (fromIntegral port)}
        (_, _) -> conf

    getFedRemotes = namedClient @IAPI.API @"get-federation-remotes"

    updateDomains :: ClientEnv -> (FederationDomainConfigs -> IO ()) -> IO ()
    updateDomains clientEnv update = forever $ do
      threadDelay $ domainUpdateInterval opts
      strat <- runClientM getFedRemotes clientEnv
      either
        print
        update
        strat      
        
        

-------------------------------------------------------------------------------
-- Environment

newEnv :: Opts -> DNS.Resolver -> IO Env
newEnv o _dnsResolver = do
  _metrics <- Metrics.metrics
  _applog <- LogExt.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  let _requestId = def
  let _runSettings = Opt.optSettings o
  let _service Brig = Opt.brig o
      _service Galley = Opt.galley o
      _service Cargohold = Opt.cargohold o
  _httpManager <- initHttpManager
  sslContext <- mkTLSSettingsOrThrow _runSettings
  _http2Manager <- newIORef =<< mkHttp2Manager sslContext
  pure Env {..}

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog

initHttpManager :: IO HTTP.Manager
initHttpManager =
  HTTP.newManager
    HTTP.defaultManagerSettings
      { HTTP.managerConnCount = 1024,
        HTTP.managerIdleConnectionCount = 4096,
        HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 10000000
      }
