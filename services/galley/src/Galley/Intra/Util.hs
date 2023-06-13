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

module Galley.Intra.Util
  ( IntraComponent (..),
    call,
    asyncCall,
    HasIntraComponentEndpoints (..),
  )
where

import Bilge hiding (getHeader, options, statusCode)
import Bilge.RPC
import Bilge.Retry
import Control.Lens (Lens', (^.))
import Control.Monad.Catch
import Control.Retry
import qualified Data.ByteString.Lazy as LB
import Data.Misc (portNumber)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import Galley.Env (Env, options)
import Galley.Options
import Imports hiding (log)
import Network.HTTP.Types
import System.Logger
import qualified System.Logger.Class as LC
import Util.Options

data IntraComponent = Brig | Spar | Gundeck
  deriving (Show)

componentName :: IntraComponent -> String
componentName Brig = "brig"
componentName Spar = "spar"
componentName Gundeck = "gundeck"

class HasIntraComponentEndpoints opts where
  brig :: Lens' opts Endpoint
  gundeck :: Lens' opts Endpoint
  spar :: Lens' opts Endpoint

instance HasIntraComponentEndpoints Env where
  brig = options . optBrig
  spar = options . optSpar
  gundeck = options . optGundeck

componentRequest :: HasIntraComponentEndpoints c => IntraComponent -> c -> Request -> Request
componentRequest Brig o =
  host (encodeUtf8 (o ^. brig . epHost))
    . port (portNumber (fromIntegral (o ^. brig . epPort)))
componentRequest Spar o =
  host (encodeUtf8 (o ^. spar . epHost))
    . port (portNumber (fromIntegral (o ^. spar . epPort)))
componentRequest Gundeck o =
  host (encodeUtf8 $ o ^. gundeck . epHost)
    . port (portNumber $ fromIntegral (o ^. gundeck . epPort))
    . method POST
    . path "/i/push/v2"
    . expect2xx

componentRetryPolicy :: IntraComponent -> RetryPolicy
componentRetryPolicy Brig = x1
componentRetryPolicy Spar = x1
componentRetryPolicy Gundeck = x3

call ::
  ( MonadReader c m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    HasIntraComponentEndpoints c
  ) =>
  IntraComponent ->
  (Request -> Request) ->
  m (Response (Maybe LB.ByteString))
call comp r = do
  o <- ask
  let r0 = componentRequest comp o
  let n = LT.pack (componentName comp)
  recovering (componentRetryPolicy comp) rpcHandlers (const (rpc n (r . r0)))

asyncCall ::
  ( MonadUnliftIO m,
    MonadReader c m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    HasIntraComponentEndpoints c,
    LC.MonadLogger m
  ) =>
  IntraComponent ->
  (Request -> Request) ->
  m ()
asyncCall comp req = void $ do
  let n = LT.pack (componentName comp)
  forkIO $ catches (void (call comp req)) (handlers n)
  where
    handlers n =
      [ Handler $ \(x :: RPCException) -> LC.err (rpcExceptionMsg x),
        Handler $ \(x :: SomeException) -> LC.err $ "remote" .= n ~~ msg (show x)
      ]

x1 :: RetryPolicy
x1 = limitRetries 1

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000
