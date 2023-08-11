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
module Gundeck.Redis.HedisExtensions
  ( ClusterInfoResponse (..),
    ClusterInfoResponseState (..),
    clusterInfo,
    checkedConnectCluster,
    ClusterDownError,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Database.Redis
import Imports hiding (Down)
import UnliftIO

-- https://redis.io/commands/cluster-info/
data ClusterInfoResponse = ClusterInfoResponse
  { state :: ClusterInfoResponseState,
    slotsAssigned :: Integer,
    slotsOK :: Integer,
    slotsPfail :: Integer,
    slotsFail :: Integer,
    knownNodes :: Integer,
    size :: Integer,
    currentEpoch :: Integer,
    myEpoch :: Integer,
    statsMessagesSent :: Integer,
    statsMessagesReceived :: Integer,
    totalLinksBufferLimitExceeded :: Integer,
    statsMessagesPingSent :: Maybe Integer,
    statsMessagesPingReceived :: Maybe Integer,
    statsMessagesPongSent :: Maybe Integer,
    statsMessagesPongReceived :: Maybe Integer,
    statsMessagesMeetSent :: Maybe Integer,
    statsMessagesMeetReceived :: Maybe Integer,
    statsMessagesFailSent :: Maybe Integer,
    statsMessagesFailReceived :: Maybe Integer,
    statsMessagesPublishSent :: Maybe Integer,
    statsMessagesPublishReceived :: Maybe Integer,
    statsMessagesAuthReqSent :: Maybe Integer,
    statsMessagesAuthReqReceived :: Maybe Integer,
    statsMessagesAuthAckSent :: Maybe Integer,
    statsMessagesAuthAckReceived :: Maybe Integer,
    statsMessagesUpdateSent :: Maybe Integer,
    statsMessagesUpdateReceived :: Maybe Integer,
    statsMessagesMfstartSent :: Maybe Integer,
    statsMessagesMfstartReceived :: Maybe Integer,
    statsMessagesModuleSent :: Maybe Integer,
    statsMessagesModuleReceived :: Maybe Integer,
    statsMessagesPublishshardSent :: Maybe Integer,
    statsMessagesPublishshardReceived :: Maybe Integer
  }
  deriving (Show, Eq)

data ClusterInfoResponseState
  = OK
  | Down
  deriving (Show, Eq)

defClusterInfoResponse :: ClusterInfoResponse
defClusterInfoResponse =
  ClusterInfoResponse
    { state = Down,
      slotsAssigned = 0,
      slotsOK = 0,
      slotsPfail = 0,
      slotsFail = 0,
      knownNodes = 0,
      size = 0,
      currentEpoch = 0,
      myEpoch = 0,
      statsMessagesSent = 0,
      statsMessagesReceived = 0,
      totalLinksBufferLimitExceeded = 0,
      statsMessagesPingSent = Nothing,
      statsMessagesPingReceived = Nothing,
      statsMessagesPongSent = Nothing,
      statsMessagesPongReceived = Nothing,
      statsMessagesMeetSent = Nothing,
      statsMessagesMeetReceived = Nothing,
      statsMessagesFailSent = Nothing,
      statsMessagesFailReceived = Nothing,
      statsMessagesPublishSent = Nothing,
      statsMessagesPublishReceived = Nothing,
      statsMessagesAuthReqSent = Nothing,
      statsMessagesAuthReqReceived = Nothing,
      statsMessagesAuthAckSent = Nothing,
      statsMessagesAuthAckReceived = Nothing,
      statsMessagesUpdateSent = Nothing,
      statsMessagesUpdateReceived = Nothing,
      statsMessagesMfstartSent = Nothing,
      statsMessagesMfstartReceived = Nothing,
      statsMessagesModuleSent = Nothing,
      statsMessagesModuleReceived = Nothing,
      statsMessagesPublishshardSent = Nothing,
      statsMessagesPublishshardReceived = Nothing
    }

parseClusterInfoResponse :: [[ByteString]] -> ClusterInfoResponse -> Maybe ClusterInfoResponse
parseClusterInfoResponse fields resp = case fields of
  [] -> pure resp
  (["cluster_state", state] : fs) -> parseState state >>= \s -> parseClusterInfoResponse fs $ resp {state = s}
  (["cluster_slots_assigned", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {slotsAssigned = v}
  (["cluster_slots_ok", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {slotsOK = v}
  (["cluster_slots_pfail", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {slotsPfail = v}
  (["cluster_slots_fail", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {slotsFail = v}
  (["cluster_known_nodes", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {knownNodes = v}
  (["cluster_size", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {size = v}
  (["cluster_current_epoch", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {currentEpoch = v}
  (["cluster_my_epoch", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {myEpoch = v}
  (["cluster_stats_messages_sent", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {statsMessagesSent = v}
  (["cluster_stats_messages_received", value] : fs) -> parseInteger value >>= \v -> parseClusterInfoResponse fs $ resp {statsMessagesReceived = v}
  (["total_cluster_links_buffer_limit_exceeded", value] : fs) -> parseClusterInfoResponse fs $ resp {totalLinksBufferLimitExceeded = fromMaybe 0 $ parseInteger value} -- this value should be mandatory according to the spec, but isn't necessarily set in Redis 6
  (["cluster_stats_messages_ping_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesPingSent = parseInteger value}
  (["cluster_stats_messages_ping_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesPingReceived = parseInteger value}
  (["cluster_stats_messages_pong_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesPongSent = parseInteger value}
  (["cluster_stats_messages_pong_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesPongReceived = parseInteger value}
  (["cluster_stats_messages_meet_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesMeetSent = parseInteger value}
  (["cluster_stats_messages_meet_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesMeetReceived = parseInteger value}
  (["cluster_stats_messages_fail_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesFailSent = parseInteger value}
  (["cluster_stats_messages_fail_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesFailReceived = parseInteger value}
  (["cluster_stats_messages_publish_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesPublishSent = parseInteger value}
  (["cluster_stats_messages_publish_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesPublishReceived = parseInteger value}
  (["cluster_stats_messages_auth_req_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesAuthReqSent = parseInteger value}
  (["cluster_stats_messages_auth_req_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesAuthReqReceived = parseInteger value}
  (["cluster_stats_messages_auth_ack_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesAuthAckSent = parseInteger value}
  (["cluster_stats_messages_auth_ack_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesAuthAckReceived = parseInteger value}
  (["cluster_stats_messages_update_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesUpdateSent = parseInteger value}
  (["cluster_stats_messages_update_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesUpdateReceived = parseInteger value}
  (["cluster_stats_messages_mfstart_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesMfstartSent = parseInteger value}
  (["cluster_stats_messages_mfstart_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesMfstartReceived = parseInteger value}
  (["cluster_stats_messages_module_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesModuleSent = parseInteger value}
  (["cluster_stats_messages_module_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesModuleReceived = parseInteger value}
  (["cluster_stats_messages_publishshard_sent", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesPublishshardSent = parseInteger value}
  (["cluster_stats_messages_publishshard_received", value] : fs) -> parseClusterInfoResponse fs $ resp {statsMessagesPublishshardReceived = parseInteger value}
  (_ : fs) -> parseClusterInfoResponse fs resp
  where
    parseState bs = case bs of
      "ok" -> Just OK
      "fail" -> Just Down
      _ -> Nothing
    parseInteger = fmap fst . Char8.readInteger

instance RedisResult ClusterInfoResponse where
  decode r@(Bulk (Just bulkData)) =
    maybe (Left r) Right
      . flip parseClusterInfoResponse defClusterInfoResponse
      . map (Char8.split ':' . Char8.takeWhile (/= '\r'))
      $ Char8.lines bulkData
  decode r = Left r

clusterInfo :: RedisCtx m f => m (f ClusterInfoResponse)
clusterInfo = sendRequest ["CLUSTER", "INFO"]

checkedConnectCluster :: ConnectInfo -> IO Connection
checkedConnectCluster connInfo = do
  conn <- connectCluster connInfo
  res <- runRedis conn clusterInfo
  case res of
    Right r -> case state r of
      OK -> pure conn
      _ -> throwIO $ ClusterDownError r
    Left e -> throwIO $ ConnectSelectError e

newtype ClusterDownError = ClusterDownError ClusterInfoResponse deriving (Eq, Show, Typeable)

instance Exception ClusterDownError
