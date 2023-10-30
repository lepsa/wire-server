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

module Wire.API.Federation.API.Galley
  ( module Wire.API.Federation.API.Galley,
    module Notifications,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Json.Util
import Data.Misc (Milliseconds)
import Data.OpenApi (OpenApi, ToSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Qualified
import Data.Time.Clock (UTCTime)
import Imports
import Servant.API
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Conversation.Typing
import Wire.API.Error.Galley
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley.Notifications as Notifications
import Wire.API.Federation.Endpoint
import Wire.API.MLS.SubConversation
import Wire.API.MakesFederatedCall
import Wire.API.Util.Aeson (CustomEncoded (..))
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

-- FUTUREWORK: data types, json instances, more endpoints. See
-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/356090113/Federation+Galley+Conversation+API
-- for the current list we need.

type GalleyApi =
  -- | Register a new conversation. This is only called on backends of users
  -- that are part of a conversation at creation time. Since MLS conversations
  -- are always created empty (i.e. they only contain the creator), this RPC is
  -- never invoked for such conversations.
  FedEndpoint "on-conversation-created" (ConversationCreated ConvId) EmptyResponse
    -- This endpoint is called the first time a user from this backend is
    -- added to a remote conversation.
    :<|> FedEndpoint "get-conversations" GetConversationsRequest GetConversationsResponse
    :<|> FedEndpoint "mls-welcome" MLSWelcomeRequest MLSWelcomeResponse
    :<|> FedEndpoint "query-group-info" GetGroupInfoRequest GetGroupInfoResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-typing-indicator-updated"
            ]
           "update-typing-indicator"
           TypingDataUpdateRequest
           TypingDataUpdateResponse
    :<|> FedEndpoint "on-typing-indicator-updated" TypingDataUpdated EmptyResponse
    :<|> FedEndpoint "get-sub-conversation" GetSubConversationsRequest GetSubConversationsResponse
    :<|> FedEndpointWithMods
           '[
            ]
           "delete-sub-conversation"
           DeleteSubConversationFedRequest
           DeleteSubConversationResponse
    :<|> FedEndpoint
           "get-one2one-conversation"
           GetOne2OneConversationRequest
           GetOne2OneConversationResponse
    -- All the notification endpoints that go through the queue-based
    -- federation client ('fedQueueClient').
    :<|> GalleyNotificationAPI

data TypingDataUpdateRequest = TypingDataUpdateRequest
  { typingStatus :: TypingStatus,
    userId :: UserId,
    convId :: ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdateRequest)

instance ToSchema TypingDataUpdateRequest

data TypingDataUpdateResponse
  = TypingDataUpdateSuccess TypingDataUpdated
  | TypingDataUpdateError GalleyError
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdateResponse)

instance ToSchema TypingDataUpdateResponse

data TypingDataUpdated = TypingDataUpdated
  { time :: UTCTime,
    origUserId :: Qualified UserId,
    -- | Implicitely qualified by sender's domain
    convId :: ConvId,
    -- | Implicitely qualified by receiver's domain
    usersInConv :: [UserId],
    typingStatus :: TypingStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (CustomEncoded TypingDataUpdated)

instance ToSchema TypingDataUpdated

data GetConversationsRequest = GetConversationsRequest
  { userId :: UserId,
    convIds :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsRequest)

instance ToSchema GetConversationsRequest

data GetOne2OneConversationRequest = GetOne2OneConversationRequest
  { -- The user on the sender's domain
    goocSenderUser :: UserId,
    -- The user on the receiver's domain
    goocReceiverUser :: UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetOne2OneConversationRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetOne2OneConversationRequest)

instance ToSchema GetOne2OneConversationRequest

data RemoteConvMembers = RemoteConvMembers
  { selfRole :: RoleName,
    others :: [OtherMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConvMembers)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConvMembers)

instance ToSchema RemoteConvMembers

-- | A conversation hosted on a remote backend. This contains the same
-- information as a 'Conversation', with the exception that conversation status
-- fields (muted\/archived\/hidden) are omitted, since they are not known by the
-- remote backend.
data RemoteConversation = RemoteConversation
  { -- | Id of the conversation, implicitly qualified with the domain of the
    -- backend that created this value.
    id :: ConvId,
    metadata :: ConversationMetadata,
    members :: RemoteConvMembers,
    protocol :: Protocol
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteConversation)
  deriving (FromJSON, ToJSON) via (CustomEncoded RemoteConversation)

instance ToSchema RemoteConversation

newtype GetConversationsResponse = GetConversationsResponse
  { convs :: [RemoteConversation]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetConversationsResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetConversationsResponse)

instance ToSchema GetConversationsResponse

data GetOne2OneConversationResponse
  = GetOne2OneConversationOk RemoteConversation
  | -- | This is returned when the local backend is asked for a 1-1 conversation
    -- that should reside on the other backend.
    GetOne2OneConversationBackendMismatch
  | -- | This is returned when a 1-1 conversation between two unconnected users
    -- is requested.
    GetOne2OneConversationNotConnected
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetOne2OneConversationResponse)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetOne2OneConversationResponse)

instance ToSchema GetOne2OneConversationResponse

-- | A record type describing a new federated conversation
--
-- FUTUREWORK: Think about extracting common conversation metadata into a
-- separarate data type that can be reused in several data types in this module.
data ConversationCreated conv = ConversationCreated
  { -- | The time when the conversation was created
    time :: UTCTime,
    -- | The user that created the conversation. This is implicitly qualified
    -- by the requesting domain, since it is impossible to create a regular/group
    -- conversation on a remote backend.
    origUserId :: UserId,
    -- | The conversation ID, local to the backend invoking the RPC
    cnvId :: conv,
    -- | The conversation type
    cnvType :: ConvType,
    cnvAccess :: [Access],
    cnvAccessRoles :: Set AccessRole,
    -- | The conversation name,
    cnvName :: Maybe Text,
    -- | Members of the conversation apart from the creator
    nonCreatorMembers :: Set OtherMember,
    messageTimer :: Maybe Milliseconds,
    receiptMode :: Maybe ReceiptMode,
    protocol :: Protocol
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (ToJSON, FromJSON) via (CustomEncoded (ConversationCreated conv))

instance (ToSchema a) => ToSchema (ConversationCreated a)

ccRemoteOrigUserId :: ConversationCreated (Remote ConvId) -> Remote UserId
ccRemoteOrigUserId cc = qualifyAs cc.cnvId cc.origUserId

data RemoteMLSMessageResponse
  = RemoteMLSMessageOk
  | RemoteMLSMessageMLSNotEnabled
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoteMLSMessageResponse)

instance ToSchema RemoteMLSMessageResponse

-- | A wrapper around a raw welcome message
data MLSWelcomeRequest = MLSWelcomeRequest
  { -- | Implicitely qualified by origin domain
    originatingUser :: UserId,
    -- | A serialised welcome message.
    welcomeMessage :: Base64ByteString,
    -- | Recipients local to the target backend.
    recipients :: [(UserId, ClientId)],
    -- | The conversation id, qualified to the owning domain
    qualifiedConvId :: Qualified ConvId
  }
  deriving stock (Eq, Generic, Show)
  deriving (Arbitrary) via (GenericUniform MLSWelcomeRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded MLSWelcomeRequest)

instance ToSchema MLSWelcomeRequest

data MLSWelcomeResponse
  = MLSWelcomeSent
  | MLSWelcomeMLSNotEnabled
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (CustomEncoded MLSWelcomeResponse)

instance ToSchema MLSWelcomeResponse

data GetGroupInfoRequest = GetGroupInfoRequest
  { -- | Conversation (or subconversation) is assumed to be owned by the target
    -- domain, this allows us to protect against relay attacks
    conv :: ConvOrSubConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    sender :: UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GetGroupInfoRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetGroupInfoRequest)

instance ToSchema GetGroupInfoRequest

data GetGroupInfoResponse
  = GetGroupInfoResponseError GalleyError
  | GetGroupInfoResponseState Base64ByteString
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetGroupInfoResponse)

instance ToSchema GetGroupInfoResponse

data GetSubConversationsRequest = GetSubConversationsRequest
  { gsreqUser :: UserId,
    gsreqConv :: ConvId,
    gsreqSubConv :: SubConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetSubConversationsRequest)

instance ToSchema GetSubConversationsRequest

data GetSubConversationsResponse
  = GetSubConversationsResponseError GalleyError
  | GetSubConversationsResponseSuccess PublicSubConversation
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded GetSubConversationsResponse)

instance ToSchema GetSubConversationsResponse

data DeleteSubConversationFedRequest = DeleteSubConversationFedRequest
  { dscreqUser :: UserId,
    dscreqConv :: ConvId,
    dscreqSubConv :: SubConvId,
    dscreqGroupId :: GroupId,
    dscreqEpoch :: Epoch
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded DeleteSubConversationFedRequest)

instance ToSchema DeleteSubConversationFedRequest

data DeleteSubConversationResponse
  = DeleteSubConversationResponseError GalleyError
  | DeleteSubConversationResponseSuccess
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded DeleteSubConversationResponse)

instance ToSchema DeleteSubConversationResponse

swaggerDoc :: OpenApi
swaggerDoc = toOpenApi (Proxy @GalleyApi)
