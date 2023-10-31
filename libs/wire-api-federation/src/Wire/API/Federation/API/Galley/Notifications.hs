{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Federation.API.Galley.Notifications where

import Data.Aeson
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty
import Data.OpenApi (ToSchema)
import Data.Qualified
import Data.Range
import Data.Time.Clock
import Imports
import Network.Wai.Utilities.JSONResponse
import Servant.API
import Wire.API.Conversation.Action
import Wire.API.Error.Galley
import Wire.API.Federation.Component
import Wire.API.Federation.Endpoint
import Wire.API.Federation.HasNotificationEndpoint
import Wire.API.MLS.SubConversation
import Wire.API.MakesFederatedCall
import Wire.API.Message
import Wire.API.Routes.Public.Galley.Messaging
import Wire.API.Util.Aeson
import Wire.Arbitrary

data GalleyNotificationTag
  = OnClientRemovedTag
  | OnMessageSentTag
  | OnMLSMessageSentTag
  | OnConversationUpdatedTag
  | OnUserDeletedConversationsTag
  deriving (Show, Eq, Generic, Bounded, Enum)

instance HasNotificationEndpoint 'OnClientRemovedTag where
  type Payload 'OnClientRemovedTag = ClientRemovedRequest
  type NotificationPath 'OnClientRemovedTag = "on-client-removed"
  type NotificationComponent 'OnClientRemovedTag = 'Galley
  type
    NotificationAPI 'OnClientRemovedTag 'Galley =
      NotificationFedEndpointWithMods
        '[ MakesFederatedCall 'Galley "on-mls-message-sent"
         ]
        (NotificationPath 'OnClientRemovedTag)
        (Payload 'OnClientRemovedTag)

instance HasNotificationEndpoint 'OnMessageSentTag where
  type Payload 'OnMessageSentTag = RemoteMessage ConvId
  type NotificationPath 'OnMessageSentTag = "on-message-sent"
  type NotificationComponent 'OnMessageSentTag = 'Galley

  -- used to notify this backend that a new message has been posted to a
  -- remote conversation
  type NotificationAPI 'OnMessageSentTag 'Galley = NotificationFedEndpoint 'OnMessageSentTag

instance HasNotificationEndpoint 'OnMLSMessageSentTag where
  type Payload 'OnMLSMessageSentTag = RemoteMLSMessage
  type NotificationPath 'OnMLSMessageSentTag = "on-mls-message-sent"
  type NotificationComponent 'OnMLSMessageSentTag = 'Galley
  type NotificationAPI 'OnMLSMessageSentTag 'Galley = NotificationFedEndpoint 'OnMLSMessageSentTag

instance HasNotificationEndpoint 'OnConversationUpdatedTag where
  type Payload 'OnConversationUpdatedTag = ConversationUpdate
  type NotificationPath 'OnConversationUpdatedTag = "on-conversation-updated"
  type NotificationComponent 'OnConversationUpdatedTag = 'Galley

  -- used by the backend that owns a conversation to inform this backend of
  -- changes to the conversation
  type NotificationAPI 'OnConversationUpdatedTag 'Galley = NotificationFedEndpoint 'OnConversationUpdatedTag

instance HasNotificationEndpoint 'OnUserDeletedConversationsTag where
  type Payload 'OnUserDeletedConversationsTag = UserDeletedConversationsNotification
  type NotificationPath 'OnUserDeletedConversationsTag = "on-user-deleted-conversations"
  type NotificationComponent 'OnUserDeletedConversationsTag = 'Galley
  type
    NotificationAPI 'OnUserDeletedConversationsTag 'Galley =
      NotificationFedEndpointWithMods
        '[ MakesFederatedCall 'Galley "on-mls-message-sent",
           MakesFederatedCall 'Galley "on-conversation-updated",
           MakesFederatedCall 'Brig "api-version"
         ]
        (NotificationPath 'OnUserDeletedConversationsTag)
        (Payload 'OnUserDeletedConversationsTag)

-- | All the notification endpoints return an 'EmptyResponse'.
type GalleyNotificationsAPI =
  NotificationAPI 'OnClientRemovedTag 'Galley
    :<|> NotificationAPI 'OnMessageSentTag 'Galley
    :<|> NotificationAPI 'OnMLSMessageSentTag 'Galley
    :<|> NotificationAPI 'OnConversationUpdatedTag 'Galley
    :<|> NotificationAPI 'OnUserDeletedConversationsTag 'Galley
    -- Similar to above, but only that this uses the queue at all, not that it is entirely queue based
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Brig "get-users-by-ids",
              MakesFederatedCall 'Brig "api-version"
            ]
           "leave-conversation"
           LeaveConversationRequest
           LeaveConversationResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Brig "get-users-by-ids",
              MakesFederatedCall 'Galley "on-mls-message-sent"
            ]
           "update-conversation"
           ConversationUpdateRequest
           ConversationUpdateResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "mls-welcome",
              MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Galley "send-mls-commit-bundle",
              MakesFederatedCall 'Brig "get-mls-clients",
              MakesFederatedCall 'Brig "get-users-by-ids",
              MakesFederatedCall 'Brig "api-version"
            ]
           "send-mls-commit-bundle"
           MLSMessageSendRequest
           MLSMessageResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent",
              MakesFederatedCall 'Galley "send-mls-message",
              MakesFederatedCall 'Brig "get-mls-clients"
            ]
           "send-mls-message"
           MLSMessageSendRequest
           MLSMessageResponse
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-mls-message-sent"
            ]
           "leave-sub-conversation"
           LeaveSubConversationRequest
           LeaveSubConversationResponse
    -- used by a remote backend to send a message to a conversation owned by
    -- this backend
    :<|> FedEndpointWithMods
           '[ MakesFederatedCall 'Galley "on-message-sent",
              MakesFederatedCall 'Brig "get-user-clients"
            ]
           "send-message"
           ProteusMessageSendRequest
           MessageSendResponse

data ClientRemovedRequest = ClientRemovedRequest
  { user :: UserId,
    client :: ClientId,
    convs :: [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientRemovedRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ClientRemovedRequest)

instance ToSchema ClientRemovedRequest

-- Note: this is parametric in the conversation type to allow it to be used
-- both for conversations with a fixed known domain (e.g. as the argument of the
-- federation RPC), and for conversations with an arbitrary Qualified or Remote id
-- (e.g. as the argument of the corresponding handler).
data RemoteMessage conv = RemoteMessage
  { time :: UTCTime,
    _data :: Maybe Text,
    sender :: Qualified UserId,
    senderClient :: ClientId,
    conversation :: conv,
    priority :: Maybe Priority,
    push :: Bool,
    transient :: Bool,
    recipients :: UserClientMap Text
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (Arbitrary) via (GenericUniform (RemoteMessage conv))
  deriving (ToJSON, FromJSON) via (CustomEncodedLensable (RemoteMessage conv))

instance (ToSchema a) => ToSchema (RemoteMessage a)

data RemoteMLSMessage = RemoteMLSMessage
  { time :: UTCTime,
    metadata :: MessageMetadata,
    sender :: Qualified UserId,
    conversation :: ConvId,
    subConversation :: Maybe SubConvId,
    recipients :: Map UserId (NonEmpty ClientId),
    message :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteMLSMessage)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoteMLSMessage)

instance ToSchema RemoteMLSMessage

data ConversationUpdate = ConversationUpdate
  { cuTime :: UTCTime,
    cuOrigUserId :: Qualified UserId,
    -- | The unqualified ID of the conversation where the update is happening.
    -- The ID is local to the sender to prevent putting arbitrary domain that
    -- is different than that of the backend making a conversation membership
    -- update request.
    cuConvId :: ConvId,
    -- | A list of users from the receiving backend that need to be sent
    -- notifications about this change. This is required as we do not expect a
    -- non-conversation owning backend to have an indexed mapping of
    -- conversation to users.
    cuAlreadyPresentUsers :: [UserId],
    -- | Information on the specific action that caused the update.
    cuAction :: SomeConversationAction
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConversationUpdate

instance FromJSON ConversationUpdate

instance ToSchema ConversationUpdate

type UserDeletedNotificationMaxConvs = 1000

data UserDeletedConversationsNotification = UserDeletedConversationsNotification
  { -- | This is qualified implicitly by the origin domain
    user :: UserId,
    -- | These are qualified implicitly by the target domain
    conversations :: Range 1 UserDeletedNotificationMaxConvs [ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserDeletedConversationsNotification)
  deriving (FromJSON, ToJSON) via (CustomEncoded UserDeletedConversationsNotification)

instance ToSchema UserDeletedConversationsNotification

data LeaveConversationRequest = LeaveConversationRequest
  { -- | The conversation is assumed to be owned by the target domain, which
    -- allows us to protect against relay attacks
    convId :: ConvId,
    -- | The leaver is assumed to be owned by the origin domain, which allows us
    -- to protect against spoofing attacks
    leaver :: UserId
  }
  deriving stock (Generic, Eq, Show)
  deriving (ToJSON, FromJSON) via (CustomEncoded LeaveConversationRequest)

instance ToSchema LeaveConversationRequest

newtype LeaveConversationResponse = LeaveConversationResponse
  {response :: Either RemoveFromConversationError ()}
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via (Either (CustomEncoded RemoveFromConversationError) ())

instance ToSchema LeaveConversationResponse

data ConversationUpdateRequest = ConversationUpdateRequest
  { -- | The user that is attempting to perform the action. This is qualified
    -- implicitly by the origin domain
    user :: UserId,
    -- | Id of conversation the action should be performed on. The is qualified
    -- implicity by the owning backend which receives this request.
    convId :: ConvId,
    action :: SomeConversationAction
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationUpdateRequest)
  deriving (FromJSON, ToJSON) via (CustomEncoded ConversationUpdateRequest)

instance ToSchema ConversationUpdateRequest

data ConversationUpdateResponse
  = ConversationUpdateResponseError GalleyError
  | ConversationUpdateResponseUpdate ConversationUpdate
  | ConversationUpdateResponseNoChanges
  | ConversationUpdateResponseNonFederatingBackends NonFederatingBackends
  | ConversationUpdateResponseUnreachableBackends UnreachableBackends
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomEncoded ConversationUpdateResponse)

instance ToSchema ConversationUpdateResponse

data MLSMessageSendRequest = MLSMessageSendRequest
  { -- | Conversation (or sub conversation) is assumed to be owned by the target
    -- domain, this allows us to protect against relay attacks
    convOrSubId :: ConvOrSubConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    sender :: UserId,
    senderClient :: ClientId,
    rawMessage :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform MLSMessageSendRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSMessageSendRequest)

instance ToSchema MLSMessageSendRequest

data MLSMessageResponse
  = MLSMessageResponseError GalleyError
  | MLSMessageResponseProtocolError Text
  | MLSMessageResponseProposalFailure JSONResponse
  | -- | The conversation-owning backend could not reach some of the backends that
    -- have users in the conversation when processing a commit.
    MLSMessageResponseUnreachableBackends (Set Domain)
  | -- | If the list of unreachable users is non-empty, it corresponds to users
    -- that an application message could not be sent to.
    MLSMessageResponseUpdates [ConversationUpdate]
  | MLSMessageResponseNonFederatingBackends NonFederatingBackends
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MLSMessageResponse)

instance ToSchema MLSMessageResponse

data LeaveSubConversationRequest = LeaveSubConversationRequest
  { lscrUser :: UserId,
    lscrClient :: ClientId,
    lscrConv :: ConvId,
    lscrSubConv :: SubConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LeaveSubConversationRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded LeaveSubConversationRequest)

instance ToSchema LeaveSubConversationRequest

data LeaveSubConversationResponse
  = LeaveSubConversationResponseError GalleyError
  | LeaveSubConversationResponseProtocolError Text
  | LeaveSubConversationResponseOk
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded LeaveSubConversationResponse)

instance ToSchema LeaveSubConversationResponse

data ProteusMessageSendRequest = ProteusMessageSendRequest
  { -- | Conversation is assumed to be owned by the target domain, this allows
    -- us to protect against relay attacks
    convId :: ConvId,
    -- | Sender is assumed to be owned by the origin domain, this allows us to
    -- protect against spoofing attacks
    sender :: UserId,
    rawMessage :: Base64ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ProteusMessageSendRequest)
  deriving (ToJSON, FromJSON) via (CustomEncoded ProteusMessageSendRequest)

instance ToSchema ProteusMessageSendRequest

newtype MessageSendResponse = MessageSendResponse
  {response :: PostOtrResponse MessageSendingStatus}
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via ( Either
            (CustomEncoded (MessageNotSent MessageSendingStatus))
            MessageSendingStatus
        )

instance ToSchema MessageSendResponse

-- | Error outcomes of the leave-conversation RPC.
data RemoveFromConversationError
  = RemoveFromConversationErrorRemovalNotAllowed
  | RemoveFromConversationErrorNotFound
  | RemoveFromConversationErrorUnchanged
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded RemoveFromConversationError)

instance ToSchema RemoveFromConversationError
