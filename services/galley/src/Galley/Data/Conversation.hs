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

module Galley.Data.Conversation
  ( -- * Data Conversation types
    Conversation (..),
    NewConversation,

    -- * Utilities
    isConvDeleted,
    selfConv,
    localOne2OneConvId,
    convAccess,
    convAccessData,
    convAccessRoles,
    convCreator,
    convMessageTimer,
    convName,
    convReceiptMode,
    convSetName,
    convType,
    convSetType,
    convTeam,
    defRole,
    maybeRole,
    defRegularConvAccess,
    parseAccessRoles,
  )
where

import Data.Id (ConvId, Id (Id, toUUID), TeamId, UserId)
import Data.Misc (Milliseconds)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.UUID.Tagged qualified as U
import Galley.Cassandra.Instances ()
import Galley.Data.Conversation.Types
  ( Conversation (..),
    NewConversation,
  )
import Imports
  ( Alternative ((<|>)),
    Bool,
    Maybe,
    Text,
    ($),
    (.),
    (<$>),
  )
import Wire.API.Conversation
  ( Access (InviteAccess),
    AccessRole,
    AccessRoleLegacy,
    ConvType,
    ConversationAccessData (ConversationAccessData),
    ConversationMetadata
      ( access,
        accessRoles,
        creator,
        messageTimer,
        name,
        receiptMode,
        team,
        type'
      ),
    ReceiptMode,
    defRole,
    fromAccessRoleLegacy,
    maybeRole,
  )

isConvDeleted :: Conversation -> Bool
isConvDeleted = deleted

selfConv :: UserId -> ConvId
selfConv uid = Id (toUUID uid)

-- | We deduce the conversation ID by adding the 4 components of the V4 UUID
-- together pairwise, and then setting the version bits (v4) and variant bits
-- (variant 2). This means that we always know what the UUID is for a
-- one-to-one conversation which hopefully makes them unique.
localOne2OneConvId :: U.UUID U.V4 -> U.UUID U.V4 -> ConvId
localOne2OneConvId a b = Id . U.unpack $ U.addv4 a b

convType :: Conversation -> ConvType
convType = type' . (.metadata)

convSetType :: ConvType -> Conversation -> Conversation
convSetType t c = c {metadata = c.metadata {type' = t}}

convTeam :: Conversation -> Maybe TeamId
convTeam = team . (.metadata)

convAccess :: Conversation -> [Access]
convAccess = access . (.metadata)

convAccessRoles :: Conversation -> Set AccessRole
convAccessRoles = accessRoles . (.metadata)

convAccessData :: Conversation -> ConversationAccessData
convAccessData c =
  ConversationAccessData
    (Set.fromList (convAccess c))
    (convAccessRoles c)

convCreator :: Conversation -> UserId
convCreator = creator . (.metadata)

convName :: Conversation -> Maybe Text
convName = name . (.metadata)

convSetName :: Maybe Text -> Conversation -> Conversation
convSetName n c = c {metadata = c.metadata {name = n}}

defRegularConvAccess :: [Access]
defRegularConvAccess = [InviteAccess]

parseAccessRoles :: Maybe AccessRoleLegacy -> Maybe (Set AccessRole) -> Maybe (Set AccessRole)
parseAccessRoles mbLegacy mbAccess = mbAccess <|> fromAccessRoleLegacy <$> mbLegacy

convMessageTimer :: Conversation -> Maybe Milliseconds
convMessageTimer = messageTimer . (.metadata)

convReceiptMode :: Conversation -> Maybe ReceiptMode
convReceiptMode = receiptMode . (.metadata)
