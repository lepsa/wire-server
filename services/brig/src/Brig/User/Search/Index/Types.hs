{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Brig.User.Search.Index.Types where

import Brig.Types.Search
import Control.Lens (makeLenses)
import Control.Monad.Catch
import Data.Aeson
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util (UTCTimeMillis (..), toUTCTimeMillis)
import Data.Text qualified as T
import Data.Text.ICU.Translit (trans, transliterate)
import Data.Time (UTCTime)
import Database.Bloodhound hiding (key)
import Database.Bloodhound.Internal.Client (DocVersion (DocVersion))
import Imports
import Wire.API.Team.Role (Role)
import Wire.API.User
import Wire.API.User.Search (Sso (..))

data IndexDocUpdateType
  = IndexUpdateIfNewerVersion
  | IndexUpdateIfSameOrNewerVersion

data IndexUpdate
  = IndexUpdateUser IndexDocUpdateType IndexUser
  | IndexUpdateUsers IndexDocUpdateType [IndexUser]
  | IndexDeleteUser UserId

-- | Represents the ES *index*, ie. the attributes of a user that is searchable in ES.  See also:
-- 'UserDoc'.
data IndexUser = IndexUser
  { _userId :: UserId,
    _version :: IndexVersion,
    _team :: Maybe TeamId,
    _name :: Maybe Name,
    _handle :: Maybe Handle,
    _email :: Maybe Email,
    _colourId :: Maybe ColourId,
    _accountStatus :: Maybe AccountStatus,
    _samlIdP :: Maybe Text,
    _managedBy :: Maybe ManagedBy,
    _createdAt :: Maybe UTCTime,
    _role :: Maybe Role,
    _searchVisibilityInbound :: Maybe SearchVisibilityInbound,
    _scimExternalId :: Maybe Text,
    _sso :: Maybe Sso,
    _emailUnvalidated :: Maybe Email
  }

data IndexQuery r = IndexQuery Query Filter [DefaultSort]

data IndexError
  = IndexUpdateError EsError
  | IndexLookupError EsError
  | IndexError Text
  deriving (Show)

instance Exception IndexError

newtype IndexVersion = IndexVersion {docVersion :: DocVersion}

-- | Represents an ES *document*, ie. the subset of user attributes stored in ES.
-- See also 'IndexUser'.
--
-- If a user is not searchable, e.g. because the account got
-- suspended, all fields except for the user id are set to 'Nothing' and
-- consequently removed from the index.
data UserDoc = UserDoc
  { id :: UserId,
    team :: Maybe TeamId,
    name :: Maybe Name,
    normalized :: Maybe Text,
    handle :: Maybe Handle,
    email :: Maybe Email,
    colourId :: Maybe ColourId,
    accountStatus :: Maybe AccountStatus,
    samlIdP :: Maybe Text,
    managedBy :: Maybe ManagedBy,
    createdAt :: Maybe UTCTimeMillis,
    role :: Maybe Role,
    searchVisibilityInbound :: Maybe SearchVisibilityInbound,
    scimExternalId :: Maybe Text,
    sso :: Maybe Sso,
    emailUnvalidated :: Maybe Email
  }
  deriving (Eq, Show)

-- Note: Keep this compatible with the FromJSON instances
-- of 'Contact' and 'TeamContact' from 'Wire.API.User.Search
instance ToJSON UserDoc where
  toJSON ud =
    object
      [ "id" .= ud.id,
        "team" .= ud.team,
        "name" .= ud.name,
        "normalized" .= ud.normalized,
        "handle" .= ud.handle,
        "email" .= ud.email,
        "accent_id" .= ud.colourId,
        "account_status" .= ud.accountStatus,
        "saml_idp" .= ud.samlIdP,
        "managed_by" .= ud.managedBy,
        "created_at" .= ud.createdAt,
        "role" .= ud.role,
        (fromString . T.unpack $ searchVisibilityInboundFieldName) .= ud.searchVisibilityInbound,
        "scim_external_id" .= ud.scimExternalId,
        "sso" .= ud.sso,
        "email_unvalidated" .= ud.emailUnvalidated
      ]

instance FromJSON UserDoc where
  parseJSON = withObject "UserDoc" $ \o ->
    UserDoc
      <$> o .: "id"
      <*> o .:? "team"
      <*> o .:? "name"
      <*> o .:? "normalized"
      <*> o .:? "handle"
      <*> o .:? "email"
      <*> o .:? "accent_id"
      <*> o .:? "account_status"
      <*> o .:? "saml_idp"
      <*> o .:? "managed_by"
      <*> o .:? "created_at"
      <*> o .:? "role"
      <*> o .:? (fromString . T.unpack $ searchVisibilityInboundFieldName)
      <*> o .:? "scim_external_id"
      <*> o .:? "sso"
      <*> o .:? "email_unvalidated"

searchVisibilityInboundFieldName :: Text
searchVisibilityInboundFieldName = "search_visibility_inbound"

makeLenses ''IndexUser

mkIndexVersion :: (MonadThrow m, Integral a) => a -> m IndexVersion
mkIndexVersion i =
  if i > fromIntegral (maxBound :: Int)
    then throwM $ IndexError "Index overflow"
    else pure . IndexVersion . fromMaybe maxBound . mkDocVersion . fromIntegral $ i

mkIndexUser :: UserId -> IndexVersion -> IndexUser
mkIndexUser u v =
  IndexUser
    { _userId = u,
      _version = v,
      _team = Nothing,
      _name = Nothing,
      _handle = Nothing,
      _email = Nothing,
      _colourId = Nothing,
      _accountStatus = Nothing,
      _samlIdP = Nothing,
      _managedBy = Nothing,
      _createdAt = Nothing,
      _role = Nothing,
      _searchVisibilityInbound = Nothing,
      _scimExternalId = Nothing,
      _sso = Nothing,
      _emailUnvalidated = Nothing
    }

indexToDoc :: IndexUser -> UserDoc
indexToDoc iu =
  UserDoc
    { id = _iuUserId iu,
      team = _iuTeam iu,
      name = _iuName iu,
      accountStatus = _iuAccountStatus iu,
      normalized = normalizedText . fromName <$> _iuName iu,
      handle = _iuHandle iu,
      email = _iuEmail iu,
      colourId = _iuColourId iu,
      samlIdP = _iuSAMLIdP iu,
      managedBy = _iuManagedBy iu,
      createdAt = toUTCTimeMillis <$> _iuCreatedAt iu,
      role = _iuRole iu,
      searchVisibilityInbound = _iuSearchVisibilityInbound iu,
      scimExternalId = _iuScimExternalId iu,
      sso = _iuSso iu,
      emailUnvalidated = _iuEmailUnvalidated iu
    }

-- | FUTUREWORK: Transliteration should be left to ElasticSearch (ICU plugin), but this will
-- require a data migration.
normalizedText :: Text -> Text
normalizedText = transliterate (trans "Any-Latin; Latin-ASCII; Lower")

docToIndex :: UserDoc -> IndexUser
docToIndex ud =
  -- (Don't use 'mkIndexUser' here!  With 'IndexUser', you get compiler warnings if you
  -- forget to add new fields here.)
  IndexUser
    { _userId = ud.id,
      _version = IndexVersion (DocVersion 1),
      _team = ud.team,
      _name = ud.name,
      _handle = ud.handle,
      _email = ud.email,
      _colourId = ud.colourId,
      _accountStatus = ud.accountStatus,
      _samlIdP = ud.samlIdP,
      _managedBy = ud.managedBy,
      _createdAt = fromUTCTimeMillis <$> ud.createdAt,
      _role = ud.role,
      _searchVisibilityInbound = ud.searchVisibilityInbound,
      _scimExternalId = ud.scimExternalId,
      _sso = ud.sso,
      _emailUnvalidated = ud.emailUnvalidated
    }
