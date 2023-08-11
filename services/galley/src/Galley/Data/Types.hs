{-# LANGUAGE StrictData #-}

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

module Galley.Data.Types
  ( Conversation (..),
    isConvDeleted,
    selfConv,
    Code (..),
    Scope (..),
    toCode,
    generate,
    mkKey,
    LockAcquired (..),
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.Code
import Data.Id
import Data.Range
import Data.Text.Ascii qualified as Ascii
import Galley.Data.Conversation
import Galley.Data.Scope
import Imports
import OpenSSL.EVP.Digest (digestBS, getDigestByName)
import OpenSSL.Random (randBytes)
import Wire.API.Password (Password)

--------------------------------------------------------------------------------
-- Code

data Code = Code
  { key :: !Key,
    value :: !Value,
    ttl :: !Timeout,
    conversation :: !ConvId,
    scope :: !Scope,
    hasPassword :: !Bool
  }
  deriving (Eq, Show, Generic)

toCode :: Key -> Scope -> (Value, Int32, ConvId, Maybe Password) -> (Code, Maybe Password)
toCode k s (val, ttl, cnv, mPw) =
  ( Code
      { key = k,
        value = val,
        ttl = Timeout (fromIntegral ttl),
        conversation = cnv,
        scope = s,
        hasPassword = isJust mPw
      },
    mPw
  )

-- Note on key/value used for a conversation Code
--
-- For similar reasons to those given for Codes used for verification, Password reset, etc
-- (see services/brig/src/Brig/Code.hs Note [Unique keys])
-- The 'key' is a stable, truncated, base64 encoded sha256 hash of the conversation ID
-- The 'value' is a base64 encoded, 120-bit random value (changing on each generation)

generate :: MonadIO m => ConvId -> Scope -> Timeout -> m Code
generate cnv s t = do
  key <- mkKey cnv
  val <- liftIO $ Value . unsafeRange . Ascii.encodeBase64Url <$> randBytes 15
  pure
    Code
      { key = key,
        value = val,
        conversation = cnv,
        ttl = t,
        scope = s,
        hasPassword = False
      }

mkKey :: MonadIO m => ConvId -> m Key
mkKey cnv = do
  sha256 <- liftIO $ fromJust <$> getDigestByName "SHA256"
  pure $ Key . unsafeRange . Ascii.encodeBase64Url . BS.take 15 $ digestBS sha256 (toByteString' cnv)

data LockAcquired
  = Acquired
  | NotAcquired
  deriving (Show, Eq)
