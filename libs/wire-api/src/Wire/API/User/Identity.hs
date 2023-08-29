{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
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

module Wire.API.User.Identity
  ( -- * UserIdentity
    UserIdentity (..),
    newIdentity,
    emailIdentity,
    phoneIdentity,
    ssoIdentity,
    userIdentityObjectSchema,
    maybeUserIdentityObjectSchema,
    maybeUserIdentityFromComponents,

    -- * Email
    Email (..),
    fromEmail,
    parseEmail,
    validateEmail,

    -- * Phone
    Phone (..),
    parsePhone,
    isValidPhone,

    -- * UserSSOId
    UserSSOId (..),
    emailFromSAML,
    emailToSAML,
    emailToSAMLNameID,
    emailFromSAMLNameID,
    mkSampleUref,
    mkSimpleSampleUref,
  )
where

import Control.Applicative (optional)
import Control.Lens (dimap, ix, over, view, (.~), (?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Attoparsec.Text
import Data.Bifunctor (first)
import Data.ByteString.Conversion
import Data.CaseInsensitive qualified as CI
import Data.Id (TeamId)
import Data.Schema
import Data.Swagger (ToParamSchema (..))
import Data.Swagger qualified as S
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock
import Imports
import SAML2.WebSSO.Test.Arbitrary ()
import SAML2.WebSSO.Types qualified as SAML
import SAML2.WebSSO.Types.Email qualified as SAMLEmail
import SAML2.WebSSO.XML qualified as SAML
import Servant
import Servant.API qualified as S
import System.FilePath ((</>))
import Test.QuickCheck qualified as QC
import Text.Email.Validate qualified as Email.V
import URI.ByteString qualified as URI
import URI.ByteString.QQ (uri)
import Wire.API.User.Profile (fromName, mkName)
import Wire.API.User.Types
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- UserIdentity

-- | The private unique user identity that is used for login and
-- account recovery.
data UserIdentity
  = FullIdentity Email Phone
  | EmailIdentity Email
  | PhoneIdentity Phone
  | UAuthIdentity PartialUAuthId -- email is already represented in this type; phone is not supported for saml/scim users.
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserIdentity)

userIdentityObjectSchema :: ObjectSchema SwaggerDoc UserIdentity
userIdentityObjectSchema =
  Just .= withParser maybeUserIdentityObjectSchema (maybe (fail "Missing 'email' or 'phone' or 'sso_id'.") pure)

maybeUserIdentityObjectSchema :: ObjectSchema SwaggerDoc (Maybe UserIdentity)
maybeUserIdentityObjectSchema =
  dimap maybeUserIdentityToComponents maybeUserIdentityFromComponents userIdentityComponentsObjectSchema

-- | NOTE(fisx): this is getting interesting, and it's late...
--
-- brig sometimes may parse user identities, eg. updates from team-management.  (i'm not sure
-- about this, if it makes a difference i need to go check.)
--
-- if it parser an untrusted value, it must validate the saml stuff, so we need a parser for
-- "`ValidSamlIdF t` for any tag".  not sure how to accomplish that, smells like existential
-- types?  worst case we can decide to leave the validation for later, and just write what we
-- have, even if it's inconsistent.  maybe.  it's possible this is just for parsing cassandra
-- answers.
--
-- the 3 maybe texts at the end here are for the PartialUAuthId.
--
-- NOTE(owen): The last 3 fields being Maybe Text is isn't going to be enough for PartialUAuthId
-- as it has the teamId field that must be filled. At a stretch we could use "", but that feels
-- like asking for trouble. Additionally, what order are the fields in and how should we parse them?
-- We could change the representation of UAuthIdF from it's current functor parameterised form to
-- regular generics for each field, and then have an intermediate type synonum for the functor
-- form to be used in the type family.
-- Eg. data UAuthIdF a b c = UAuthIdF { samlId :: a, scimExternalId :: b }
--     type UAuthIdFunctor a b c = UAuthIdF (a SAML.UserRef) (b Text) (c EmailWithSource)
--     type family ValidUAuthIdF (f :: UAuthIdTag) where
--       ValidUAuthIdF 'UAScimSamlEmail     = UAuthIdFunctor Identity Identity Identity
type UserIdentityComponents = (Maybe Email, Maybe Phone, Maybe LegacyUserSSOId, Maybe PartialUAuthId)

userIdentityComponentsObjectSchema :: ObjectSchema SwaggerDoc UserIdentityComponents
userIdentityComponentsObjectSchema =
  (,,,,)
    <$> fst4 .= maybe_ (optField "email" schema)
    <*> snd4 .= maybe_ (optField "phone" schema)
    <*> thd4 .= maybe_ (optField "sso_id" genericToSchema)
    <*> fth4 .= maybe_ (optField "uauth_id" genericToSchema)
  where
    fst4 (a, _, _, _) = a
    snd4 (_, a, _, _) = a
    thd4 (_, _, a, _) = a
    fth4 (_, _, _, a) = a

maybeUserIdentityFromComponents :: UserIdentityComponents -> Maybe UserIdentity
maybeUserIdentityFromComponents = \case
  (maybeEmail, maybePhone, Just ssoid, _, _) -> Just $ UAuthIdentity $ UAuthIdF _ (pure ssoid) (flip EmailWithSource _ <$> maybeEmail) _ -- maybePhone
  (Just email, Just phone, Nothing, _, _) -> Just $ FullIdentity email phone
  (Just email, Nothing, Nothing, _, _) -> Just $ EmailIdentity email
  (Nothing, Just phone, Nothing, _, _) -> Just $ PhoneIdentity phone
  (Nothing, Nothing, Nothing, _, _) -> Nothing

maybeUserIdentityToComponents :: Maybe UserIdentity -> UserIdentityComponents
maybeUserIdentityToComponents Nothing = (Nothing, Nothing, Nothing, _, _)
maybeUserIdentityToComponents (Just (FullIdentity email phone)) = (Just email, Just phone, Nothing, _, _)
maybeUserIdentityToComponents (Just (EmailIdentity email)) = (Just email, Nothing, Nothing, _, _)
maybeUserIdentityToComponents (Just (PhoneIdentity phone)) = (Nothing, Just phone, Nothing, _, _)
maybeUserIdentityToComponents (Just (UAuthIdentity uaid)) = (ewsEmail <$> uaid.email, Nothing, uaid.scimExternalId, _, _)

newIdentity :: Maybe Email -> Maybe Phone -> Maybe LegacyUserSSOId -> Maybe UserIdentity
newIdentity email phone (Just sso) =
  Just $!
    UAuthIdentity $
      sso.fromLegacyUserSSOId
        { email = flip EmailWithSource _ <$> email
        }
-- UAuthIdF _ sso  _ -- phone
newIdentity Nothing Nothing Nothing = Nothing
newIdentity (Just e) Nothing Nothing = Just $! EmailIdentity e
newIdentity Nothing (Just p) Nothing = Just $! PhoneIdentity p
newIdentity (Just e) (Just p) Nothing = Just $! FullIdentity e p

emailIdentity :: UserIdentity -> Maybe Email
emailIdentity (FullIdentity email _) = Just email
emailIdentity (EmailIdentity email) = Just email
emailIdentity (PhoneIdentity _) = Nothing
emailIdentity (UAuthIdentity uaid) = ewsEmail <$> uaid.email

phoneIdentity :: UserIdentity -> Maybe Phone
phoneIdentity (FullIdentity _ phone) = Just phone
phoneIdentity (PhoneIdentity phone) = Just phone
phoneIdentity (EmailIdentity _) = Nothing
phoneIdentity (UAuthIdentity _) = Nothing

ssoIdentity :: UserIdentity -> Maybe LegacyUserSSOId
ssoIdentity (UAuthIdentity uaid) = pure $ LegacyUserSSOId uaid
ssoIdentity _ = Nothing

-- |
-- FUTUREWORK:
--
-- * Enforce these constrains during parsing already or use a separate type, see
--   [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate).
--
-- * Check for differences to validation of `Data.Domain.Domain` and decide whether to
--   align/de-duplicate the two.
--
-- * Drop dependency on email-validate? We do our own email domain validation anyways,
--   is the dependency worth it just for validating the local part?
validateEmail :: Email -> Either String Email
validateEmail =
  pure . uncurry Email
    <=< validateDomain
    <=< validateExternalLib
    <=< validateLength . fromEmail
  where
    validateLength e
      | len <= 100 = Right e
      | otherwise = Left $ "length " <> show len <> " exceeds 100"
      where
        len = Text.length e
    validateExternalLib e = do
      email <- Email.V.validate $ encodeUtf8 e
      l <- first show . decodeUtf8' $ Email.V.localPart email
      d <- first show . decodeUtf8' $ Email.V.domainPart email
      pure (l, d)
    -- cf. https://en.wikipedia.org/wiki/Email_address#Domain
    -- n.b. We do not allow IP address literals, comments or non-ASCII
    --      characters, mostly because SES (and probably many other mail
    --      systems) don't support that (yet?) either.
    validateDomain (l, d) = parseOnly domain d
      where
        domain = (label *> many1 (char '.' *> label) *> endOfInput) $> (l, d)
        label =
          satisfy (inClass "a-zA-Z0-9")
            *> count 61 (optional (satisfy (inClass "-a-zA-Z0-9")))
            *> optional (satisfy (inClass "a-zA-Z0-9"))

--------------------------------------------------------------------------------
-- Phone

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Phone)

instance ToParamSchema Phone where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema Phone where
  schema =
    over doc (S.description ?~ "E.164 phone number") $
      fromPhone .= parsedText "PhoneNumber" (maybe (Left "Invalid phone number. Expected E.164 format.") Right . parsePhone)

instance ToByteString Phone where
  builder = builder . fromPhone

instance FromByteString Phone where
  parser = parser >>= maybe (fail "Invalid phone") pure . parsePhone

instance S.FromHttpApiData Phone where
  parseUrlPiece = maybe (Left "Invalid phone") Right . fromByteString . cs

instance S.ToHttpApiData Phone where
  toUrlPiece = cs . toByteString'

instance Arbitrary Phone where
  arbitrary =
    Phone . Text.pack <$> do
      let mkdigits n = replicateM n (QC.elements ['0' .. '9'])
      mini <- mkdigits 8
      maxi <- mkdigits =<< QC.chooseInt (0, 7)
      pure $ '+' : mini <> maxi

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p
  | isValidPhone p = Just $! Phone p
  | otherwise = Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput

-- | If the budget for SMS and voice calls for a phone number
-- has been exhausted within a certain time frame, this timeout
-- indicates in seconds when another attempt may be made.
newtype PhoneBudgetTimeout = PhoneBudgetTimeout
  {phoneBudgetTimeout :: NominalDiffTime}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

instance FromJSON PhoneBudgetTimeout where
  parseJSON = A.withObject "PhoneBudgetTimeout" $ \o ->
    PhoneBudgetTimeout <$> o A..: "expires_in"

instance ToJSON PhoneBudgetTimeout where
  toJSON (PhoneBudgetTimeout t) = A.object ["expires_in" A..= t]

--------------------------------------------------------------------------------
-- UserSSOId (DEPRECATED)

-- | User's legacy external identity (DEPRECATED).
-- NB: this type is serialized to the full xml encoding of the `SAML.UserRef` components, but
-- deserialiation is more lenient: it also allows for the `Issuer` to be a plain URL (without
-- xml around it), and the `NameID` to be an email address (=> format "email") or an arbitrary
-- text (=> format "unspecified").  This is for backwards compatibility and general
-- robustness.
data LegacyUserSSOId = LegacyUserSSOId {fromLegacyUserSSOId :: PartialUAuthId}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegacyUserSSOId)

instance S.ToSchema LegacyUserSSOId where
  declareNamedSchema _ = do
    tenantSchema <- S.declareSchemaRef (Proxy @Text) -- FUTUREWORK: 'Issuer'
    subjectSchema <- S.declareSchemaRef (Proxy @Text) -- FUTUREWORK: 'NameID'
    scimSchema <- S.declareSchemaRef (Proxy @Text)
    pure $
      S.NamedSchema (Just "UserSSOId") $
        mempty
          & S.type_ ?~ S.SwaggerObject
          & S.description
            ?~ "[DEPRECATED] Only combinations tenant+subject or scim_external_id are allowed. \
               \If both are available, fill in only tenant+subject.  (This is deprecated, \
               \so please don't ask...)"
          & S.properties
            .~ [ ("tenant", tenantSchema),
                 ("subject", subjectSchema),
                 ("scim_external_id", scimSchema)
               ]

instance ToJSON LegacyUserSSOId where
  toJSON (LegacyUserSSOId uid) = case uid.samlId of
    Just (SAML.UserRef tenant subject) -> A.object ["tenant" A..= SAML.encodeElem tenant, "subject" A..= SAML.encodeElem subject]
    Nothing -> case uid.scimExternalId of
      Just eid -> A.object ["scim_external_id" A..= eid]
      Nothing -> A.object []

instance FromJSON LegacyUserSSOId where
  parseJSON = A.withObject "UserSSOId" $ \obj -> do
    -- TODO
    mtenant <- lenientlyParseSAMLIssuer =<< (obj A..:? "tenant")
    msubject <- lenientlyParseSAMLNameID =<< (obj A..:? "subject")
    meid <- obj A..:? "scim_external_id"
    case (mtenant, msubject, meid) of
      (Just tenant, Just subject, Nothing) -> pure $ LegacyUserSSOId $ UAuthIdF (pure $ SAML.UserRef tenant subject) Nothing Nothing _
      (Nothing, Nothing, Just eid) -> pure $ LegacyUserSSOId $ UAuthIdF Nothing eid Nothing _
      _ -> fail "either need tenant and subject, or scim_external_id, but not both"

lenientlyParseSAMLIssuer :: Maybe LText -> A.Parser (Maybe SAML.Issuer)
lenientlyParseSAMLIssuer mbtxt = forM mbtxt $ \txt -> do
  let asxml :: Either String SAML.Issuer
      asxml = SAML.decodeElem txt

      asurl :: Either String SAML.Issuer
      asurl =
        bimap show SAML.Issuer $
          URI.parseURI URI.laxURIParserOptions (cs txt)

      err :: String
      err = "lenientlyParseSAMLIssuer: " <> show (asxml, asurl, mbtxt)

  either (const $ fail err) pure $ asxml <|> asurl

lenientlyParseSAMLNameID :: Maybe LText -> A.Parser (Maybe SAML.NameID)
lenientlyParseSAMLNameID Nothing = pure Nothing
lenientlyParseSAMLNameID (Just txt) = do
  let asxml :: Either String SAML.NameID
      asxml = SAML.decodeElem txt

      asemail :: Either String SAML.NameID
      asemail =
        maybe
          (Left "not an email")
          (fmap emailToSAMLNameID . validateEmail)
          (parseEmail (cs txt))

      astxt :: Either String SAML.NameID
      astxt = do
        nm <- mkName (cs txt)
        SAML.mkNameID (SAML.mkUNameIDUnspecified (fromName nm)) Nothing Nothing Nothing

      err :: String
      err = "lenientlyParseSAMLNameID: " <> show (asxml, asemail, astxt, txt)

  either
    (const $ fail err)
    (pure . Just)
    (asxml <|> asemail <|> astxt)

emailFromSAML :: HasCallStack => SAMLEmail.Email -> Email
emailFromSAML = fromJust . parseEmail . SAMLEmail.render

emailToSAML :: HasCallStack => Email -> SAMLEmail.Email
emailToSAML = CI.original . fromRight (error "emailToSAML") . SAMLEmail.validate . toByteString

-- | FUTUREWORK(fisx): if saml2-web-sso exported the 'NameID' constructor, we could make this
-- function total without all that praying and hoping.
emailToSAMLNameID :: HasCallStack => Email -> SAML.NameID
emailToSAMLNameID = fromRight (error "impossible") . SAML.emailNameID . fromEmail

emailFromSAMLNameID :: HasCallStack => SAML.NameID -> Maybe Email
emailFromSAMLNameID nid = case nid ^. SAML.nameID of
  SAML.UNameIDEmail email -> Just . emailFromSAML . CI.original $ email
  _ -> Nothing

-- | For testing.  Create a sample 'SAML.UserRef' value with random seeds to make 'Issuer' and
-- 'NameID' unique.  FUTUREWORK: move to saml2-web-sso.
mkSampleUref :: Text -> Text -> SAML.UserRef
mkSampleUref iseed nseed = SAML.UserRef issuer nameid
  where
    issuer :: SAML.Issuer
    issuer = SAML.Issuer ([uri|http://example.com/|] & URI.pathL .~ cs ("/" </> cs iseed))

    nameid :: SAML.NameID
    nameid = fromRight (error "impossible") $ do
      unqualified <- SAML.mkUNameIDEmail $ "me" <> nseed <> "@example.com"
      SAML.mkNameID unqualified Nothing Nothing Nothing

-- | @mkSampleUref "" ""@
mkSimpleSampleUref :: SAML.UserRef
mkSimpleSampleUref = mkSampleUref "" ""
