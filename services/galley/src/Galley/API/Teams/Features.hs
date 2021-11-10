-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.Teams.Features
  ( getFeatureStatus,
    setFeatureStatus,
    getFeatureConfig,
    getAllFeatureConfigs,
    getAllFeaturesH,
    getSSOStatusInternal,
    setSSOStatusInternal,
    getLegalholdStatusInternal,
    setLegalholdStatusInternal,
    getTeamSearchVisibilityAvailableInternal,
    setTeamSearchVisibilityAvailableInternal,
    getValidateSAMLEmailsInternal,
    setValidateSAMLEmailsInternal,
    getDigitalSignaturesInternal,
    setDigitalSignaturesInternal,
    getClassifiedDomainsInternal,
    getAppLockInternal,
    setAppLockInternal,
    getFileSharingInternal,
    setFileSharingInternal,
    getConferenceCallingInternal,
    setConferenceCallingInternal,
    getSelfDeletingMessagesInternal,
    setSelfDeletingMessagesInternal,
    DoAuth (..),
    GetFeatureInternalParam,
  )
where

import Control.Lens
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion hiding (fromList)
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions (cs)
import Galley.API.Error as Galley
import Galley.API.LegalHold
import Galley.API.Teams (ensureNotTooLargeToActivateLegalHold)
import Galley.API.Util
import Galley.App
import Galley.Cassandra.Paging
import Galley.Data.TeamFeatures
import Galley.Effects
import Galley.Effects.GundeckAccess
import Galley.Effects.Paging
import qualified Galley.Effects.SearchVisibilityStore as SearchVisibilityData
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Effects.TeamStore
import Galley.Intra.Push (PushEvent (FeatureConfigEvent), newPush)
import Galley.Options
import Galley.Types.Teams hiding (newTeam)
import Imports
import Network.HTTP.Client (Manager)
import Network.Wai
import Network.Wai.Predicate hiding (Error, or, result, setStatus)
import Network.Wai.Utilities hiding (Error)
import Polysemy.Error
import Servant.API ((:<|>) ((:<|>)))
import qualified Servant.Client as Client
import qualified System.Logger.Class as Log
import Util.Options (Endpoint, epHost, epPort)
import Wire.API.ErrorDescription
import Wire.API.Event.FeatureConfig
import qualified Wire.API.Event.FeatureConfig as Event
import Wire.API.Federation.Client
import qualified Wire.API.Routes.Internal.Brig as IAPI
import Wire.API.Team.Feature (AllFeatureConfigs (..), FeatureHasNoConfig, KnownTeamFeatureName, TeamFeatureName)
import qualified Wire.API.Team.Feature as Public

data DoAuth = DoAuth UserId | DontDoAuth

-- | For team-settings, to administrate team feature configuration.  Here we have an admin uid
-- and a team id, but no uid of the member for which the feature config holds.
getFeatureStatus ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    Members
      '[ Error ActionError,
         Error TeamError,
         Error NotATeamMember,
         TeamStore
       ]
      r
  ) =>
  (GetFeatureInternalParam -> Galley r (Public.TeamFeatureStatus a)) ->
  DoAuth ->
  TeamId ->
  Galley r (Public.TeamFeatureStatus a)
getFeatureStatus getter doauth tid = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- liftSem $ getTeamMember tid uid
      void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  getter (Right tid)

-- | For team-settings, like 'getFeatureStatus'.
setFeatureStatus ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    Members
      '[ Error ActionError,
         Error TeamError,
         Error NotATeamMember,
         TeamStore
       ]
      r
  ) =>
  (TeamId -> Public.TeamFeatureStatus a -> Galley r (Public.TeamFeatureStatus a)) ->
  DoAuth ->
  TeamId ->
  Public.TeamFeatureStatus a ->
  Galley r (Public.TeamFeatureStatus a)
setFeatureStatus setter doauth tid status = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- liftSem $ getTeamMember tid uid
      void $ permissionCheck (ChangeTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  setter tid status

-- | For individual users to get feature config for their account (personal or team).
getFeatureConfig ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    Members
      '[ Error ActionError,
         Error TeamError,
         Error NotATeamMember,
         TeamStore
       ]
      r
  ) =>
  (GetFeatureInternalParam -> Galley r (Public.TeamFeatureStatus a)) ->
  UserId ->
  Galley r (Public.TeamFeatureStatus a)
getFeatureConfig getter zusr = do
  mbTeam <- liftSem $ getOneUserTeam zusr
  case mbTeam of
    Nothing -> getter (Left (Just zusr))
    Just tid -> do
      zusrMembership <- liftSem $ getTeamMember tid zusr
      void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
      assertTeamExists tid
      getter (Right tid)

getAllFeatureConfigs ::
  Members
    '[ Error ActionError,
       Error InternalError,
       Error NotATeamMember,
       Error TeamError,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore
     ]
    r =>
  UserId ->
  Galley r AllFeatureConfigs
getAllFeatureConfigs zusr = do
  mbTeam <- liftSem $ getOneUserTeam zusr
  zusrMembership <- maybe (pure Nothing) (liftSem . (flip getTeamMember zusr)) mbTeam
  let getStatus ::
        forall (a :: Public.TeamFeatureName) r.
        ( Public.KnownTeamFeatureName a,
          Aeson.ToJSON (Public.TeamFeatureStatus a),
          Members '[Error ActionError, Error TeamError, Error NotATeamMember, TeamStore] r
        ) =>
        (GetFeatureInternalParam -> Galley r (Public.TeamFeatureStatus a)) ->
        Galley r (Text, Aeson.Value)
      getStatus getter = do
        when (isJust mbTeam) $ do
          void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
        status <- getter (maybe (Left (Just zusr)) Right mbTeam)
        let feature = Public.knownTeamFeatureName @a
        pure $ (cs (toByteString' feature) Aeson..= status)

  AllFeatureConfigs . HashMap.fromList
    <$> sequence
      [ getStatus @'Public.TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'Public.TeamFeatureSSO getSSOStatusInternal,
        getStatus @'Public.TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'Public.TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'Public.TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'Public.TeamFeatureAppLock getAppLockInternal,
        getStatus @'Public.TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'Public.TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'Public.TeamFeatureConferenceCalling getConferenceCallingInternal,
        getStatus @'Public.TeamFeatureSelfDeletingMessages getSelfDeletingMessagesInternal
      ]

getAllFeaturesH ::
  Members
    '[ Error ActionError,
       Error InternalError,
       Error TeamError,
       Error NotATeamMember,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore
     ]
    r =>
  UserId ::: TeamId ::: JSON ->
  Galley r Response
getAllFeaturesH (uid ::: tid ::: _) =
  json <$> getAllFeatures uid tid

getAllFeatures ::
  forall r.
  Members
    '[ Error ActionError,
       Error InternalError,
       Error TeamError,
       Error NotATeamMember,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore
     ]
    r =>
  UserId ->
  TeamId ->
  Galley r Aeson.Value
getAllFeatures uid tid = do
  Aeson.object
    <$> sequence
      [ getStatus @'Public.TeamFeatureSSO getSSOStatusInternal,
        getStatus @'Public.TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'Public.TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'Public.TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'Public.TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'Public.TeamFeatureAppLock getAppLockInternal,
        getStatus @'Public.TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'Public.TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'Public.TeamFeatureConferenceCalling getConferenceCallingInternal,
        getStatus @'Public.TeamFeatureSelfDeletingMessages getSelfDeletingMessagesInternal
      ]
  where
    getStatus ::
      forall (a :: Public.TeamFeatureName).
      ( Public.KnownTeamFeatureName a,
        Aeson.ToJSON (Public.TeamFeatureStatus a)
      ) =>
      (GetFeatureInternalParam -> Galley r (Public.TeamFeatureStatus a)) ->
      Galley r (Text, Aeson.Value)
    getStatus getter = do
      status <- getFeatureStatus @a getter (DoAuth uid) tid
      let feature = Public.knownTeamFeatureName @a
      pure $ (cs (toByteString' feature) Aeson..= status)

getFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.FeatureHasNoConfig a,
    HasStatusCol a,
    Member TeamFeatureStore r
  ) =>
  Galley r Public.TeamFeatureStatusValue ->
  TeamId ->
  Galley r (Public.TeamFeatureStatus a)
getFeatureStatusNoConfig getDefault tid = do
  defaultStatus <- Public.TeamFeatureStatusNoConfig <$> getDefault
  liftSem $ fromMaybe defaultStatus <$> TeamFeatures.getFeatureStatusNoConfig @a tid

setFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    Public.FeatureHasNoConfig a,
    HasStatusCol a,
    Members '[GundeckAccess, TeamFeatureStore, TeamStore] r
  ) =>
  (Public.TeamFeatureStatusValue -> TeamId -> Galley r ()) ->
  TeamId ->
  Public.TeamFeatureStatus a ->
  Galley r (Public.TeamFeatureStatus a)
setFeatureStatusNoConfig applyState tid status = do
  applyState (Public.tfwoStatus status) tid
  newStatus <- liftSem $ TeamFeatures.setFeatureStatusNoConfig @a tid status
  pushFeatureConfigEvent tid $
    Event.Event Event.Update (Public.knownTeamFeatureName @a) (EdFeatureWithoutConfigChanged newStatus)
  pure newStatus

-- | FUTUREWORK(fisx): (thanks pcapriotti) this should probably be a type family dependent on
-- the feature flag, so that we get more type safety.
type GetFeatureInternalParam = Either (Maybe UserId) TeamId

getSSOStatusInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureSSO)
getSSOStatusInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureSSO getDef)
  where
    getDef :: Galley r Public.TeamFeatureStatusValue
    getDef =
      view (options . optSettings . setFeatureFlags . flagSSO) <&> \case
        FeatureSSOEnabledByDefault -> Public.TeamFeatureEnabled
        FeatureSSODisabledByDefault -> Public.TeamFeatureDisabled

setSSOStatusInternal ::
  Members '[Error TeamFeatureError, GundeckAccess, TeamFeatureStore, TeamStore] r =>
  TeamId ->
  (Public.TeamFeatureStatus 'Public.TeamFeatureSSO) ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureSSO)
setSSOStatusInternal = setFeatureStatusNoConfig @'Public.TeamFeatureSSO $ \case
  Public.TeamFeatureDisabled -> const (liftSem (throw DisableSsoNotImplemented))
  Public.TeamFeatureEnabled -> const (pure ())

getTeamSearchVisibilityAvailableInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility)
getTeamSearchVisibilityAvailableInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility getDef)
  where
    getDef = do
      view (options . optSettings . setFeatureFlags . flagTeamSearchVisibility) <&> \case
        FeatureTeamSearchVisibilityEnabledByDefault -> Public.TeamFeatureEnabled
        FeatureTeamSearchVisibilityDisabledByDefault -> Public.TeamFeatureDisabled

setTeamSearchVisibilityAvailableInternal ::
  Members '[GundeckAccess, SearchVisibilityStore, TeamFeatureStore, TeamStore] r =>
  TeamId ->
  (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility) ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility)
setTeamSearchVisibilityAvailableInternal = setFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility $ \case
  Public.TeamFeatureDisabled -> liftSem . SearchVisibilityData.resetSearchVisibility
  Public.TeamFeatureEnabled -> const (pure ())

getValidateSAMLEmailsInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails)
getValidateSAMLEmailsInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureValidateSAMLEmails getDef)
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure Public.TeamFeatureDisabled

setValidateSAMLEmailsInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore] r =>
  TeamId ->
  (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails) ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails)
setValidateSAMLEmailsInternal = setFeatureStatusNoConfig @'Public.TeamFeatureValidateSAMLEmails $ \_ _ -> pure ()

getDigitalSignaturesInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures)
getDigitalSignaturesInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureDigitalSignatures getDef)
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure Public.TeamFeatureDisabled

setDigitalSignaturesInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures)
setDigitalSignaturesInternal = setFeatureStatusNoConfig @'Public.TeamFeatureDigitalSignatures $ \_ _ -> pure ()

getLegalholdStatusInternal ::
  Members '[LegalHoldStore, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold)
getLegalholdStatusInternal (Left _) =
  pure $ Public.TeamFeatureStatusNoConfig Public.TeamFeatureDisabled
getLegalholdStatusInternal (Right tid) = do
  isLegalHoldEnabledForTeam tid <&> \case
    True -> Public.TeamFeatureStatusNoConfig Public.TeamFeatureEnabled
    False -> Public.TeamFeatureStatusNoConfig Public.TeamFeatureDisabled

setLegalholdStatusInternal ::
  ( Paging p,
    Bounded (PagingBounds p TeamMember),
    Members
      '[ BotAccess,
         BrigAccess,
         CodeStore,
         ConversationStore,
         Error ActionError,
         Error AuthenticationError,
         Error ConversationError,
         Error FederationError,
         Error InvalidInput,
         Error LegalHoldError,
         Error TeamError,
         Error NotATeamMember,
         Error TeamFeatureError,
         ExternalAccess,
         FederatorAccess,
         FireAndForget,
         GundeckAccess,
         LegalHoldStore,
         ListItems LegacyPaging ConvId,
         MemberStore,
         TeamFeatureStore,
         TeamStore,
         TeamMemberStore p
       ]
      r
  ) =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold)
setLegalholdStatusInternal tid status@(Public.tfwoStatus -> statusValue) = do
  do
    -- this extra do is to encapsulate the assertions running before the actual operation.
    -- enabeling LH for teams is only allowed in normal operation; disabled-permanently and
    -- whitelist-teams have no or their own way to do that, resp.
    featureLegalHold <- view (options . optSettings . setFeatureFlags . flagLegalHold)
    liftSem $ case featureLegalHold of
      FeatureLegalHoldDisabledByDefault -> do
        pure ()
      FeatureLegalHoldDisabledPermanently -> do
        throw LegalHoldFeatureFlagNotEnabled
      FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
        throw LegalHoldWhitelistedOnly

  -- we're good to update the status now.
  case statusValue of
    Public.TeamFeatureDisabled -> removeSettings' tid
    Public.TeamFeatureEnabled -> do
      ensureNotTooLargeToActivateLegalHold tid
  liftSem $ TeamFeatures.setFeatureStatusNoConfig @'Public.TeamFeatureLegalHold tid status

getFileSharingInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureFileSharing)
getFileSharingInternal =
  getFeatureStatusWithDefaultConfig @'Public.TeamFeatureFileSharing flagFileSharing . either (const Nothing) Just

getFeatureStatusWithDefaultConfig ::
  forall (a :: TeamFeatureName) r.
  (KnownTeamFeatureName a, HasStatusCol a, FeatureHasNoConfig a, Member TeamFeatureStore r) =>
  Lens' FeatureFlags (Defaults (Public.TeamFeatureStatus a)) ->
  Maybe TeamId ->
  Galley r (Public.TeamFeatureStatus a)
getFeatureStatusWithDefaultConfig lens' =
  maybe
    (Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @a getDef)
  where
    getDef :: Galley r Public.TeamFeatureStatusValue
    getDef =
      view (options . optSettings . setFeatureFlags . lens')
        <&> Public.tfwoStatus . view unDefaults

setFileSharingInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.TeamFeatureFileSharing ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureFileSharing)
setFileSharingInternal = setFeatureStatusNoConfig @'Public.TeamFeatureFileSharing $ \_status _tid -> pure ()

getAppLockInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureAppLock)
getAppLockInternal mbtid = do
  Defaults defaultStatus <- view (options . optSettings . setFeatureFlags . flagAppLockDefaults)
  status <-
    liftSem $
      join <$> (TeamFeatures.getApplockFeatureStatus `mapM` either (const Nothing) Just mbtid)
  pure $ fromMaybe defaultStatus status

setAppLockInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, Error TeamFeatureError] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.TeamFeatureAppLock ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureAppLock)
setAppLockInternal tid status = do
  when (Public.applockInactivityTimeoutSecs (Public.tfwcConfig status) < 30) $
    liftSem $ throw AppLockinactivityTimeoutTooLow
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event Event.Update Public.TeamFeatureAppLock (EdFeatureApplockChanged status)
  (liftSem $ TeamFeatures.setApplockFeatureStatus tid status) <* pushEvent

getClassifiedDomainsInternal :: GetFeatureInternalParam -> Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains)
getClassifiedDomainsInternal _mbtid = do
  globalConfig <- view (options . optSettings . setFeatureFlags . flagClassifiedDomains)
  let config = globalConfig
  pure $ case Public.tfwcStatus config of
    Public.TeamFeatureDisabled ->
      Public.TeamFeatureStatusWithConfig Public.TeamFeatureDisabled (Public.TeamFeatureClassifiedDomainsConfig [])
    Public.TeamFeatureEnabled -> config

getConferenceCallingInternal ::
  Members '[Error InternalError, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureConferenceCalling)
getConferenceCallingInternal (Left (Just uid)) = do
  getFeatureConfigViaAccount @'Public.TeamFeatureConferenceCalling uid
getConferenceCallingInternal (Left Nothing) = do
  getFeatureStatusWithDefaultConfig @'Public.TeamFeatureConferenceCalling flagConferenceCalling Nothing
getConferenceCallingInternal (Right tid) = do
  getFeatureStatusWithDefaultConfig @'Public.TeamFeatureConferenceCalling flagConferenceCalling (Just tid)

setConferenceCallingInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.TeamFeatureConferenceCalling ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureConferenceCalling)
setConferenceCallingInternal =
  setFeatureStatusNoConfig @'Public.TeamFeatureConferenceCalling $ \_status _tid -> pure ()

getSelfDeletingMessagesInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages)
getSelfDeletingMessagesInternal = \case
  Left _ -> pure Public.defaultSelfDeletingMessagesStatus
  Right tid ->
    liftSem $
      TeamFeatures.getSelfDeletingMessagesStatus tid
        <&> maybe Public.defaultSelfDeletingMessagesStatus id

setSelfDeletingMessagesInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages ->
  Galley r (Public.TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages)
setSelfDeletingMessagesInternal tid st = do
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event Event.Update Public.TeamFeatureSelfDeletingMessages (EdFeatureSelfDeletingMessagesChanged st)
  (liftSem $ TeamFeatures.setSelfDeletingMessagesStatus tid st) <* pushEvent

pushFeatureConfigEvent ::
  Members '[GundeckAccess, TeamStore] r =>
  TeamId ->
  Event.Event ->
  Galley r ()
pushFeatureConfigEvent tid event = do
  memList <- getTeamMembersForFanout tid
  when ((memList ^. teamMemberListType) == ListTruncated) $ do
    Log.warn $
      Log.field "action" (Log.val "Features.pushFeatureConfigEvent")
        . Log.field "feature" (Log.val (toByteString' . Event._eventFeatureName $ event))
        . Log.field "team" (Log.val (cs . show $ tid))
        . Log.msg @Text "Fanout limit exceeded. Some events will not be sent."
  let recipients = membersToRecipients Nothing (memList ^. teamMembers)
  for_
    (newPush (memList ^. teamMemberListType) Nothing (FeatureConfigEvent event) recipients)
    (liftSem . push1)

-- | (Currently, we only have 'Public.TeamFeatureConferenceCalling' here, but we may have to
-- extend this in the future.)
getFeatureConfigViaAccount ::
  (flag ~ 'Public.TeamFeatureConferenceCalling, Member (Error InternalError) r) =>
  UserId ->
  Galley r (Public.TeamFeatureStatus flag)
getFeatureConfigViaAccount uid = do
  mgr <- asks (^. manager)
  brigep <- asks (^. brig)
  getAccountFeatureConfigClient brigep mgr uid >>= handleResp
  where
    handleResp ::
      Member (Error InternalError) r =>
      Either Client.ClientError Public.TeamFeatureStatusNoConfig ->
      Galley r Public.TeamFeatureStatusNoConfig
    handleResp (Right cfg) = pure cfg
    handleResp (Left errmsg) = liftSem . throw . InternalErrorWithDescription . cs . show $ errmsg

    getAccountFeatureConfigClient ::
      (HasCallStack, MonadIO m) =>
      Endpoint ->
      Manager ->
      UserId ->
      m (Either Client.ClientError Public.TeamFeatureStatusNoConfig)
    getAccountFeatureConfigClient brigep mgr = runHereClientM brigep mgr . getAccountFeatureConfigClientM

    getAccountFeatureConfigClientM ::
      UserId -> Client.ClientM Public.TeamFeatureStatusNoConfig
    ( _
        :<|> getAccountFeatureConfigClientM
        :<|> _
        :<|> _
      ) = Client.client (Proxy @IAPI.API)

    runHereClientM ::
      (HasCallStack, MonadIO m) =>
      Endpoint ->
      Manager ->
      Client.ClientM a ->
      m (Either Client.ClientError a)
    runHereClientM brigep mgr action = do
      let env = Client.mkClientEnv mgr baseurl
          baseurl = Client.BaseUrl Client.Http (cs $ brigep ^. epHost) (fromIntegral $ brigep ^. epPort) ""
      liftIO $ Client.runClientM action env
