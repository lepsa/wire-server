{-# LANGUAGE TemplateHaskell #-}

module Wire.API.Event.Federation
  ( Event (..),
    EventData (..),
  )
where

import Control.Arrow ((&&&))
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Domain
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Schema
import Data.Swagger qualified as S
import Imports
import Test.QuickCheck.Gen
import Wire.Arbitrary

data Event = Event
  { _eventData :: EventData
  }
  deriving (Eq, Show, Generic)

data EventData
  = FederationDelete Domain
  | FederationConnectionRemoved (Domain, Domain)
  deriving stock (Eq, Show, Generic)

$(makePrisms ''EventData)

instance Arbitrary EventData where
  arbitrary =
    oneof
      [ FederationDelete <$> arbitrary,
        FederationConnectionRemoved <$> arbitrary
      ]

instance Arbitrary Event where
  arbitrary = Event <$> arbitrary

data EventType
  = FederationTypeDelete
  | FederationTypeConnectionRemoved
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform EventType)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema EventType

instance ToSchema EventType where
  schema =
    enum @Text "FederationEventType" $
      mconcat
        [ element "federation.delete" FederationTypeDelete,
          element "federation.connectionRemoved" FederationTypeConnectionRemoved
        ]

eventType :: Event -> EventType
eventType = eventDataType . _eventData

eventDataType :: EventData -> EventType
eventDataType (FederationDelete _) = FederationTypeDelete
eventDataType (FederationConnectionRemoved _) = FederationTypeConnectionRemoved

taggedEventDataSchema :: ObjectSchema SwaggerDoc (EventType, EventData)
taggedEventDataSchema =
  bind
    (fst .= field "type" schema)
    -- The fields we need to look at change based on the event
    -- type, so we need to dispatch here to get monadic-ish behaviour.
    --
    -- federation.delete is expecting a "domain" field that contains a bare domain string.
    -- federation.connectionRemoved is expecting a "doamins" field that contains exactly a pair of domains in a list
    ( snd .= dispatch dataSchema
    )
  where
    dataSchema :: EventType -> ObjectSchema SwaggerDoc EventData
    dataSchema FederationTypeDelete = tag _FederationDelete deleteSchema
    dataSchema FederationTypeConnectionRemoved = tag _FederationConnectionRemoved connectionRemovedSchema

-- These schemas have different fields they are targeting.
deleteSchema :: ObjectSchema SwaggerDoc Domain
deleteSchema = field "domain" schema

connectionRemovedSchema :: ObjectSchema SwaggerDoc (Domain, Domain)
connectionRemovedSchema = field "domains" (pair schema)

-- Schemas for the events, as they have different structures.
eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  Event . snd
    <$> (eventType &&& _eventData) .= taggedEventDataSchema

instance ToSchema Event where
  schema = object "Event" eventObjectSchema

instance ToJSONObject Event where
  toJSONObject =
    KeyMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger

instance FromJSON Event where
  parseJSON = schemaParseJSON

instance ToJSON Event where
  toJSON = schemaToJSON
