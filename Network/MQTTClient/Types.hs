module Network.MQTTClient.Types where

import           Data.ByteString         as BS
import qualified Data.Text               as T
import           Data.Word               (Word8, Word16)

data QoS =
  -- | The message is delivered according to the capabilities of the underlying network.
  -- | No response is sent by the receiver and no retry is performed by the sender.
  -- | The message arrives at the receiver either once or not at all.
    AtMostOnce
  -- | This quality of service ensures that the message arrives at the receiver at least once.
  | AtLeastOnce
  -- | This is the highest quality of service, for use when neither loss nor duplication of messages are acceptable.
  -- | There is an increased overhead associated with this quality of service.
  | ExactlyOnce deriving (Eq, Show)

newtype PacketIdentifier = PacketIdentifier Word16 deriving (Eq, Ord, Show)

newtype ProtocolLevel = ProtocolLevel Int

data ConnectFlags = ConnectFlags
  { _connectUserNameFlag :: Bool
  , _connectPasswordFlag  :: Bool
  , _connectWillRetain    :: Bool
  , _connectWillQoS       :: QoS
  , _connectWillFlag      :: Bool
  , _connectCleanSession  :: Bool
  } deriving (Eq, Show)

newtype MQTTTopic = MQTTTopic { _unMQTTTopic :: T.Text } deriving (Eq, Show)

data ConnectPayload = ConnectPayload
  {
  -- | 3.1.3.1 Client identifier
    _connectPayloadClientId    :: MQTTClientId
  -- | 3.1.3.2 Will Topic
  , _connectPayloadWillTopic   :: Maybe MQTTTopic
  -- | 3.1.3.3 Will Message
  , _connectPayloadWillMessage :: Maybe T.Text
  -- | 3.1.3.4 User Name
  , _connectPayloadUserName    :: Maybe T.Text
  -- | 3.1.3.5 Password
  , _connectPayloadPassword    :: Maybe T.Text
  } deriving (Eq, Show)

-- | KeepAlive is a time interval measured in seconds.
newtype KeepAlive = KeepAlive { _unKeepAlive :: Word16 } deriving (Eq, Show)

newtype MQTTClientId = MQTTClientId { _unMQTTClientId ::T.Text } deriving (Eq, Show)

type RemainingLength = Word8

-- | 3.2.2.3 Connect Return code
data ConnectReturnCode =
    ConnectionAccepted
  -- | The Server does not support the level of the MQTT protocol requested by the Client
  | UnacceptableProtocol
  -- | The Client identifier is correct UTF-8 but not allowed by the Server.
  | IdentifierRejected
  -- | The Network Connection has been made but the MQTT service is unavailable.
  | ServerUnavailable
  -- | The data in the user name or password is malformed.
  | BadUserNameOrPassword
  -- | The Client is not authorized to connect.
  | NotAuthorized
  deriving (Eq, Show)

data ControlPacket =
  -- | Client requests a connection to a server
    CONNECT
    { _connectKeepAlive        :: KeepAlive
    , _connectConnectFlags     :: ConnectFlags
    , _connectPayload          :: ConnectPayload
    }
  -- | Acknowledge connection request
  | CONNACK
    { -- | The Session Present flag enables a Client to establish whether the Client and Server
    -- | have a consistent view about whether there is already stored Session state.
    _connackSessionPresent :: Bool
    -- | 3.2.2.3
  , _connackReturnCode     :: ConnectReturnCode
    }
  -- | Publish message
  | PUBLISH
    { -- fixed header information
      _publishFlagsDUP         :: Bool
    , _publishFlagsQoS         :: QoS
    , _publishFlagsRetain      :: Bool
      -- var header information
    , _publishPacketIdentifier :: Maybe PacketIdentifier
    , _publishTopic            :: T.Text
   -- | 3.3.3 The Payload contains the Application Message that is being published.
   -- | The content and format of the data is application specific.
    , _publicPayload           :: BS.ByteString
    }
  -- | Publish acknowledgement
  | PUBACK
  -- | Publish received (assured delivery part 1)
  | PUBREC
  -- | Public release (assured delivery part 2)
  | PUBREL
  -- | Publish complete (assured delivery part 3)
  | PUBCOMP
  -- | Publish subscribe to topics
  | SUBSCRIBE
    { _subscribePacketIdentifier :: PacketIdentifier
    , _subscribeTopicFilter :: [(MQTTTopic, QoS)]
    }
  -- | Subscribe acknowledgement
  | SUBACK
    { _subackPacketIdentifier :: PacketIdentifier
    }
  -- | Unsubscribe from topics
  | UNSUBSCRIBE
    { _unsubscribePacketIdentifier :: PacketIdentifier
    }
  -- | Unbsubscribe acknowledgement
  | UNSUBACK
  -- | PING request
  | PINGREQ
  -- | PING response
  | PINGRESP
  -- | Disconnect notification
  | DISCONNECT
  deriving (Eq, Show)
