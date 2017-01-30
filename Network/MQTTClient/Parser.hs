{-# LANGUAGE DataKinds #-}
module Network.MQTTClient.Parser where

import           Data.Bits               ((.|.), shiftL)
import           Data.ByteString         (ByteString(..))
import           Data.ByteString.Builder
import           Data.Default            (def)
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
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

willMessage :: T.Text -> Builder
willMessage t = word16BE 0 <> encodeUtf8 t

newtype MQTTTopic = MQTTTopic { _unMQTTTopic :: T.Text } deriving (Eq, Show)

data ConnectPayload = ConnectPayload
  {
  -- | 3.1.3.1 Client identifier
    _connectPayloadClientId    :: MQTTClientId
  -- | 3.1.3.2 Will Topic
  , _connectPayloadWillTopic   :: MQTTTopic
  -- | 3.1.3.3 Will Message
  , _connectPayloadWillMessage :: T.Text
  -- | 3.1.3.4 User Name
  , _connectPayloadUserName    :: T.Text
  -- | 3.1.3.5 Password
  , _connectPayloadPassword    :: T.Text
  } deriving (Eq, Show)

willTopic :: Builder
willTopic = word8 0

protocolName :: Builder
protocolName =
     word8 (0x0 :: Word8)
  <> word8 (0x4  :: Word8)
  <> word8 (0x4D :: Word8)
  <> word8 (0x51 :: Word8)
  <> word8 (0x54 :: Word8)
  <> word8 (0x54 :: Word8)

protocolLevel_3_1_1 :: Builder
protocolLevel_3_1_1 = word8 0x04

connectFlags :: ConnectFlags -> Builder
connectFlags (ConnectFlags userName pass retain qos will clean) =
  let
    userName' = shiftL (boolWord userName) 7
    pass'     = shiftL (boolWord pass) 6
    retain'   = shiftL (boolWord retain) 5
    qos'      = shiftL (qosWord qos) 3
    will'     = shiftL (boolWord will) 2
    clean'    = shiftL (boolWord clean) 1
  in word8 $ userName' .|. pass' .|. retain' .|. qos' .|. will' .|. clean'

-- | KeepAlive is a time interval measured in seconds.
newtype KeepAlive = KeepAlive { _unKeepAlive :: Word16 } deriving (Eq, Show)

newtype MQTTClientId = MQTTClientId { _unMQTTClientId :: Word16 } deriving (Eq, Show)

keepAlive :: KeepAlive -> Builder
keepAlive (KeepAlive a) = word16BE a

data ControlPacket =
  -- | Client requests a connection to a server
    CONNECT
    { _connectPacketIdentifier :: PacketIdentifier
    , _connectKeepAlive        :: KeepAlive
    , _connectConnectFlags     :: ConnectFlags
    , _connectPayload          :: ConnectPayload
    }
  -- | Acknowledge connection request
  | CONNACK {}
  -- | Publish message
  | PUBLISH
    { -- fixed header information
      _publishFlagsDUP         :: Bool
    , _publishFlagsQoS         :: QoS
    , _publishFlagsRetain      :: Bool
      -- var header information
    , _publishPacketIdentifier :: Maybe PacketIdentifier
    , _publishTopic            :: T.Text
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

type RemainingLength = Word8

-- | 1.5.3 UTF-8 encoded strings
encodeUtf8 :: T.Text -> Builder
encodeUtf8 t =
  let len = fromIntegral $ T.length t :: Word16
  in word16BE len <> TE.encodeUtf8Builder t

mqttClientId :: MQTTClientId -> Builder
mqttClientId (MQTTClientId m)= word16BE m

-- | 2.2 Fixed header
fixedHeader :: ControlPacket -> RemainingLength -> Builder
fixedHeader cp rl =
  let type'      = shiftL (controlPacketTypeWord cp) 4
      typeFlags' = fixedHeaderFlags cp
  in word8 (type' .|. typeFlags') <> word8 rl

-- | 2.2 Fixed header - Flags specific to each MQTT Control Packet type
fixedHeaderFlags :: ControlPacket -> Word8
fixedHeaderFlags CONNECT {} = 0
 -- for most this is currently "reserved" defined in 2.2.2
fixedHeaderFlags CONNACK     = 0
fixedHeaderFlags (PUBLISH dup qos retain _ _) =
    let retain' = boolWord retain
        qos'    = shiftL (qosWord qos) 1
        dup'    = shiftL (boolWord dup) 3
    in (dup' .|. qos' .|. retain')
fixedHeaderFlags PUBACK         = 0
fixedHeaderFlags PUBREC         = 0
fixedHeaderFlags PUBREL         = 2
fixedHeaderFlags PUBCOMP        = 0
fixedHeaderFlags SUBSCRIBE {}   = 2
fixedHeaderFlags SUBACK {}      = 0
fixedHeaderFlags UNSUBSCRIBE {} = 2
fixedHeaderFlags UNSUBACK       = 0
fixedHeaderFlags PINGREQ        = 0
fixedHeaderFlags PINGRESP       = 0
fixedHeaderFlags DISCONNECT     = 0

varHeader :: ControlPacket -> Builder
-- | 3.2.2
varHeader (CONNECT identifier keepAlive' flags _) =
     protocolName
  <> protocolLevel_3_1_1
  <> connectFlags flags
  <> keepAlive keepAlive'
varHeader CONNACK        = word8 0
varHeader (PUBLISH _ _ _ packetIdentifier topic) = word8 0
varHeader PUBACK         = word8 0
varHeader PUBREC         = word8 0
varHeader PUBREL         = word8 2
varHeader PUBCOMP        = word8 0
varHeader SUBSCRIBE {}   = word8 2
varHeader SUBACK {}      = word8 0
varHeader UNSUBSCRIBE {} = word8 2
varHeader UNSUBACK       = word8 0
varHeader PINGREQ        = word8 0
varHeader PINGRESP       = word8 0
varHeader DISCONNECT     = word8 0

payload :: ControlPacket -> Builder
-- | 3.2.2
payload (CONNECT _ _ _ payload) =
  let ci   = mqttClientId $ _connectPayloadClientId payload :: Builder
      wt   = encodeUtf8   $ _unMQTTTopic $ _connectPayloadWillTopic payload  :: Builder
      wm   = willMessage  $ _connectPayloadWillMessage payload :: Builder
      un   = encodeUtf8   $ _connectPayloadUserName payload :: Builder
      pass = encodeUtf8   $ _connectPayloadPassword payload :: Builder
  in  ci <> wt <> wm <> un <> pass
payload CONNACK        = word8 0
payload (PUBLISH _ _ _ packetIdentifier topic) = word8 0
payload PUBACK         = word8 0
payload PUBREC         = word8 0
payload PUBREL         = word8 0
payload PUBCOMP        = word8 0
payload SUBSCRIBE {}   = word8 0
payload SUBACK {}      = word8 0
payload UNSUBSCRIBE {} = word8 0
payload UNSUBACK       = word8 0
payload PINGREQ        = word8 0
payload PINGRESP       = word8 0
payload DISCONNECT     = word8 0

-- | encode remaining length using variable length encoding defined in 2.2.3
encodeRemainingLength :: Word -> Builder
encodeRemainingLength a =
  let
    b = fromIntegral a
    (quot', rem') = quotRem b 128
  in if quot' > 0
    then word8 (rem' .|. 128) <> encodeRemainingLength a
    else word8 rem'

boolWord :: Bool -> Word8
boolWord False = 0
boolWord True = 1

-- | Quality of Service as defined in 4.3
qosWord :: QoS -> Word8
qosWord AtMostOnce  = 0
qosWord AtLeastOnce = 1
qosWord ExactlyOnce = 2

-- | Control packet types as defined in 2.2.2
controlPacketTypeWord :: ControlPacket -> Word8
controlPacketTypeWord CONNECT {}     = 1
controlPacketTypeWord CONNACK        = 2
controlPacketTypeWord PUBLISH {}     = 3
controlPacketTypeWord PUBACK         = 4
controlPacketTypeWord PUBREC         = 5
controlPacketTypeWord PUBREL         = 6
controlPacketTypeWord PUBCOMP        = 7
controlPacketTypeWord SUBSCRIBE {}   = 8
controlPacketTypeWord SUBACK {}      = 9
controlPacketTypeWord UNSUBSCRIBE {} = 10
controlPacketTypeWord UNSUBACK       = 11
controlPacketTypeWord PINGREQ        = 12
controlPacketTypeWord PINGRESP       = 13
controlPacketTypeWord DISCONNECT     = 14
