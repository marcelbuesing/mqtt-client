{-# LANGUAGE DataKinds #-}
module Network.MQTTClient.Parser where

import           Data.Bits               ((.|.), shiftL)
import           Data.ByteString         (ByteString(..))
import           Data.ByteString.Builder
import           Data.Default            (def)
import           Data.Monoid
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

data ControlPacket =
  -- | Client requests a connection to a server
    CONNECT
    { _connectPacketIdentifier :: PacketIdentifier
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

encodeIdentifier :: Word16
encodeIdentifier = undefined

-- | 2.2 Fixed header
fixedHeader :: ControlPacket -> RemainingLength -> Builder
fixedHeader cp rl =
  let
    type'      = shiftL (controlPacketTypeWord cp) 4
    typeFlags' = fixedHeaderFlags cp
    byte1      = word8 (type' .|. typeFlags')
    byte2      = word8 rl
  in byte1 <> byte2

-- | 2.2 Fixed header - Flags specific to each MQTT Control Packet type
fixedHeaderFlags :: ControlPacket -> Word8
 -- for all others this is currently "reserved" defined in 2.2.2
fixedHeaderFlags CONNECT {}  = 0
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
varHeader CONNECT {}     = word8 0
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
