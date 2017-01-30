{-# LANGUAGE DataKinds #-}
module Network.MQTTClient.Parser where

import Data.Bits               ((.|.), shiftL)
import Data.ByteString         (ByteString(..))
import Data.ByteString.Builder
import Data.Default            (def)
import Data.Monoid
import Data.Word               (Word8)

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

data ControlPacket =
  -- | Client requests a connection to a server
    CONNECT
  -- | Acknowledge connection request
  | CONNACK {}
  -- | Publish message
  | PUBLISH { _publishDUP :: Bool, _publishQoS :: QoS, _publishRetain :: Bool }
  -- | Publish acknowledgement
  | PUBACK {}
  -- | Publish received (assured delivery part 1)
  | PUBREC {}
  -- | Public release (assured delivery part 2)
  | PUBREL {}
  -- | Publish complete (assured delivery part 3)
  | PUBCOMP {}
  -- | Publish subscribe to topics
  | SUBSCRIBE {}
  -- | Subscribe acknowledgement
  | SUBACK {}
  -- | Unsubscribe from topics
  | UNSUBSCRIBE {}
  -- | Unbsubscribe acknowledgement
  | UNSUBACK {}
  -- | PING request
  | PINGREQ {}
  -- | PING response
  | PINGRESP {}
  -- | Disconnect notification
  | DISCONNECT {}
  deriving (Eq, Show)

type RemainingLength = Word8

-- | 2.2 Fixed header
fixedHeader :: ControlPacket -> RemainingLength -> Builder
fixedHeader f rl =
  let
    type'   = shiftL (controlPacketTypeWord f) 4
    typeFlags' = fixedHeaderFlags f
    byte1 = word8 (type' .|. typeFlags')
    byte2 = word8 rl
  in byte1 <> byte2

-- | 2.2 Fixed header - Flags specific to each MQTT Control Packet type
fixedHeaderFlags :: ControlPacket -> Word8
fixedHeaderFlags (PUBLISH dup qos retain)=
    let retain' = boolWord retain
        qos'    = shiftL (qosWord qos) 1
        dup'    = shiftL (boolWord dup) 3
    in dup' .|. qos' .|. retain'
 -- for all others this is currently "reserved" defined in 2.2.2
fixedHeaderFlags CONNECT     = 0
fixedHeaderFlags CONNACK     = 0
fixedHeaderFlags PUBACK      = 0
fixedHeaderFlags PUBREC      = 0
fixedHeaderFlags PUBREL      = 2
fixedHeaderFlags PUBCOMP     = 0
fixedHeaderFlags SUBSCRIBE   = 2
fixedHeaderFlags SUBACK      = 0
fixedHeaderFlags UNSUBSCRIBE = 2
fixedHeaderFlags UNSUBACK    = 0
fixedHeaderFlags PINGREQ     = 0
fixedHeaderFlags PINGRESP    = 0
fixedHeaderFlags DISCONNECT  = 0

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
controlPacketTypeWord CONNECT     = 1
controlPacketTypeWord CONNACK     = 2
controlPacketTypeWord PUBLISH {}  = 3
controlPacketTypeWord PUBACK      = 4
controlPacketTypeWord PUBREC      = 5
controlPacketTypeWord PUBREL      = 6
controlPacketTypeWord PUBCOMP     = 7
controlPacketTypeWord SUBSCRIBE   = 8
controlPacketTypeWord SUBACK      = 9
controlPacketTypeWord UNSUBSCRIBE = 10
controlPacketTypeWord UNSUBACK    = 11
controlPacketTypeWord PINGREQ     = 12
controlPacketTypeWord PINGRESP    = 13
controlPacketTypeWord DISCONNECT  = 14
