{-# LANGUAGE DataKinds #-}
module Network.MQTTClient.Encoding where

import           Data.Bits               ((.|.), shiftL)
import           Data.ByteString         as BS
import           Data.ByteString.Lazy    as BSL
import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra (BufferWriter)
import           Data.Default            (def)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Data.Word               (Word8, Word16)

import Network.MQTTClient.Types

controlPacket :: ControlPacket -> BSL.ByteString
controlPacket cp =
  let fixedHeader' len = toLazyByteString $ fixedHeader cp len
      body             = toLazyByteString (varHeader cp <> payload cp)
      remainingLength  = fromIntegral $ BSL.length body
  in fixedHeader' remainingLength <> body

willMessage :: T.Text -> Builder
willMessage t = word16BE 0 <> encodeUtf8 t

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

keepAlive :: KeepAlive -> Builder
keepAlive (KeepAlive a) = word16BE a

-- | 1.5.3 UTF-8 encoded strings
encodeUtf8 :: T.Text -> Builder
encodeUtf8 t =
  let len = fromIntegral $ T.length t :: Word16
  in word16BE len <> TE.encodeUtf8Builder t

mqttClientId :: MQTTClientId -> Builder
mqttClientId (MQTTClientId m)= encodeUtf8 m

packetIdentifier :: PacketIdentifier -> Builder
packetIdentifier (PacketIdentifier pi) = word16BE pi

-- | 2.2 Fixed header
fixedHeader :: ControlPacket -> RemainingLength -> Builder
fixedHeader cp rl =
  let type'      = shiftL (controlPacketTypeWord cp) 4
      typeFlags' = fixedHeaderFlags cp
  in word8 (type' .|. typeFlags') <> word8 rl

-- | 2.2 Fixed header - Flags specific to each MQTT Control Packet type
-- for most this is currently "reserved" defined in 2.2.2
fixedHeaderFlags :: ControlPacket -> Word8
fixedHeaderFlags CONNECT {} = 0x0
fixedHeaderFlags CONNACK {} = 0x0
fixedHeaderFlags (PUBLISH dup qos retain _ _ _) =
    let retain' = boolWord retain
        qos'    = shiftL (qosWord qos) 1
        dup'    = shiftL (boolWord dup) 3
    in (dup' .|. qos' .|. retain')
fixedHeaderFlags PUBACK         = 0x0
fixedHeaderFlags PUBREC         = 0x0
fixedHeaderFlags PUBREL         = 0x2
fixedHeaderFlags PUBCOMP        = 0x0
fixedHeaderFlags SUBSCRIBE {}   = 0x2
fixedHeaderFlags SUBACK {}      = 0x0
fixedHeaderFlags UNSUBSCRIBE {} = 0x2
fixedHeaderFlags UNSUBACK       = 0x0
fixedHeaderFlags PINGREQ        = 0x0
fixedHeaderFlags PINGRESP       = 0x0
fixedHeaderFlags DISCONNECT     = 0x0

-- | 2.3.1
packetIdentifierRequired :: QoS -> Bool
packetIdentifierRequired qos = qos == AtLeastOnce || qos == ExactlyOnce

varHeader :: ControlPacket -> Builder
-- | 3.2.2
varHeader (CONNECT keepAlive' flags _) =
     protocolName
  <> protocolLevel_3_1_1
  <> connectFlags flags
  <> keepAlive keepAlive'
varHeader CONNACK {}   = word8 0
varHeader (PUBLISH _ qos _ packetIdentifier' topic _) =
  let topic'      = encodeUtf8 topic
      identifier' =
        if packetIdentifierRequired qos -- 2.3.1
        then fromMaybe mempty (packetIdentifier <$> packetIdentifier')
        else mempty
  in  topic' <> identifier'
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
payload (CONNECT _ _ payload) =
  let ci   = mqttClientId $ _connectPayloadClientId payload :: Builder
      wt   = encodeUtf8   $ _unMQTTTopic $ _connectPayloadWillTopic payload  :: Builder
      wm   = willMessage  $ _connectPayloadWillMessage payload :: Builder
      un   = encodeUtf8   $ _connectPayloadUserName payload :: Builder
      pass = encodeUtf8   $ _connectPayloadPassword payload :: Builder
  in  ci <> wt <> wm <> un <> pass
payload CONNACK {}     = word8 0
payload (PUBLISH _ _ _ _ _ payload) =
  byteString payload
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
controlPacketTypeWord CONNACK {}     = 2
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
