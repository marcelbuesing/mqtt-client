module Network.MQTTClient.Parser where

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Attoparsec as A
import Data.Attoparsec.Binary
import Data.Bits
import Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word16, Word8)

import Network.MQTTClient.Types

boolWord :: Parser Bool
boolWord =
  let true = return True <$> word8 0x1
      false = return False <$> word8 0x0
  in true <|> false

remainingLength :: Parser RemainingLength
remainingLength = anyWord8 -- TODO properly decode

packetIdentifier :: Parser PacketIdentifier
packetIdentifier =  PacketIdentifier <$> anyWord16be

packetIdentifierMaybe :: QoS -> Parser (Maybe PacketIdentifier)
packetIdentifierMaybe qos =
  if qos /= AtMostOnce
  then Just <$> packetIdentifier
  else return $ Nothing

controlPacketType :: Word8 -> Parser Word8
controlPacketType pre = satisfy (\w -> (shiftL w 4 .&. pre) == pre)

decodeMqttUtf8 :: Parser (Word16, T.Text)
decodeMqttUtf8 = do
  len <- anyWord16be
  txt <- A.take $ fromIntegral len
  return $ (len, decodeUtf8With lenientDecode txt)

connack :: Parser ControlPacket
connack = do
  _ <- word8 0x20 -- packetType
  _ <- word8 0x2 -- 3.2.1 apparently this always equals 2
  sessionPresent <- boolWord
  returnCode     <- connectReturnCode
  return $ CONNACK sessionPresent returnCode

-- | Table 3.1 - Connect Return code values
connectReturnCode :: Parser ConnectReturnCode
connectReturnCode =
      return ConnectionAccepted    <$> word8 0x00
  <|> return UnacceptableProtocol  <$> word8 0x01
  <|> return IdentifierRejected    <$> word8 0x02
  <|> return ServerUnavailable     <$> word8 0x03
  <|> return BadUserNameOrPassword <$> word8 0x04
  <|> return NotAuthorized         <$> word8 0x05

suback :: Parser ControlPacket
suback = do
  _ <- word8 0x90
  _ <- remainingLength
  pID <- packetIdentifier
  returnCodes <- A.count 2 subAckReturnCode
  return $ SUBACK pID returnCodes

subAckReturnCode :: Parser SubAckReturnCode
subAckReturnCode =
      return SuccessMaxQoS0 <$> word8 0x00
  <|> return SuccessMaxQoS0 <$> word8 0x00
  <|> return SuccessMaxQoS1 <$> word8 0x01
  <|> return SuccessMaxQoS2 <$> word8 0x02
  <|> return SubFailure     <$> word8 0x80

publish :: Parser ControlPacket
publish = do
  _ <- word8 0x30 -- TODO parse flags
  remainingLength' <- remainingLength
  (len, topic)     <- decodeMqttUtf8
  pID              <- packetIdentifierMaybe AtMostOnce -- TODO use qos from flags
  payload          <- A.take $ lengthPayload remainingLength' len AtMostOnce  -- TODO use qos from flags
  return $ PUBLISH False AtMostOnce False Nothing topic payload
  where
    lengthPacketIdentifier qos  = if qos == AtMostOnce then 0 else 2
    lengthTopic lenTopic = 2 + (fromIntegral lenTopic)
    lengthPayload rl lenTopic qos =
          (fromIntegral rl) - lengthTopic lenTopic - lengthPacketIdentifier qos

controlPacket ::  Parser ControlPacket
controlPacket = connack <|> suback <|> publish


