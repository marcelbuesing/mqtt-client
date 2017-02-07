module Network.MQTTClient.Parser where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Word (Word16)

import Network.MQTTClient.Types

boolWord :: Parser Bool
boolWord =
  let true = return True <$> word8 0x1
      false = return False <$> word8 0x0
  in true <|> false

remainingLength :: Parser RemainingLength
remainingLength = anyWord8

packetIdentifier :: Parser PacketIdentifier
packetIdentifier = PacketIdentifier <$> anyWord16be

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
  returnCodes <- many1 subAckReturnCode
  return $ SUBACK pID returnCodes

subAckReturnCode :: Parser SubAckReturnCode
subAckReturnCode =
      return SuccessMaxQoS0 <$> word8 0x00
  <|> return SuccessMaxQoS0 <$> word8 0x00
  <|> return SuccessMaxQoS1 <$> word8 0x01
  <|> return SuccessMaxQoS2 <$> word8 0x02
  <|> return SubFailure     <$> word8 0x80

controlPacket ::  Parser ControlPacket
controlPacket = connack <|> suback
