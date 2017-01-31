{-# OverloadedStrings #-}

module Network.MQTTClient.Client where

import           Data.ByteString.Lazy       (toStrict)
import           Data.Text                  as T
import           Network (PortNumber)
import           Network.Socket.ByteString  as NSB
import qualified Network.Socket             as NS

import           Network.MQTTClient.Types
import           Network.MQTTClient.Parser as Parser
import           Network.MQTTClient.Encoding as Encoding

clientSocket :: IO NS.Socket
clientSocket = NS.socket NS.AF_INET NS.Stream NS.defaultProtocol

-- | 4.2 default MQTT non TLS port
defaultPort :: PortNumber
defaultPort = read "1833"

-- | 4.2 default MQTT TLS port
defaultTLSPort :: PortNumber
defaultTLSPort = read "8883"

connect :: NS.SockAddr -> IO NS.Socket
connect addr = do
  sckt <- clientSocket
  _ <- NS.connect sckt addr
  _ <- NSB.send sckt $ toStrict $ Encoding.controlPacket connectPacket
  return sckt

connectPacket :: ControlPacket
connectPacket =
  let keepAlive' = KeepAlive 60
      flags' =
        ConnectFlags
        { _connectUserNameFlag = True
        , _connectPasswordFlag = False
        , _connectWillRetain   = False
        , _connectWillQoS      = ExactlyOnce
        , _connectWillFlag     = False
        , _connectCleanSession = True
        }
      payload' =
        ConnectPayload
        { _connectPayloadClientId = MQTTClientId $ T.pack "lambda-myIdIs"
        , _connectPayloadWillTopic = MQTTTopic $ T.pack "TESTTOPIC"
        , _connectPayloadWillMessage = T.pack $ "Oh no!"
        , _connectPayloadUserName = T.pack "JohnDoe"
        , _connectPayloadPassword = mempty
        }
  in CONNECT keepAlive' flags' payload'
