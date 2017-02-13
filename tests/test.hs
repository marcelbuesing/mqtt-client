{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Concurrent.Async   (concurrently)
import           Data.Attoparsec.ByteString
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString            as BS
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Conduit
import           Data.Conduit.Network
import           Data.ByteString.Base16     as B16
import           Network.MQTTClient.Client  as MQTT
import           Network.Socket.ByteString  as NSB
import qualified Network.Socket             as NS

import qualified Data.Conduit.List as CL

import           Network.MQTTClient.Client as Client
import           Network.MQTTClient.Parser  as Parser
import           Network.MQTTClient.Encoding as Encoding
import           Network.MQTTClient.Types

main = defaultMain $ tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

host :: BS.ByteString
host = "localhost"

stdSink :: Show a => Sink a IO () -- consumes a stream of Strings, no result
stdSink = CL.mapM_ $ (putStrLn . show)

echo :: ConduitM BS.ByteString BS.ByteString IO ()
echo = do
  awaitForever $ \x -> do
    liftIO $ BS.putStrLn $ B16.encode x  -- Just output the stream to stdout
    yield x

sourceMqtt :: AppData -> Source IO (Either ParseError (PositionRange, ControlPacket))
sourceMqtt appData= appSource appData $= echo $= conduitParserEither Parser.controlPacket

initMqtt :: Source IO BS.ByteString
initMqtt = do
  yield $ toStrict $ Encoding.controlPacket connectPacket
  yield $ toStrict $ Encoding.controlPacket subscribePacket
  yield $ toStrict $ Client.publish "B" "HI"

unitTests = testGroup "Setup"
  [ testCase "CONNECT" $ do
      runTCPClient (clientSettings 1883 host) $ \appData ->
        void $ concurrently
          (sourceMqtt appData $$ stdSink)
          (initMqtt $$ appSink appData)
      return ()
  ]
