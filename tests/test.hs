import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad              (forever)
import           Data.Attoparsec.ByteString
import           Network.MQTTClient.Client  as MQTT
import           Network.Socket.ByteString  as NSB
import qualified Network.Socket             as NS

import           Network.MQTTClient.Parser  as Parser

main = defaultMain $ tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

addr :: IO NS.SockAddr
addr =
  let host = Just "localhost"
  in NS.addrAddress . head <$> NS.getAddrInfo Nothing host (Just "1883")

unitTests = testGroup "Setup"
  [ testCase "CONNECT" $ do
      addr' <- addr
      sckt <- MQTT.connect addr'
      _ <-forever $ do
        r <- NSB.recv sckt 8
        _ <- putStrLn "Reading"
        parseTest Parser.controlPacket r
      _ <- putStrLn "Closing socket"
      _ <- NS.close sckt
      return ()
  ]
