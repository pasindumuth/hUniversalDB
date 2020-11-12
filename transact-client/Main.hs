{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--import qualified Control.Concurrent as Ct
--import qualified Control.Monad as Mo
--import qualified Network.Simple.TCP as TCP
--import qualified System.Environment as SE
--
--import qualified Infra.Logging as Lg
import qualified Infra.Utils as U
--import qualified Net.Connections as Cn
--import qualified Proto.Common as Co
--import qualified Proto.Messages as Ms
--import Infra.State

import qualified Transact.SQL.Sql as Sql
import qualified Transact.SQL.Parse as P

--startClient :: String -> IO ()
--startClient sip = do
--  Lg.infoM Lg.main "Starting client"
--  TCP.connect sip "8000" $ \(slaveSocket, remoteAddr) -> do
--    Lg.infoM Lg.main $ "Connection established to " ++ show remoteAddr
--    Ct.forkIO $ Mo.forever $ do
--      msg :: Ms.Message <- Cn.receiveMessage slaveSocket
--      putStrLn $ U.ppShow msg
--    return ()
--
--main :: IO ()
--main = do
--  Lg.setupLogging
--  args <- SE.getArgs
--  let (sip:_) = args
--  startClient sip

main :: IO ()
main = do
   let line = unlines [
                "SELECT firstName as f,",
                "       lastName as l",
                "FROM Customer",
                "WHERE true;"
              ]
   print $ "Query: " ++ line
   putStrLn $ U.ppShow $ Sql.calc $ P.lexer line
