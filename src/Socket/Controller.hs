module Socket.Controller where

import           Control.Concurrent.Chan (Chan, writeChan)
import           Data.Aeson              (encode)
import           Data.ByteString.Lazy    (ByteString)
import           Socket.Domain           (ChanMsg, NewMessage, SocketMsg,
                                          payload)

newMessage :: Chan ChanMsg -> Maybe (SocketMsg NewMessage) -> IO ()
newMessage chan (Just newMessage) = do
    writeChan chan (encode $ payload newMessage)
    return ()
newMessage _ _ = return ()
