module Socket.Controller where

import           Control.Concurrent.Chan (Chan, writeChan)
import           Data.Aeson              (encode)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Pool               (Pool)
import qualified Data.Text.Lazy          as TL
import           Database.MySQL.Simple   (Connection)
import           Domain                  (NewMessageBody (..), User (..))
import           Entity.Message
import           Socket.Domain           (ChanMsg, NewMessageRequest (..),
                                          SocketMsgRequest, payload, prepare)

newMessage ::
       Pool Connection
    -> Chan ChanMsg
    -> Maybe User
    -> Maybe (SocketMsgRequest NewMessageRequest)
    -> IO ()
newMessage pool chan (Just (User id _ _ _)) (Just newMessage) = do
    let chatIdVal = TL.pack $ show $ chatId $ payload newMessage
        message = Just (NewMessageBody $ TL.pack $ Socket.Domain.value $ payload newMessage)
    message <- insertMessage pool chatIdVal (Just id) message
    case message of
        Left _    -> return ()
        Right mes -> writeChan chan (encode $ prepare mes) >> return ()
newMessage _ _ _ _ = return ()
