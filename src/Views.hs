module Views where

import           Domain
import           Web.Scotty
import           Web.Scotty.Internal.Types

chatWithLastMessageList :: [ChatWithLastMessage] -> ActionM ()
chatWithLastMessageList = json

viewChat :: Maybe Chat -> ActionM ()
viewChat (Just chat) = json chat
viewChat Nothing     = json ()

viewChatWithMessages :: ChatWithMessages -> ActionM ()
viewChatWithMessages = json

createdMessage :: Maybe Message -> ActionM ()
createdMessage (Just message) = json message
createdMessage Nothing        = json ()
