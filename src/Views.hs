module Views where

import           Control.Monad.IO.Class
import           Data.Monoid               (mconcat)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Domain
import           GHC.Generics              (Generic)
import           Web.Scotty
import           Web.Scotty.Internal.Types

articlesList :: [Article] -> ActionM ()
articlesList articles = json articles

viewArticle :: Maybe Article -> ActionM ()
viewArticle Nothing        = json ()
viewArticle (Just article) = json article

createdArticle :: Maybe Article -> ActionM ()
createdArticle article = json ()

updatedArticle :: Maybe Article -> ActionM ()
updatedArticle article = json ()

deletedArticle :: TL.Text -> ActionM ()
deletedArticle id = json ()

chatWithLastMessageList :: [ChatWithLastMessage] -> ActionM ()
chatWithLastMessageList chats = json chats

viewChat :: Maybe Chat -> ActionM ()
viewChat (Just chat) = json chat
viewChat Nothing     = json ()

viewChatWithMessages :: Maybe ChatWithMessages -> ActionM ()
viewChatWithMessages (Just chat) = json chat
viewChatWithMessages Nothing     = json ()

createdMessage :: Maybe Message -> ActionM ()
createdMessage (Just message) = json message
createdMessage Nothing        = json ()
