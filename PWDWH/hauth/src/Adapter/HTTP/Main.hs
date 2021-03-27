module Adapter.HTTP.Main where

import           ClassyPrelude           hiding ( delete )
import           Network.HTTP.Types.Status      ( unauthorized401 )
import           Network.Wai.Middleware.Gzip
import           Web.Scotty.Trans

main :: IO ()
main = scottyT 3000 id routes

routes :: (MonadIO m) => ScottyT LText m ()
routes = do
  -- gzip
  middleware $ gzip (def { gzipFiles = GzipCompress })
  -- request logging
  -- middleware logStdout
  -- serve static files
  -- middleware static

  get "/unauth" $ do
    status unauthorized401
    addHeader "serverName" "gandalfService"
    text "you shall not pass !"

  get "/hello" $ do
    name <- param "name" `rescue` \_ -> return "anonymous"
    text $ "Hello, " <> name

  get "/hello/:name" $ param "name" >>= text . ("Hello, " <>)

  get "/users/:userId/books/:bookId" $ do
    userId <- param "userId"
    bookId <- param "bookId"
    text $ userId <> " - " <> bookId

  get (regex "^/users/(.+)/investments/(.+)$") $ do
    fullPath     <- param "0"
    userId       <- param "1"
    investmentId <- param "2"
    text $ fullPath <> " : " <> userId <> " - " <> investmentId

  post "/users" $ text "adding user"
  put "/users/:id" $ text "updating user"
  patch "/users/:id" $ text "partially updating users"
  delete "/users/:id" $ text "deleting user"

  matchAny "/admin" $ text "I don't care about your HTTP verb"
  options (regex ".*") $ text "CORS usually use this"
  notFound $ text "404"

  defaultHandler $ \_ -> text "Something went terribly wrong :("
