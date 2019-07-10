{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module DaDaPushPublic.API
  -- * Client and Server
  ( Config(..)
  , DaDaPushPublicBackend(..)
  , createDaDaPushPublicClient
  , runDaDaPushPublicServer
  , runDaDaPushPublicClient
  , runDaDaPushPublicClientWithManager
  , callDaDaPushPublic
  , DaDaPushPublicClient
  , DaDaPushPublicClientError(..)
  -- ** Servant
  , DaDaPushPublicAPI
  ) where

import           DaDaPushPublic.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serve)
import           Servant.API
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..))
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for DaDaPushPublic.
type DaDaPushPublicAPI
    =    "api" :> "v1" :> "message" :> ReqBody '[JSON] MessagePushRequest :> Header "x-channel-token" Text :> Verb 'POST 200 '[JSON] ResultOfMessagePushResponse -- 'createMessage' route
    :<|> "api" :> "v1" :> "message" :> Capture "messageId" Integer :> Header "x-channel-token" Text :> Verb 'DELETE 200 '[JSON] Result -- 'deleteMessage' route
    :<|> "api" :> "v1" :> "message" :> Capture "messageId" Integer :> Header "x-channel-token" Text :> Verb 'GET 200 '[JSON] ResultOfMessageObject -- 'getMessage' route
    :<|> "api" :> "v1" :> "messages" :> QueryParam "page" Int :> QueryParam "pageSize" Int :> Header "x-channel-token" Text :> Verb 'GET 200 '[JSON] ResultOfPageResponseOfMessageObject -- 'getMessages' route


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype DaDaPushPublicClientError = DaDaPushPublicClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for DaDaPushPublic.
-- The backend can be used both for the client and the server. The client generated from the DaDaPushPublic OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createDaDaPushPublicClient@). Alternatively, provided
-- a backend, the API can be served using @runDaDaPushPublicServer@.
data DaDaPushPublicBackend m = DaDaPushPublicBackend
  { createMessage :: MessagePushRequest -> Maybe Text -> m ResultOfMessagePushResponse{- ^ <h2>Rate Limit:</h2><ul><li>1 request per 1s</li><li>30 request per 1m</li><li>500 request per 1h</li></ul><h2>Result code/errmsg List:</h2><ul><li>0: ok</li><li>1: server error</li><li>101: channel is exists</li><li>102: channel is not exists</li><li>103: channel token error</li><li>104: channel is not exists</li><li>105: message is not exists</li><li>204: bad request</li><li>205: permission deny</li><li>206: too many request, please after 5 minutes to try!</li><li>301: duplicate username/email</li><li>302: user is not exists</li><li>303: user password is error</li><li>304: client push token is error</li><li>305: user is disabled</li><li>306: your subscription is expired</li><li>307: user not subscribe channel</li></ul> -}
  , deleteMessage :: Integer -> Maybe Text -> m Result{- ^ <h2>Rate Limit:</h2><ul><li>10 request per 1s</li><li>100 request per 1m</li><li>1000 request per 1h</li></ul><h2>Result code/errmsg List:</h2><ul><li>0: ok</li><li>1: server error</li><li>101: channel is exists</li><li>102: channel is not exists</li><li>103: channel token error</li><li>104: channel is not exists</li><li>105: message is not exists</li><li>204: bad request</li><li>205: permission deny</li><li>206: too many request, please after 5 minutes to try!</li><li>301: duplicate username/email</li><li>302: user is not exists</li><li>303: user password is error</li><li>304: client push token is error</li><li>305: user is disabled</li><li>306: your subscription is expired</li><li>307: user not subscribe channel</li></ul> -}
  , getMessage :: Integer -> Maybe Text -> m ResultOfMessageObject{- ^ <h2>Rate Limit:</h2><ul><li>10 request per 1s</li><li>100 request per 1m</li><li>1000 request per 1h</li></ul><h2>Result code/errmsg List:</h2><ul><li>0: ok</li><li>1: server error</li><li>101: channel is exists</li><li>102: channel is not exists</li><li>103: channel token error</li><li>104: channel is not exists</li><li>105: message is not exists</li><li>204: bad request</li><li>205: permission deny</li><li>206: too many request, please after 5 minutes to try!</li><li>301: duplicate username/email</li><li>302: user is not exists</li><li>303: user password is error</li><li>304: client push token is error</li><li>305: user is disabled</li><li>306: your subscription is expired</li><li>307: user not subscribe channel</li></ul> -}
  , getMessages :: Maybe Int -> Maybe Int -> Maybe Text -> m ResultOfPageResponseOfMessageObject{- ^ <h2>Rate Limit:</h2><ul><li>1 request per 1s</li><li>45 request per 1m</li></ul><h2>Result code/errmsg List:</h2><ul><li>0: ok</li><li>1: server error</li><li>101: channel is exists</li><li>102: channel is not exists</li><li>103: channel token error</li><li>104: channel is not exists</li><li>105: message is not exists</li><li>204: bad request</li><li>205: permission deny</li><li>206: too many request, please after 5 minutes to try!</li><li>301: duplicate username/email</li><li>302: user is not exists</li><li>303: user password is error</li><li>304: client push token is error</li><li>305: user is disabled</li><li>306: your subscription is expired</li><li>307: user not subscribe channel</li></ul> -}
  }

newtype DaDaPushPublicClient a = DaDaPushPublicClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative DaDaPushPublicClient where
  pure x = DaDaPushPublicClient (\_ -> pure x)
  (DaDaPushPublicClient f) <*> (DaDaPushPublicClient x) =
    DaDaPushPublicClient (\env -> f env <*> x env)

instance Monad DaDaPushPublicClient where
  (DaDaPushPublicClient a) >>= f =
    DaDaPushPublicClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO DaDaPushPublicClient where
  liftIO io = DaDaPushPublicClient (\_ -> liftIO io)

createDaDaPushPublicClient :: DaDaPushPublicBackend DaDaPushPublicClient
createDaDaPushPublicClient = DaDaPushPublicBackend{..}
  where
    ((coerce -> createMessage) :<|>
     (coerce -> deleteMessage) :<|>
     (coerce -> getMessage) :<|>
     (coerce -> getMessages)) = client (Proxy :: Proxy DaDaPushPublicAPI)

-- | Run requests in the DaDaPushPublicClient monad.
runDaDaPushPublicClient :: Config -> DaDaPushPublicClient a -> ExceptT ClientError IO a
runDaDaPushPublicClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runDaDaPushPublicClientWithManager manager clientConfig cl

-- | Run requests in the DaDaPushPublicClient monad using a custom manager.
runDaDaPushPublicClientWithManager :: Manager -> Config -> DaDaPushPublicClient a -> ExceptT ClientError IO a
runDaDaPushPublicClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a DaDaPushPublicClientError
callDaDaPushPublic
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> DaDaPushPublicClient a -> m a
callDaDaPushPublic env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (DaDaPushPublicClientError err)
    Right response -> pure response

-- | Run the DaDaPushPublic server at the provided host and port.
runDaDaPushPublicServer
  :: (MonadIO m, MonadThrow m)
  => Config -> DaDaPushPublicBackend (ExceptT ServerError IO) -> m ()
runDaDaPushPublicServer Config{..} backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy DaDaPushPublicAPI) (serverFromBackend backend)
  where
    serverFromBackend DaDaPushPublicBackend{..} =
      (coerce createMessage :<|>
       coerce deleteMessage :<|>
       coerce getMessage :<|>
       coerce getMessages)
