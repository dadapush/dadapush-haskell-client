{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DaDaPushPublic.Types (
  Action (..),
  MessageObject (..),
  MessagePushRequest (..),
  MessagePushResponse (..),
  PageResponseOfMessageObject (..),
  Result (..),
  ResultOfMessageObject (..),
  ResultOfMessagePushResponse (..),
  ResultOfPageResponseOfMessageObject (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data Action = Action
  { actionName :: Text -- ^ action button name
  , actionType :: Text -- ^ fix value: link
  , actionUrl :: Text -- ^ action url
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Action where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "action")
instance ToJSON Action where
  toJSON = genericToJSON (removeFieldLabelPrefix False "action")


-- | 
data MessageObject = MessageObject
  { messageObjectActions :: Maybe [Action] -- ^ action size range is 0,3
  , messageObjectChannelName :: Text -- ^ 
  , messageObjectContent :: Text -- ^ 
  , messageObjectCreatedAt :: Text -- ^ 
  , messageObjectId :: Integer -- ^ 
  , messageObjectTitle :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessageObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "messageObject")
instance ToJSON MessageObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "messageObject")


-- | 
data MessagePushRequest = MessagePushRequest
  { messagePushRequestActions :: Maybe [Action] -- ^ action size range is 0,3
  , messagePushRequestContent :: Text -- ^ message content
  , messagePushRequestNeedPush :: Bool -- ^ when value is false, will not send client push
  , messagePushRequestTitle :: Text -- ^ message title
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessagePushRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "messagePushRequest")
instance ToJSON MessagePushRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "messagePushRequest")


-- | 
data MessagePushResponse = MessagePushResponse
  { messagePushResponseMessageId :: Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessagePushResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "messagePushResponse")
instance ToJSON MessagePushResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "messagePushResponse")


-- | 
data PageResponseOfMessageObject = PageResponseOfMessageObject
  { pageResponseOfMessageObjectContent :: Maybe [MessageObject] -- ^ 
  , pageResponseOfMessageObjectTotalElements :: Maybe Integer -- ^ 
  , pageResponseOfMessageObjectTotalPages :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageResponseOfMessageObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "pageResponseOfMessageObject")
instance ToJSON PageResponseOfMessageObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "pageResponseOfMessageObject")


-- | 
data Result = Result
  { resultCode :: Int -- ^ 
  , resultData :: Maybe Value -- ^ 
  , resultErrmsg :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Result where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "result")
instance ToJSON Result where
  toJSON = genericToJSON (removeFieldLabelPrefix False "result")


-- | 
data ResultOfMessageObject = ResultOfMessageObject
  { resultOfMessageObjectCode :: Int -- ^ 
  , resultOfMessageObjectData :: Maybe MessageObject -- ^ 
  , resultOfMessageObjectErrmsg :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ResultOfMessageObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "resultOfMessageObject")
instance ToJSON ResultOfMessageObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "resultOfMessageObject")


-- | 
data ResultOfMessagePushResponse = ResultOfMessagePushResponse
  { resultOfMessagePushResponseCode :: Int -- ^ 
  , resultOfMessagePushResponseData :: Maybe MessagePushResponse -- ^ 
  , resultOfMessagePushResponseErrmsg :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ResultOfMessagePushResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "resultOfMessagePushResponse")
instance ToJSON ResultOfMessagePushResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "resultOfMessagePushResponse")


-- | 
data ResultOfPageResponseOfMessageObject = ResultOfPageResponseOfMessageObject
  { resultOfPageResponseOfMessageObjectCode :: Int -- ^ 
  , resultOfPageResponseOfMessageObjectData :: Maybe PageResponseOfMessageObject -- ^ 
  , resultOfPageResponseOfMessageObjectErrmsg :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ResultOfPageResponseOfMessageObject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "resultOfPageResponseOfMessageObject")
instance ToJSON ResultOfPageResponseOfMessageObject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "resultOfPageResponseOfMessageObject")


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do viceversa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
