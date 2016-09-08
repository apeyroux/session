{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy as BSL
import           Data.Monoid
import           Data.Text as T
import           Data.Text.Encoding as TE
import qualified Database.Redis as R
import           GHC.Base
import           GHC.Generics
import qualified Network.HTTP.Types.Status as HTTPSTATUS
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty as S

type UID = T.Text
type IMEI = T.Text

data Status = Online | Offline deriving (Show, Generic)

data Session = Session { sessionStatus :: Status
                       , sessionUID    :: UID
                       , sessionIMEI   :: IMEI } deriving (Show, Generic)

data RedisValue = RedisValue { redisValueIMEI :: T.Text
                             , redisValueUID  :: T.Text } deriving (Show, Generic)

instance FromJSON Status
instance ToJSON Status where
  toJSON Online = "online"
  toJSON Offline = "offline"
  
instance FromJSON Session
instance ToJSON Session where
  toJSON (Session sstatus uid imei) = object ["status" .= sstatus
                                             , "imei"  .= imei
                                             , "uid"   .= uid]

instance ToJSON RedisValue
instance FromJSON RedisValue where
  parseJSON (Object v) = RedisValue <$>
    v .: "imei" <*>
    v .: "uid"
  parseJSON _          = GHC.Base.empty

sessionFromRedis :: R.Connection -> T.Text -> IO (Maybe RedisValue)
sessionFromRedis redis key = R.runRedis redis $ do
  session <- R.get $ TE.encodeUtf8 key
  case session of
    (Right (Just s)) -> return (decode (BSL.fromStrict s)::Maybe RedisValue)
    _ -> return Nothing

main :: IO ()
main = do
  redis <- R.connect R.defaultConnectInfo
  scotty 3000 $ do
    middleware logStdout
    get "/" $ do
      send401
    get "/session/" $ do
      k <- S.param "key"
      redisValue <- liftIO $ sessionFromRedis redis ("session:" <> k)
      case redisValue of
        Just value -> do
          status HTTPSTATUS.status200
          S.json $ redisValueToSession value
        Nothing -> send401
  where
    redisValueToSession redisValue = Session Online (redisValueUID redisValue) (redisValueIMEI redisValue)
    send401 = status HTTPSTATUS.status401 >> text "401 not authorized"
