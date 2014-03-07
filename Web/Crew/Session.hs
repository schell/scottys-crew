{-# LANGUAGE OverloadedStrings #-}
module Web.Crew.Session where

import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Web.Scotty
import           Web.ClientSession
import           Crypto.BCrypt
import           Data.Time.Clock
import           Network.HTTP.Types.Status
import           Data.Aeson hiding (json)
import           Web.Crew.User.Types
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L


type CookieMap = M.Map T.Text T.Text


cookieName :: T.Text
cookieName = "scottys_crew_session"


parseCookies :: T.Text -> CookieMap
parseCookies = foldl mapify M.empty . map tuple . splitCookies
    where splitCookies   = T.split (==';')
          tuple t        = (T.takeWhile (/= '=') $ T.dropWhile (== ' ') t, T.drop 1 $ T.dropWhile (/= '=') t)
          mapify m (k,v) = M.insert k v m


statusOk :: ActionM ()
statusOk = json $ object [ "status" .= ("ok" :: T.Text) ]


jsonErr :: Status -> ActionM ()
jsonErr stat =
    json $ object [ "status"  .= ("error" :: T.Text)
                  , "code"    .= statusCode stat
                  , "message" .= (T.pack . B.unpack $ statusMessage stat)
                  ]


authorize :: ActionM () -> ActionM ()
authorize f = f `ifAuthorizedOr` jsonErr unauthorized401


deauthorize :: ActionM ()
deauthorize = writeUserCookie $ UserCookie (Id (-1)) zeroDay


ifAuthorizedOr :: ActionM () -> ActionM () -> ActionM ()
ifAuthorizedOr f g = do
    mCookie <- readUserCookie
    if isNothing mCookie then g else do
        -- Check the expiry.
        let c = fromJust mCookie
        invalidCookie <- cookieHasExpired c
        if invalidCookie || _cookieUserId c <= Id (-1)
        then g else do
            -- Update the cookie.
            writeUserCookie c
            f


authorizeAdmin :: ActionM() -> ActionM ()
authorizeAdmin f = authorize $ do
    id' <- fmap fromJust getUserId
    if id' > Id 0 then jsonErr forbidden403 else f


authorizeAndId :: (Id -> ActionM ()) -> ActionM ()
authorizeAndId f = authorize $ do
    mUid <- getUserId
    maybe (jsonErr forbidden403) f mUid


authorizeThenWithId :: (Id -> ActionM ()) -> ActionM ()
authorizeThenWithId = authorizeAndId


cookieHasExpired :: UserCookie -> ActionM Bool
cookieHasExpired c = fmap (<= 0) $ cookieExpiresIn c


cookieExpiresIn :: UserCookie -> ActionM NominalDiffTime
cookieExpiresIn c = do
    t <- liftIO getCurrentTime
    let d = diffUTCTime (_cookieExpires c) t
    return d


userWithPasswordIsValid :: User -> T.Text -> Bool
userWithPasswordIsValid u p = do
    let saltedPass = _userSalt u `B.append` B.pack (T.unpack p)
    validatePassword (_userPassword u) saltedPass


writeUserCookie :: UserCookie -> ActionM ()
writeUserCookie c = do
    c'  <- liftIO $ updateExpiryOnCookie c
    k   <- liftIO getDefaultKey
    c'' <- liftIO $ encryptIO k $ L.toStrict $ encode c'
    setHeader "Set-Cookie" $ T.concat [ cookieName
                                      , "="
                                      , T.pack $ B.unpack c''
                                      , "; "
                                      ]


readUserCookie :: ActionM (Maybe UserCookie)
readUserCookie = do
    -- Retrieve and parse our cookies.
    mCookies <- reqHeader "Cookie"
    if isNothing mCookies then return Nothing else
      -- Make sure we have our specific cookie.
      let cookies = parseCookies $ fromJust mCookies
          mCookie = M.lookup cookieName cookies
      in if isNothing mCookie then return Nothing else
           -- Decrypt our cookie data.
           do k <- liftIO getDefaultKey
              let cookie = fromJust mCookie
                  mData  = decrypt k (B.pack $ T.unpack cookie)
              if isNothing mData then return Nothing else
                let datum = L.fromStrict $ fromJust mData
                in return (decode datum :: Maybe UserCookie)


updateExpiryOnCookie :: UserCookie -> IO UserCookie
updateExpiryOnCookie c = do
    t <- getCurrentTime
    let t' = addUTCTime (10*60) t
    return c{_cookieExpires=t'}


getUserId :: ActionM (Maybe Id)
getUserId = do
    mCookie <- readUserCookie
    return $ if isNothing mCookie then Nothing else
        Just $ _cookieUserId $ fromJust mCookie

