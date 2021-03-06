{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Crew.User.DB where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Web.Crew.User.Types


peekUserWithEmail :: T.Text -> Query Users (Maybe User)
peekUserWithEmail email = do
    (Users _ users) <- ask
    let users' = M.elems users
        user   = foldl accum Nothing users'
        accum Nothing u = if _userEmail u == email then Just u else Nothing
        accum (Just u) _ = Just u
    return user


peekUserWithId :: Id -> Query Users (Maybe User)
peekUserWithId uid = do
    (Users _ users) <- ask
    return $ M.lookup uid users


updateUser :: User -> Update Users ()
updateUser user = do
    (Users n m) <- get
    let m' = M.update (\_ -> Just user) (_userId user) m
    put $ Users n m'


addUser :: T.Text -> T.Text -> B.ByteString -> B.ByteString -> Update Users ()
addUser name email pass salt = do
    (Users n m) <- get
    let u  = User (Id n) name email pass salt zeroDay
        m' = M.insert (Id n) u m
    put $ Users (succ n) m'


peekUsers :: Query Users [User]
peekUsers = do
    (Users _ m) <- ask
    return $ M.elems m


$(makeAcidic ''Users ['peekUserWithEmail, 'peekUserWithId, 'updateUser, 'addUser, 'peekUsers])
