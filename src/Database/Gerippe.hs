{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Gerippe
    ( module Database.Gerippe
    , module Database.Gerippe.Utils
    , module Database.Esqueleto.Legacy
    , delete
    ) where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Database.Esqueleto.Experimental as Esqueleto
import           Database.Esqueleto.Legacy      hiding (delete)
import           Database.Persist               ( delete )

import           Database.Gerippe.Utils         ( collectSnd )

-- $example usage
--
-- @
-- -- from the Yesod book
--
-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Person
--     name String
--     age Int Maybe
--     deriving Show
-- BlogPost
--     title String
--     authorId PersonId
--     deriving Show
-- |]
--
-- -- run queries, e.g. on an sqlite database
-- runSqlite ":memory:" $ do
--   runMigration migrateAll
--
--   johnKey <- insert $ Person "John Doe" $ Just 35
--   _       <- insert $ Person "Jane Doe" Nothing
--
--   insert $ BlogPost "My fr1st p0st" johnKey
--   insert $ BlogPost "One more for good measure" johnKey
-- @

type ToBack a = ToBackendKey SqlBackend a

-- | integer representation of sql key
-- @
--   let johnId = keyToId johnKey -- 1 :: Int64
-- @
keyToId :: (ToBack a, Integral b) => Key a -> b
keyToId = fromIntegral . fromSqlKey

-- | integer representation of sql key of a database entity
entityId :: (ToBack a, Integral b) => Entity a -> b
entityId = keyToId . entityKey

-- | get using the integer representation of sql key
-- @
--   mPerson <- getById johnId
--   liftIO . print $ case mPerson of
--     Just person -> personName person -- type inference
--     Nothing     -> "Not found"
-- @
getById :: (ToBack b, MonadIO m, Integral a) => a -> SqlPersistT m (Maybe b)
getById = get . toSqlKey . fromIntegral

type ToBackEq a = (ToBack a, PersistEntityBackend a ~ SqlBackend)

-- | retrieve all entities by means of type inference
-- @
--   persons <- getAll
--   liftIO . putStrLn . unwords $ map personName persons
-- @
getAll :: (ToBackEq a, MonadIO m) => SqlPersistT m [Entity a]
getAll = select $ from pure

getAllValues :: (ToBackEq a, MonadIO m) => SqlPersistT m [a]
getAllValues = map entityVal <$> getAll

-- | convenience function for simple queries with one where-clause
-- @
--   [johnDoe@(Entity key ent)] <- getWhere PersonName "John Doe"
-- @
getWhere
    :: (ToBackEq a, PersistField b, MonadIO m)
    => EntityField a b
    -> b
    -> SqlPersistT m [Entity a]
getWhere field value = select . from $ \t -> do
    where_ $ t ^. field ==. val value
    pure t

deleteAll
  :: forall a m
  . ( MonadIO m
    , PersistEntity a
    , ToBackEq a
    )
  => SqlPersistT m ()
deleteAll = Esqueleto.delete $ from $ \(a :: SqlExpr (Entity a)) -> pure ()

type EntEq a = (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
type EntEqs a b = (EntEq a, EntEq b)

-- | join a one-to-many relationship and get all entities
-- where the second field is collected as list of values in a map
-- @
--   personMap <- join1ToM PersonId BlogPostAuthorId
--   liftIO . print $ Map.lookup johnDoe personMap
--   -- Just [blogpost1, blogpost2]
--   liftIO . print $ Map.lookup janeDoe personMap
--   -- inner join, no jane
--   -- Nothing
-- @
join1ToM
    :: (EntEqs a b, MonadIO m, Ord a)
    => EntityField a (Key a)
    -> EntityField b (Key a)
    -> SqlPersistT m (Map (Entity a) [Entity b])
join1ToM = (fmap collectSnd .) . join1ToM'

-- | join a one-to-many relationship and get all entities
-- @
--   [(johnDoe, blogpost1), (johnDoe, blogpost2)] -- no map
-- @
join1ToM'
    :: (EntEqs a b, MonadIO m)
    => EntityField a (Key a)
    -> EntityField b (Key a)
    -> SqlPersistT m [(Entity a, Entity b)]
join1ToM' idField fkField = select . from $ \(t1 `InnerJoin` t2) -> do
    on (t1 ^. idField ==. t2 ^. fkField)
    pure (t1, t2)

-- | join a one-to-many relationship and get all entities
-- where the first field is restricted by a where-clause
-- and the second field is collected as list of values in a map
join1ToMWhere
    :: (EntEqs a b, PersistField c, MonadIO m, Ord a)
    => EntityField a (Key a)
    -> EntityField b (Key a)
    -> EntityField a c
    -> c
    -> SqlPersistT m (Map (Entity a) [Entity b])
join1ToMWhere idField fkField field value =
    collectSnd <$> join1ToMWhere' idField fkField field value

-- | join a one-to-many relationship and get all entities
-- @
--   [(johnDoe, blogpost1), (johnDoe, blogpost2)] -- no map
-- @
join1ToMWhere'
    :: (EntEqs a b, PersistField c, MonadIO m)
    => EntityField a (Key a)
    -> EntityField b (Key a)
    -> EntityField a c
    -> c
    -> SqlPersistT m [(Entity a, Entity b)]
join1ToMWhere' idField fkField field value =
    select . from $ \(t1 `InnerJoin` t2) -> do
        on $ t1 ^. idField ==. t2 ^. fkField
        where_ $ t1 ^. field ==. val value
        pure (t1, t2)

-- | join a many-to-one relationship and get all entities
-- as keys and values in a map
-- @
--   blogPostMap <- joinMTo1 BlogPostAuthorId PersonId
--   liftIO . print $ Map.lookup blogpost1 blogPostMap -- johnDoe
--   liftIO . print $ Map.lookup blogpost2 blogPostMap -- johnDoe
-- @
joinMTo1
    :: (EntEqs a b, MonadIO m, Ord a)
    => EntityField a (Key b)
    -> EntityField b (Key b)
    -> SqlPersistT m (Map (Entity a) (Entity b))
joinMTo1 = (fmap Map.fromList .) . joinMTo1'

-- | join a many-to-one relationship and get all entities
joinMTo1'
    :: (EntEqs a b, MonadIO m)
    => EntityField a (Key b)
    -> EntityField b (Key b)
    -> SqlPersistT m [(Entity a, Entity b)]
joinMTo1' fkField idField = select . from $ \(t1 `InnerJoin` t2) -> do
    on (t1 ^. fkField ==. t2 ^. idField)
    pure (t1, t2)

-- | join a many-to-one relationship and query
-- where the first field is restricted by a where-clause
-- as keys and values in a map
-- @
--   blogPostMap <- joinMTo1Where BlogPostAuthorId
--                                PersonId
--                                BlogPostTitle
--                                "My fr1st p0st"
-- @
joinMTo1Where
    :: (EntEqs a b, PersistField c, MonadIO m, Ord a)
    => EntityField a (Key b)
    -> EntityField b (Key b)
    -> EntityField a c
    -> c
    -> SqlPersistT m (Map (Entity a) (Entity b))
joinMTo1Where fkField idField =
    (fmap Map.fromList .) . joinMTo1Where' fkField idField

-- | join a many-to-one relationship and query with where-clause
joinMTo1Where'
    :: (EntEqs a b, PersistField c, MonadIO m)
    => EntityField a (Key b)
    -> EntityField b (Key b)
    -> EntityField a c
    -> c
    -> SqlPersistT m [(Entity a, Entity b)]
joinMTo1Where' fkField idField field value =
    select . from $ \(t1 `InnerJoin` t2) -> do
        on $ t1 ^. fkField ==. t2 ^. idField
        where_ $ t1 ^. field ==. val value
        pure (t1, t2)

-- | join a many-to-many relationship
-- where the second field is collected as list of values in a map
joinMToM
    :: (EntEq a, EntEq b, EntEq c, MonadIO m, Ord a)
    => EntityField a (Key a)
    -> EntityField c (Key a)
    -> EntityField c (Key b)
    -> EntityField b (Key b)
    -> SqlPersistT m (Map (Entity a) [Entity b])
joinMToM idField1 fkField1 fkField2 idField2 =
    collectSnd <$> joinMToM' idField1 fkField1 fkField2 idField2

-- | join a many-to-many relationship
joinMToM'
    :: (EntEq a, EntEq b, EntEq c, MonadIO m)
    => EntityField a (Key a)
    -> EntityField c (Key a)
    -> EntityField c (Key b)
    -> EntityField b (Key b)
    -> SqlPersistT m [(Entity a, Entity b)]
joinMToM' idField1 fkField1 fkField2 idField2 =
    select . from $ \(t1 `InnerJoin` m2m `InnerJoin` t2) -> do
        on $ t2 ^. idField2 ==. m2m ^. fkField2
        on $ m2m ^. fkField1 ==. t1 ^. idField1
        pure (t1, t2)

-- | join a many-to-many relationship
-- where the first field is restricted by a where-clause
joinMToMWhere
    :: (EntEq a, EntEq b, EntEq c, PersistField d, MonadIO m, Ord a)
    => EntityField a (Key a)
    -> EntityField c (Key a)
    -> EntityField c (Key b)
    -> EntityField b (Key b)
    -> EntityField a d
    -> d
    -> SqlPersistT m (Map (Entity a) [Entity b])
joinMToMWhere idField1 fkField1 fkField2 idField2 field value =
    collectSnd
        <$> joinMToMWhere' idField1 fkField1 fkField2 idField2 field value

-- | join a many-to-many relationship
-- where the first field is restricted by a where-clause
joinMToMWhere'
    :: (EntEq a, EntEq b, EntEq c, PersistField d, MonadIO m)
    => EntityField a (Key a)
    -> EntityField c (Key a)
    -> EntityField c (Key b)
    -> EntityField b (Key b)
    -> EntityField a d
    -> d
    -> SqlPersistT m [(Entity a, Entity b)]
joinMToMWhere' idField1 fkField1 fkField2 idField2 field value =
    select . from $ \(t1 `InnerJoin` m2m `InnerJoin` t2) -> do
        on $ t2 ^. idField2 ==. m2m ^. fkField2
        on $ m2m ^. fkField1 ==. t1 ^. idField1
        where_ $ t1 ^. field ==. val value
        pure (t1, t2)
