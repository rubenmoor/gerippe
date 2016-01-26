{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Gerippe
  ( module Database.Gerippe
  , module Database.Gerippe.Utils
  , module Database.Esqueleto
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Map               (Map)
import qualified Data.Map               as Map

import           Database.Esqueleto     hiding (entityId)

import           Database.Gerippe.Utils (collectSnd)

-- | integer representation of sql key
keyToId :: ( ToBackendKey SqlBackend record
           , Integral a
           ) => Key record -> a
keyToId = fromIntegral . fromSqlKey

-- | integer representation of sql key of a database entity
entityId :: (ToBackendKey SqlBackend record
            , Integral a
            ) => Entity record -> a
entityId = keyToId . entityKey

-- | get using the integer representation of sql key
getById :: ( PersistEntityBackend b ~ SqlBackend
           , ToBackendKey SqlBackend b
           , MonadIO m
           , Integral a
           ) => a -> SqlPersistT m (Maybe b)
getById = get . toSqlKey . fromIntegral

-- | retrieve all entities by means of type inference
getAll :: ( PersistEntityBackend a ~ SqlBackend
          , PersistEntity a
          , MonadIO m
          ) => SqlPersistT m [Entity a]
getAll = select . from $ pure

-- | convenience function for simple queries with one where-clause
getWhere :: ( PersistEntity a
            , PersistEntityBackend a ~ SqlBackend
            , PersistField b
            , MonadIO m
            ) => EntityField a b -> b -> SqlPersistT m [Entity a]
getWhere field value = select . from $ \t -> do
  where_ $ t ^. field ==. val value
  pure t

-- | join a one-to-many relationship and get all entities
-- in a map
join1ToM :: ( PersistEntity a
            , PersistEntity b
            , PersistEntityBackend a ~ SqlBackend
            , PersistEntityBackend b ~ SqlBackend
            , MonadIO m
            , Ord a
            ) => EntityField a (Key a) -> EntityField b (Key a)
              -> SqlPersistT m (Map (Entity a) [Entity b])
join1ToM = (fmap collectSnd .) . join1ToM'

-- | join a one-to-many relationship and get all entities
join1ToM' :: ( PersistEntity a
             , PersistEntity b
             , PersistEntityBackend a ~ SqlBackend
             , PersistEntityBackend b ~ SqlBackend
             , MonadIO m
             ) => EntityField a (Key a) -> EntityField b (Key a)
               -> SqlPersistT m [(Entity a, Entity b)]
join1ToM' idField fkField =
  select . from $ \(t1 `InnerJoin` t2) -> do
    on (t1 ^. idField ==. t2 ^. fkField)
    pure (t1, t2)

-- | join a many-to-one relationship and get all entities
-- in a map
joinMTo1 :: ( PersistEntity a
            , PersistEntity b
            , PersistEntityBackend a ~ SqlBackend
            , PersistEntityBackend b ~ SqlBackend
            , MonadIO m
            , Ord a
            ) => EntityField a (Key b) -> EntityField b (Key b)
              -> SqlPersistT m (Map (Entity a) (Entity b))
joinMTo1 = (fmap Map.fromList .) . joinMTo1'

-- | join a many-to-one relationship and get all entities
joinMTo1' :: ( PersistEntity a
             , PersistEntity b
             , PersistEntityBackend a ~ SqlBackend
             , PersistEntityBackend b ~ SqlBackend
             , MonadIO m
             ) => EntityField a (Key b) -> EntityField b (Key b)
               -> SqlPersistT m [(Entity a, Entity b)]
joinMTo1' fkField idField =
  select . from $ \(t1 `InnerJoin` t2) -> do
    on (t1 ^. fkField ==. t2 ^. idField)
    pure (t1, t2)

-- | join a many-to-one relationship and query with where-clause
-- in a map
joinMTo1Where :: ( PersistEntity a
                 , PersistEntity b
                 , PersistEntityBackend a ~ SqlBackend
                 , PersistEntityBackend b ~ SqlBackend
                 , PersistField c
                 , MonadIO m
                 , Ord a
                 ) => EntityField a (Key b) -> EntityField b (Key b)
                   -> EntityField a c -> c
                   -> SqlPersistT m (Map (Entity a) (Entity b))
joinMTo1Where fkField idField = (fmap Map.fromList .) . joinMTo1Where' fkField idField

-- | join a many-to-one relationship and query with where-clause
joinMTo1Where' :: ( PersistEntity a
                  , PersistEntity b
                  , PersistEntityBackend a ~ SqlBackend
                  , PersistEntityBackend b ~ SqlBackend
                  , PersistField c
                  , MonadIO m
                  ) => EntityField a (Key b) -> EntityField b (Key b)
                    -> EntityField a c -> c
                    -> SqlPersistT m [(Entity a, Entity b)]
joinMTo1Where' fkField idField field value =
  select . from $ \(t1 `InnerJoin` t2) -> do
    on     $ t1 ^. fkField ==. t2 ^. idField
    where_ $ t1 ^. field   ==. val value
    pure (t1, t2)
