{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Lib
import Control.Monad.IO.Class
import Data.Morpheus.Types
import Data.Morpheus
import GHC.Generics
import Web.Scotty
import Data.Morpheus.Kind
import Data.Text (Text)

data Identifier
  = MusicianId Int 
  | AlbumId Int 
  | SongId Int 
  | ReviewId Int
  deriving (Generic)

instance GQLScalar Identifier where 
  -- bblank because it's demo purposes

instance GQLType Identifier where
  type KIND Identifier = SCALAR

data Musician m 
  = Musician 
  { id  :: Identifier
  , albums :: m [Album m]
  , songs :: m [Song m]
  } deriving (Generic, GQLType)

data Album m
  = Album 
  { id :: Identifier
  , name :: Text 
  , tracks :: m [Song m]
  , musician :: m (Musician m)
  }  deriving (Generic, GQLType)

data Song m
  = Song
  { id :: Identifier
  , name :: Text 
  , album :: m (Album m)
  , musician :: m (Musician m)
  } deriving (Generic, GQLType)

musician' = Musician (MusicianId 10) (pure [album']) (pure songs')

songs' =
  [ Song (SongId 1) "My Cool Song" (pure album') (pure musician')
  , Song (SongId 1) "My Other Song" (pure album') (pure musician')
  ]

album' = Album (AlbumId 1) "My cool album" (pure songs') (pure musician')

data AnyNode m
  = MusicianNode (Musician m)
  | AlbumNode (Album m)
  | SongNode (Song m)
  deriving (Generic, GQLType)

data FindArgs = FindArgs { getId :: Identifier } deriving (Generic, GQLType)

data Query m
  = Query 
  { findNode :: FindArgs -> m (AnyNode m) }
  deriving (Generic, GQLType)

byId :: (Applicative m) => Identifier -> m (AnyNode m)
byId (MusicianId _) = pure $ MusicianNode musician'
byId (AlbumId _) = pure $ AlbumNode album' 
byId (SongId _) = pure $ SongNode (head songs')

query' = Query (\(FindArgs f) -> byId f)

rootResolver :: GQLRootResolver IO (Event Int Int) Query Undefined Undefined 
rootResolver = GQLRootResolver { queryResolver = query', mutationResolver = Undefined, subscriptionResolver = Undefined }

main :: IO ()
main = scotty 5000 $ do 
  post "/graphql" $ do 
    setHeader "Content-Type" "application/json"
    b <- body 
    response <- liftIO $ interpreter rootResolver b
    raw response 
