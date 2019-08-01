{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j repository for Album, including interface and cypher queries.
module Repository.Album
  ( mkAlbumRepository
  , AlbumRepository(..)
  )
where

import           Control.Monad.Catch            ( throwM )
import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( listToMaybe )
import           Data.Monoid                    ( (<>) )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Repository.Entity
import           Repository.Mapper
import           Repository.Song

data AlbumRepository m = AlbumRepository
  { findAlbum :: AlbumName -> m (Maybe Album)
  , findAlbumsByArtist :: ArtistName -> m [Album]
  , findAlbumsByArtistId :: ArtistSpotifyId -> m [Album]
  , createAlbum :: ArtistSpotifyId -> Album -> m (Maybe AlbumSpotifyId)
  }

mkAlbumRepository :: Pool Pipe -> IO (AlbumRepository IO)
mkAlbumRepository pool = pure $ AlbumRepository
  { findAlbum            = withResource pool . findAlbum'
  , findAlbumsByArtist   = withResource pool . findAlbumsByArtist'
  , findAlbumsByArtistId = withResource pool . findAlbumsByArtistId'
  , createAlbum          = \spotifyId album ->
                             withResource pool (createAlbum' spotifyId album)
  }

findAlbum' :: AlbumName -> Pipe -> IO (Maybe Album)
findAlbum' a pipe = toEntityMaybe "b" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (b:Album) WHERE b.name CONTAINS {title} RETURN b"
    (fromList [("title", T (unAlbumName a))])

findAlbumsByArtistId' :: ArtistSpotifyId -> Pipe -> IO [Album]
findAlbumsByArtistId' sid pipe = toEntityList "b" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (a:Artist)-[:HAS_ALBUM]->(b:Album) WHERE a.spotifyId={artistId} RETURN b"
    (fromList [("artistId", T (unArtistSpotifyId sid))])

findAlbumsByArtist' :: ArtistName -> Pipe -> IO [Album]
findAlbumsByArtist' a pipe = toEntityList "b" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (a:Artist)-[:HAS_ALBUM]->(b:Album) WHERE a.name CONTAINS {artistName} RETURN b"
    (fromList [("artistName", T (unArtistName a))])

createAlbum' :: ArtistSpotifyId -> Album -> Pipe -> IO (Maybe AlbumSpotifyId)
createAlbum' artistId a pipe = do
  records <- run pipe $ queryP
    (  "MATCH (a:Artist) WHERE a.spotifyId={artistId} "
    <> "CREATE (b:Album { spotifyId : {albumId}, name : {name}, released : {released}, length : {length} }) "
    <> "CREATE (a)-[h:HAS_ALBUM]->(b) "
    <> "CREATE (b)-[f:FROM_ARTIST]->(a) "
    <> "RETURN b.spotifyId"
    )
    (fromList
      [ ("artistId", T (unArtistSpotifyId artistId))
      , ("albumId" , T (unAlbumSpotifyId $ albumSpotifyId a))
      , ("name"    , T (albumName a))
      , ("released", I (albumReleasedYear a))
      , ("length"  , I (albumTotalLength a))
      ]
    )
  pure $ listToMaybe records >>= toAlbumSpotifyId
