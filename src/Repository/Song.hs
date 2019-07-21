{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j repository for Song, including interface and cypher queries.
module Repository.Song
  ( mkSongRepository
  , SongRepository(..)
  )
where

import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Mapper
import           Utils                          ( headMaybe )

data SongRepository m = SongRepository
  { findSong :: Text -> m (Maybe Song)
  , findSongsByAlbum :: Text -> m [Song]
  , findSongsByArtist :: Text -> m [Song]
  , createSong :: ArtistId -> AlbumId -> Song -> m ()
  }

mkSongRepository :: Pool Pipe -> IO (SongRepository IO)
mkSongRepository pool = pure $ SongRepository
  { findSong          = withResource pool . findSong'
  , findSongsByAlbum  = withResource pool . findSongsByAlbum'
  , findSongsByArtist = withResource pool . findSongsByArtist'
  , createSong        = \artistId albumId song ->
                          withResource pool (createSong' artistId albumId song)
  }

findSong' :: Text -> Pipe -> IO (Maybe Song)
findSong' t pipe = do
  records <- run pipe $ queryP
    "MATCH (s:Song) WHERE s.title CONTAINS {title} RETURN s"
    (fromList [("title", T t)])
  pure $ headMaybe records >>= toNodeProps "s" >>= toEntity

findSongsByAlbum' :: Text -> Pipe -> IO [Song]
findSongsByAlbum' albumName pipe = do
  records <- run pipe $ queryP
    "MATCH (b:Album)-[:HAS_SONG]->(s:Song) WHERE b.name CONTAINS {albumName} RETURN s"
    (fromList [("albumName", T albumName)])
  pure $ records >>= (\r -> maybeToList ((toNodeProps "s" r :: Maybe NodeProps) >>= toEntity))

findSongsByArtist' :: Text -> Pipe -> IO [Song]
findSongsByArtist' artistName pipe = do
  records <- run pipe $ queryP
    "MATCH (a:Artist)-[:HAS_SONG]->(s:Song) WHERE a.name CONTAINS {artistName} RETURN s"
    (fromList [("artistName", T artistName)])
  pure $ records >>= (\r -> maybeToList ((toNodeProps "s" r :: Maybe NodeProps) >>= toEntity))

createSong' :: ArtistId -> AlbumId -> Song -> Pipe -> IO ()
createSong' artistId albumId s pipe = void . run pipe $ queryP
  (  "MATCH (a:Artist), (b:Album) WHERE ID(a)={artistId} AND ID(b)={albumId} "
  <> "CREATE (s:Song { no : {no}, title : {title}, duration : {duration} }) "
  <> "CREATE (b)-[hb:HAS_SONG]->(s) "
  <> "CREATE (a)-[ha:HAS_SONG]->(s) "
  <> "CREATE (s)-[fb:FROM_ALBUM]->(b) "
  <> "CREATE (s)-[fa:FROM_ARTIST]->(a) "
  <> "RETURN ID(s)"
  )
  (fromList
    [ ("artistId", I (unArtistId artistId))
    , ("albumId" , I (unAlbumId albumId))
    , ("no"      , I (songNo s))
    , ("title"   , T (songTitle s))
    , ("duration", I (songDuration s))
    ]
  )
