{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j repository for Song, including interface and cypher queries.
module Repository.Song
  ( mkSongRepository
  , SongRepository(..)
  )
where

import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Monoid                    ( (<>) )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Mapper
import           Utils                          ( headMaybe )

data SongRepository m = SongRepository
  { findSong :: SongName -> m (Maybe Song)
  , findSongsByAlbum :: AlbumName -> m [Song]
  , findSongsByArtist :: ArtistName -> m [Song]
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

findSong' :: SongName -> Pipe -> IO (Maybe Song)
findSong' t pipe = toEntityMaybe "s" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (s:Song) WHERE s.title CONTAINS {title} RETURN s"
    (fromList [("title", T (unSongName t))])

findSongsByAlbum' :: AlbumName -> Pipe -> IO [Song]
findSongsByAlbum' a pipe = toEntityList "s" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (b:Album)-[:HAS_SONG]->(s:Song) WHERE b.name CONTAINS {albumName} RETURN s"
    (fromList [("albumName", T (unAlbumName a))])

findSongsByArtist' :: ArtistName -> Pipe -> IO [Song]
findSongsByArtist' a pipe = toEntityList "s" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (a:Artist)-[:HAS_SONG]->(s:Song) WHERE a.name CONTAINS {artistName} RETURN s"
    (fromList [("artistName", T (unArtistName a))])

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
