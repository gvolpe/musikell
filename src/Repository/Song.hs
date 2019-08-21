{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- | The Neo4j repository for Song, including interface and cypher queries.
module Repository.Song
  ( mkSongRepository
  , SongRepository(..)
  )
where

import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( listToMaybe )
import           Data.Monoid                    ( (<>) )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Repository.Entity
import           Repository.Mapper

data SongRepository m = SongRepository
  { findSong :: SongName -> m (Maybe Song)
  , findSongsByAlbum :: AlbumName -> m [Song]
  , findSongsByArtist :: ArtistName -> m [Song]
  , createSong :: ArtistSpotifyId -> AlbumSpotifyId -> Song -> m ()
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
findSong' SongName {..} pipe = toEntityMaybe "s" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (s:Song) WHERE s.title CONTAINS {title} RETURN s"
    (fromList [("title", T unSongName)])

findSongsByAlbum' :: AlbumName -> Pipe -> IO [Song]
findSongsByAlbum' AlbumName {..} pipe = toEntityList "s" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (b:Album)-[:HAS_SONG]->(s:Song) WHERE b.name CONTAINS {albumName} RETURN s"
    (fromList [("albumName", T unAlbumName)])

findSongsByArtist' :: ArtistName -> Pipe -> IO [Song]
findSongsByArtist' ArtistName {..} pipe = toEntityList "s" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (a:Artist)-[:HAS_SONG]->(s:Song) WHERE a.name CONTAINS {artistName} RETURN s"
    (fromList [("artistName", T unArtistName)])

createSong' :: ArtistSpotifyId -> AlbumSpotifyId -> Song -> Pipe -> IO ()
createSong' ArtistSpotifyId {..} AlbumSpotifyId {..} Song {..} pipe = void . run pipe $ queryP
  (  "MATCH (a:Artist), (b:Album) WHERE a.spotifyId={artistId} AND b.spotifyId={albumId} "
  <> "CREATE (s:Song { no : {no}, title : {title}, duration : {duration} }) "
  <> "CREATE (b)-[hb:HAS_SONG]->(s) "
  <> "CREATE (a)-[ha:HAS_SONG]->(s) "
  <> "CREATE (s)-[fb:FROM_ALBUM]->(b) "
  <> "CREATE (s)-[fa:FROM_ARTIST]->(a) "
  <> "RETURN ID(s)"
  )
  (fromList
    [ ("artistId", T unArtistSpotifyId)
    , ("albumId" , T unAlbumSpotifyId)
    , ("no"      , I songNo)
    , ("title"   , T songTitle)
    , ("duration", I songDuration)
    ]
  )
