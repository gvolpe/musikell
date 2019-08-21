{-# LANGUAGE OverloadedStrings, RecordWildCards, RankNTypes, ScopedTypeVariables #-}

module Http.Client.Spotify
  ( SpotifyClient(..)
  , mkSpotifyClient
  )
where

import           Config
import           Control.Lens
import           Data.Aeson                     ( FromJSON )
import qualified Data.ByteString.Internal      as C
import           Data.Monoid                    ( (<>) )
import           Data.Text
import           Data.Text.Encoding             ( encodeUtf8 )
import           GHC.Generics                   ( Generic )
import           GHC.Natural                    ( naturalToInt
                                                , naturalToInteger
                                                )
import           Http.Client.Params
import           Http.Client.Response
import           Network.Wreq
import           Network.Wreq.Types             ( Postable )

data SpotifyClient m = SpotifyClient
  { login :: m AccessToken
  , getAlbumTracks :: AccessToken -> AlbumId -> m TrackResponse
  , getArtistAlbums :: AccessToken -> ArtistId -> m AlbumResponse
  , searchArtist :: AccessToken -> ArtistName -> m ArtistResponse
  }

mkSpotifyClient :: SpotifyConfig -> IO (SpotifyClient IO)
mkSpotifyClient cfg = pure SpotifyClient
  { login           = login' cfg
  , getAlbumTracks  = getAlbumTracks' cfg
  , getArtistAlbums = getArtistAlbums' cfg
  , searchArtist    = searchArtist' cfg
  }

login' :: SpotifyConfig -> IO AccessToken
login' SpotifyConfig {..} =
  let url  = unpack apiAuth
      frm  = header "Content-Type" .~ ["application/x-www-form-urlencoded"]
      auth = header "Authorization" .~ ["Basic " <> encodeUtf8 apiKey]
      ops  = defaults & frm & auth
      body = ["grant_type" := ("client_credentials" :: Text)]
  in  reqP ops url body

getArtistAlbums' :: SpotifyConfig -> AccessToken -> ArtistId -> IO AlbumResponse
getArtistAlbums' SpotifyConfig {..} t a@ArtistId {..} =
  let url = apiUri <> "/artists/" <> unArtistId <> "/albums"
      ops = defaults & param "limit" .~ ["50"] & authHeader (token t)
  in  do
        putStrLn $ "Retrieving Spotify artist albums for " <> show a
        req ops url

getAlbumTracks' :: SpotifyConfig -> AccessToken -> AlbumId -> IO TrackResponse
getAlbumTracks' SpotifyConfig {..} t a@AlbumId {..} =
  let url = apiUri <> "/albums/" <> unAlbumId <> "/tracks"
      ops = defaults & param "limit" .~ ["50"] & authHeader (token t)
  in  do
        putStrLn $ "Retrieving Spotify album tracks for " <> show unAlbumId
        req ops url

searchArtist' :: SpotifyConfig -> AccessToken -> ArtistName -> IO ArtistResponse
searchArtist' SpotifyConfig {..} t n@ArtistName {..} =
  let url   = apiUri <> "/search"
      query = param "q" .~ [unArtistName]
      atype = param "type" .~ ["artist"]
      limit = param "limit" .~ ["1"]
      ops   = defaults & query & atype & limit & authHeader (token t)
  in  do
        putStrLn $ "Find artist on Spotify by " <> show n
        req ops url

token :: AccessToken -> C.ByteString
token t = "Bearer " <> encodeUtf8 (unAccessToken t)

authHeader :: C.ByteString -> Options -> Options
authHeader t = header "Authorization" .~ [t]

req :: forall a . FromJSON a => Options -> Text -> IO a
req ops url =
  (^. responseBody) <$> (asJSON =<< getWith ops (unpack url) :: IO (Response a))

reqP :: forall a b . (FromJSON a, Postable b) => Options -> String -> b -> IO a
reqP ops url body =
  (^. responseBody) <$> (asJSON =<< postWith ops url body :: IO (Response a))
