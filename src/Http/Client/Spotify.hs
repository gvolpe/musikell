{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}

module Http.Client.Spotify
  ( SpotifyClient(..)
  , mkSpotifyClient
  )
where

import           Config
import           Control.Lens
import           Data.Aeson                     ( FromJSON )
import           Data.ByteString.Lazy           ( ByteString )
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
  , getArtistAlbums :: AccessToken -> ArtistId -> m AlbumResponse
  , searchArtist :: AccessToken -> ArtistName -> m ArtistResponse
  }

mkSpotifyClient :: SpotifyConfig -> IO (SpotifyClient IO)
mkSpotifyClient cfg = pure SpotifyClient
  { login           = login' cfg
  , getArtistAlbums = getArtistAlbums' cfg
  , searchArtist    = searchArtist' cfg
  }

login' :: SpotifyConfig -> IO AccessToken
login' c =
  let url  = unpack (apiAuth c)
      key  = apiKey c
      frm  = header "Content-Type" .~ ["application/x-www-form-urlencoded"]
      auth = header "Authorization" .~ ["Basic " <> encodeUtf8 (apiKey c)]
      ops  = defaults & frm & auth
      body = ["grant_type" := ("client_credentials" :: Text)]
  in  reqP ops url body

getArtistAlbums' :: SpotifyConfig -> AccessToken -> ArtistId -> IO AlbumResponse
getArtistAlbums' c t a =
  let url   = apiUri c <> "/artists/" <> unArtistId a <> "/albums"
      token = "Bearer " <> encodeUtf8 (unAccessToken t)
      auth  = header "Authorization" .~ [token]
      ops   = defaults & param "limit" .~ ["50"] & auth
  in  do
        putStrLn $ "Retrieving Spotify data for artist " <> show (unArtistId a)
        req ops url

searchArtist' :: SpotifyConfig -> AccessToken -> ArtistName -> IO ArtistResponse
searchArtist' c t n =
  let url   = apiUri c <> "/search"
      token = "Bearer " <> encodeUtf8 (unAccessToken t)
      auth  = header "Authorization" .~ [token]
      query = param "q" .~ [unArtistName n]
      atype = param "type" .~ ["artist"]
      limit = param "limit" .~ ["1"]
      ops   = defaults & query & atype & limit & auth
  in  do
        putStrLn $ "Find artist by name on Spotify " <> show (unArtistName n)
        req ops url

req :: forall a . FromJSON a => Options -> Text -> IO a
req ops url =
  (^. responseBody) <$> (asJSON =<< getWith ops (unpack url) :: IO (Response a))

reqP :: forall a b . (FromJSON a, Postable b) => Options -> String -> b -> IO a
reqP ops url body =
  (^. responseBody) <$> (asJSON =<< postWith ops url body :: IO (Response a))
