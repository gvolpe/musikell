{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}

module Http.Client.Spotify
  ( getArtistAlbums
  , login
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

login :: SpotifyConfig -> IO AccessToken
login c =
  let url  = unpack (apiAuth c)
      key  = apiKey c
      frm  = header "Content-Type" .~ ["application/x-www-form-urlencoded"]
      auth = header "Authorization" .~ ["Basic " <> encodeUtf8 (apiKey c)]
      ops  = defaults & frm & auth
      body = ["grant_type" := ("client_credentials" :: Text)]
  in  reqP ops url body

getArtistAlbums
  :: SpotifyConfig -> AccessToken -> ArtistId -> IO AlbumResponse
getArtistAlbums c t a =
  let url   = apiUri c <> "/artists/" <> unArtistId a <> "/albums"
      token = "Bearer " <> encodeUtf8 (unAccessToken t)
      auth  = header "Authorization" .~ [token]
      ops   = defaults & param "limit" .~ ["50"] & auth
  in  req ops url

req :: forall a . FromJSON a => Options -> Text -> IO a
req ops url =
  (^. responseBody) <$> (asJSON =<< getWith ops (unpack url) :: IO (Response a))

-- TODO: Raise issue in wreq to export Postable class
reqP :: forall a . FromJSON a => Options -> String -> [FormParam] -> IO a
reqP ops url body =
  (^. responseBody) <$> (asJSON =<< postWith ops url body :: IO (Response a))
