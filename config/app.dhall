let Env = < Test : {} | Prod : {} >

let neo4jHostEnv = λ(env : Env) →
  merge { Test = λ(x : {}) →  "127.0.0.1", Prod = λ(x : {}) →  "0.0.0.0" } env

let makeNeo4jConfig = λ(env : Env) →
  { neo4jHost = "${neo4jHostEnv env}"
  , neo4jPort = 7687
  , neo4jUser = "neo4j"
  , neo4jPassword = "test"
  , neo4jSecure = False
  }

let makeHttpServerConfig = λ(env : Env) →
  { serverPort = 3000
  }

let makeSpotifyConfig = λ(env : Env) →
  { apiKey = "${env:SPOTIFY_API_KEY as Text}"
  , apiUri = "https://api.spotify.com/v1"
  , apiAuth = "https://accounts.spotify.com/api/token"
  }

let makeConfig = λ(env : Env) →
  { neo4j = makeNeo4jConfig env
  , httpServer = makeHttpServerConfig env
  , spotify = makeSpotifyConfig env
  }

in makeConfig ( Env.Test {=} )
