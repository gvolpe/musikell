let Env = < Test : {} | Prod : {} >

let neo4jEnv = λ(env : Env) →
  merge { Test = λ(x : {}) →  "127.0.0.1", Prod = λ(x : {}) →  "127.0.0.1" } env

let makeNeo4jConfig = λ(env : Env) →
  { neo4jUri = "${neo4jEnv env}"
  , neo4jUser = "neo4j"
  , neo4jPassword = "test"
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
