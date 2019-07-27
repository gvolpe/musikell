musikell
========

Example of a `GraphQL` application backed by `Neo4j` using [morpheus](https://morpheusgraphql.com/) and [hasbolt](https://hackage.haskell.org/package/hasbolt), respectively.

### Run it locally

```
docker run -it --rm -p7474:7474 -p7687:7687 --env NEO4J_AUTH=neo4j/test neo4j:latest
```

### GraphQL API via Http

Go to `localhost:3000/api` and start sending queries. Eg. using `Insomnia`:

![insomnia](insomnia.png)

### Queries

##### Get Artist

```
query GetArtist {
  artist (name: "Tool") {
    name
    spotifyId
  }
}
```

##### Get Albums by Artist

```
query GetAlbumsByArtist {
  albumsByArtist (name: "Opeth") {
    name
    yearOfReleased
    totalLength
  }
}
```

### Mutations

##### Create Artist

```
mutation CreateArtist {
  newArtist(names: ["Porcupine Tree", "Puscifer"]) {
    name
    spotifyId
  }
}
```

##### Create Artist Albums

```
mutation CreateArtistAlbums {
  newAlbums(spotifyId: "0ybFZ2Ab08V8hueghSXm6E") {
    name
    yearOfRelease
    totalLength
  }
}
```
