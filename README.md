# Auto-Generated OpenAPI Bindings to `DaDaPushPublic`

The library in `lib` provides auto-generated-from-OpenAPI bindings to the DaDaPushPublic API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

Otherwise, if you already have a Stack project, you can include this package under the `packages` key in your `stack.yaml`:
```yaml
packages:
- location:
    git: https://github.com/yourGitOrg/yourGitRepo
    commit: somecommit
```

## Main Interface

The main interface to this library is in the `DaDaPushPublic.API` module, which exports the DaDaPushPublicBackend type. The DaDaPushPublicBackend
type can be used to create and define servers and clients for the API.

## Creating a Client

A client can be created via the `createDaDaPushPublicClient` function, which will generate a function for every endpoint of the API.
Then these functions can be invoked with `runDaDaPushPublicClientWithManager` or more conveniently with `callDaDaPushPublicClient`
(depending if you want an `Either` back or you want to catch) to access the API endpoint they refer to, if the API is served
at the `url` you specified.

For example, if `localhost:8080` is serving the DaDaPushPublic API, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import DaDaPushPublic.API as API

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv, mkClientEnv, parseBaseUrl)


main :: IO ()
main = do
  -- Configure the BaseUrl for the client
  url <- parseBaseUrl "http://localhost:8080/"

  -- You probably want to reuse the Manager across calls, for performance reasons
  manager <- newManager tlsManagerSettings

  -- Create the client (all endpoint functions will be available)
  DaDaPushPublicBackend{..} <- API.createDaDaPushPublicClient

  -- Any DaDaPushPublic API call can go here, e.g. here we call `getSomeEndpoint`
  API.callDaDaPushPublic (mkClientEnv manager url) getSomeEndpoint
```

## Creating a Server

In order to create a server, you must use the `runDaDaPushPublicServer` function. However, you unlike the client, in which case you *got* a `DaDaPushPublicBackend`
from the library, you must instead *provide* a `DaDaPushPublicBackend`. For example, if you have defined handler functions for all the
functions in `DaDaPushPublic.Handlers`, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import DaDaPushPublic.API

-- A module you wrote yourself, containing all handlers needed for the DaDaPushPublicBackend type.
import DaDaPushPublic.Handlers

-- Run a DaDaPushPublic server on localhost:8080
main :: IO ()
main = do
  let server = DaDaPushPublicBackend{..}
      config = Config "http://localhost:8080/"
  runDaDaPushPublicServer config server
```
