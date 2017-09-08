module Data.AuthToken exposing (AuthToken, withAuthorization)

import HttpBuilder exposing (RequestBuilder, withHeader)


type AuthToken
    = String



{- withAuthorization
   Initialise a GET request builder
   by preparing Authorization (Bearer) header
-}


withAuthorization : Maybe AuthToken -> RequestBuilder a -> RequestBuilder a
withAuthorization maybeToken builder =
    case maybeToken of
        Just token ->
            builder
                |> withHeader "Authorization" ("Bearer " ++ token)

        Nothing ->
            builder
