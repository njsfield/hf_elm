module Profile exposing (get)

import Data.Session as Session exposing (Session)
import Data.Profile as Profile exposing (PatientId)


{- PatientQuery
   Alias for single patient Id needed
-}


type alias ProfileQuery =
    PatientId



{- get
   Create Http request using AuthToken & patient query record
-}


get : Maybe AuthToken -> ProfileQuery -> Http.Request Patient
get maybeToken profileQuery =
    let
        expect =
            (Profile.decoder)
                |> Http.expectJson
    in
        apiUrl ("/patientId/" ++ profileQuery)
            |> HttpBuilder.get
            |> HttpBuilder.withExpect expect
            |> withAuthorization maybeToken
            |> HttpBuilder.toRequest
