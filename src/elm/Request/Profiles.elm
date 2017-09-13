module Profiles exposing (ProfilessQuery, initialPatientQuery, sortOptions, get)

import Data.Session as Session exposing (Session)
import Data.Profile as Profile exposing (Profile)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)


{- ProfilesQuery
   We need to contain our patient
   query options in a record that allows
   each query field to hold a label (displayed)
   to user and the (Maybe) value it represents

   page & size need to be converted to ints
-}


type alias ProfilesQuery =
    { firstName : Labelled
    , lastName : Labelled
    , zipCode : Labelled
    , sort : Labelled
    , page : Labelled
    , size : Labelled
    }


type alias Labelled =
    { label : String
    , key : String
    , value : Maybe String
    }



{--sortOptions
   For use in the patient table view
--}


sortOptions =
    [ (Labelled "First Name (Ascending)" "sort" (Just "firstName%20ASC"))
    , (Labelled "First Name (Descending)" "sort" (Just "firstName%20DESC"))
    , (Labelled "Last Name (Ascending)" "sort" (Just "lastName%20ASC"))
    , (Labelled "Last Name (Descending)" "sort" (Just "lastName%20DESC"))
    , (Labelled "Date Of Birth (Ascending)" "sort" (Just "dateOfBirth%20ASC"))
    , (Labelled "Date Of Birth (Descending)" "sort" (Just "dateOfBirth%20DESC"))
    ]



{- initialProfilesQuery -}


initialProfilesQuery =
    { firstName = Labelled "First Name" "firstName" Nothing
    , lastName = Labelled "Last Name" "lastName" Nothing
    , zipCode = Labelled "Zip Code" "zipCode" Nothing
    , sort = Labelled "Sort By" "sort" Nothing
    , page = Labelled "Page" "page" Nothing
    , size = Labelled "Size" "size" Nothing
    }



{- get
   Create Http request using AuthToken & profilesQuery record
-}


get : Maybe AuthToken -> ProfilesQuery -> Http.Request (List Profile)
get maybeToken profilesQuery =
    let
        expect =
            (Decode.list Profile.decoder)
                |> Decode.field "content"
                |> Http.expectJson
    in
        apiUrl ("")
            |> HttpBuilder.get
            |> HttpBuilder.withQueryParams (queryToList profilesQuery)
            |> HttpBuilder.withExpect expect
            |> withAuthorization maybeToken
            |> HttpBuilder.toRequest



{- queryToList -}


queryToList : ProfilesQuery -> List ( String, String )
queryToList { firstName, lastName, zipCode, sort, page, size } =
    [ firstName
    , lastName
    , zipCode
    , sort
    , page
    , size
    ]
        |> List.filter (\{ value } -> value /= Nothing)
        |> List.map (\{ value, key } -> value => (Maybe.withDefault "" key))
