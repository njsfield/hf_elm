module Data.Profile exposing (Profile, PatientId, decoder)

import Json.Decode as Decode exposing (Decoder, at)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, custom)
import UrlParser
import Util exposing ((=>))


-- Patient model for patient profile
-- Extract patient ID (from unique identifier)
-- Basic info and available addresses


type alias Profile =
    { patient_id : PatientId
    , prefix : String
    , first_name : String
    , last_name : String
    , active : Bool
    , dob : String
    , addresses : List PatientAddress
    }


type alias PatientId =
    String



-- Each patient


type alias PatientAddress =
    { line1 : String
    , country : String
    , zipCode : String
    }



{- Main Decoder
   Patient_id is resolved by retrieving the 'value' key from the first
   array item in the 'identifiers' key

   From there, other fields are simple string/bool values that are
   extracted and used to build a Patient model

   Note: Patients may have multiple addresses, so an address decoder
   is applied to all address list items
-}


decoder : Decoder Profile
decoder =
    decode Patient
        |> custom (at [ "identifiers" ] <| Decode.index 0 <| at [ "value" ] Decode.string)
        |> required "prefix" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "active" Decode.bool
        |> required "dateOfBirth" Decode.string
        |> custom (at [ "addresses" ] <| Decode.list addressDecoder)


addressDecoder : Decoder PatientAddress
addressDecoder =
    decode PatientAddress
        |> required "line1" Decode.string
        |> required "country" Decode.string
        |> required "zipCode" Decode.string
