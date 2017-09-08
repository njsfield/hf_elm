module Data.Patient exposing (Patient, PatientId, patientDecoder, patientsDecoder)

import Json.Decode as Decode exposing (Decoder, at)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, custom)


-- Patient model for patient profile
-- Extract patient ID (from unique identifier)
-- Basic info and available addresses


type alias Patient =
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



-- patientsDecoder (Main)
-- Parse list of patients at "content" field


patientsDecoder : Decode (List Patient)
patientsDecoder =
    at [ "content" ] (Decode.list patientDecoder)



{- Patient Decoder
   Patient_id is resolved by retrieving the 'value' key from the first
   array item in the 'identifiers' key

   From there, other fields are simple string/bool values that are
   extracted and used to build a Patient model

   Note: Patients may have multiple addresses, so an address decoder
   is applied to all address list items
-}


patientDecoder : Decoder Patient
patientDecoder =
    decode Patient
        |> custom (at [ "identifiers" ] (listHeadDecoder <| at [ "value" ] Decode.string))
        |> required "prefix" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "active" Decode.bool
        |> custom (at [ "addresses" ] (Decode.list addressDecoder))


addressDecoder : Decoder PatientAddress
addressDecoder =
    decode PatientAddress
        |> required "line1" Decode.string
        |> required "country" Decode.string
        |> required "zipCode" Decode.string


listHeadDecoder : Decoder String -> Decoder String
listHeadDecoder nextDecoder =
    Decode.map (\lst -> Maybe.withDefault "" <| List.head lst) <| Decode.list nextDecoder
