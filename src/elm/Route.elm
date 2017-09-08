module Route exposing (Route(..), fromLocation, href, modifyUrl)

-- Use PatientID to define route param on patient record

import Data.Patient as Patient exposing (PatientId)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --
-- Hold routes as union type


type Route
    = Home
    | Login
    | Logout
    | Patient PatientId



-- Use oneOf


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Patient (s "patient" </> Patient.patientIdParser)
        ]



{- INTERNAL -

   e.g.
   /
   /login
   /logout
   /patient/20dj3sdgiw

-}


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Patient patientId ->
                    [ "patient", Patient.patientIdToString patientId ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl



{- Main fromLocation logic -}


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
