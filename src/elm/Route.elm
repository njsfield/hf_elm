module Route exposing (Route(..), fromLocation, href, modifyUrl)

-- Use PatientID from Profile to define route param on patient record

import Data.Profile as Profile exposing (PatientId)
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
    | Profile PatientId



-- Use oneOf for route matching


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Profile (s "profile" </> string)
        ]



{- INTERNAL -

   e.g.
   /
   /login
   /logout
   /profile/20dj3sdgiw

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

                Profile patientId ->
                    [ "profile", patientId ]
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
