module Page.Profile exposing (Model, Msg, init, update, view)

{-| Displaying a patients Profile
-}

import Data.Session as Session exposing (Session)
import Data.Profile as Profile exposing (Profile, PatientId)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Profile exposing (ProfileQuery)
import Task exposing (Task)
import Util exposing ((=>), onClickStopPropagation)


-- @TODO: Add views

import Views.Page as Page
import Styles.Page.Profile as Styles


{- MODEL
   For the home page, we'll need to store
   The list of patients at any time.

   The patientQuery model will keep track of
   queries that the user has made
-}


type alias Model =
    { errors : List String
    , profile : Profile
    }



{-
   Init
   To prepare out profile model (pre-render),
-}


init : Session -> ProfileId -> Task PageLoadError Model
init session profileId =
    let
        -- Convert main profile get function to task
        loadProfile =
            Request.Patients.get session profileId
                |> Http.toTask

        -- Prepare load error
        handleLoadError _ =
            "Profile is currently unavailable."
                |> pageLoadError (Page.Profile profileId)
    in
        Task.map (Model []) loadProfile
            |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "home-page" ]
        [ p [] [ text "Profile for" ++ model.profile.first_name ]
        ]



-- UPDATE --
-- @TODO: Add update logic


type Msg
    = NoOp


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            model => Cmd.none
