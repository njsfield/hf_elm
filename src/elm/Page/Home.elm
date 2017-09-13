module Page.Home exposing (Model, Msg, init, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Session as Session exposing (Session)
import Data.Profile as Profile exposing (Profile, PatientId)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Profiles exposing (ProfilesQuery)
import Task exposing (Task)
import Util exposing ((=>), onClickStopPropagation)
import Views.Page as Page
import Styles.Page.Home as Styles


{- MODEL
   For the home page, we'll need to store
   The list of patients at any time.

   The patientQuery model will keep track of
   queries that the user has made
-}


type alias Model =
    { patient_query : PatientsQuery
    , patients : List Patient
    }



{-
   Init
   To prepare out home model (pre-render),
   We need the querying model, and resulting Dict
   of patients
-}


init : Session -> Task PageLoadError Model
init session =
    let
        loadProfilesQuery =
            Request.Profiles.initialProfilesQuery
                |> Task.succeed

        -- Main patient load call
        loadPatients =
            Request.Profiles.initialProfilesQuery
                |> Request.Profiles.get session
                |> Http.toTask

        -- Prepare load error
        handleLoadError _ =
            pageLoadError Page.Home "Homepage is currently unavailable."
    in
        Task.map2 Model loadProfilesQuery loadPatients
            |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "home-page" ]
        [ viewBanner
        , div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-9" ] (viewFeed model.feed)
                , div [ class "col-md-3" ]
                    [ div [ class "sidebar" ]
                        [ p [] [ text "Popular Tags" ]
                        , viewTags model.tags
                        ]
                    ]
                ]
            ]
        ]


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]


viewFeed : Feed.Model -> List (Html Msg)
viewFeed feed =
    div [ class "feed-toggle" ]
        [ Feed.viewFeedSources feed |> Html.map FeedMsg ]
        :: (Feed.viewArticles feed |> List.map (Html.map FeedMsg))


viewTags : List Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag-pill tag-default"
        , href "javascript:void(0)"
        , onClick (SelectTag tagName)
        ]
        [ text (Article.tagToString tagName) ]



-- UPDATE --


type Msg
    = QueryMsg Query.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        QueryMsg subMsg ->
            let
                ( newQuery, subCmd ) =
                    Query.update session subMsg model.profiles_query
            in
                { model | profiles_query = newProfileQuery } => Cmd.map QueryMsg subCmd
