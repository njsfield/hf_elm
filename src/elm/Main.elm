module Main exposing (..)

import Data.Session as Session exposing (Session)
import Data.Profile exposing (PatientId)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Ports
import Route exposing (Route)
import Task
import Storage
import Util exposing ((=>))
import Views.Page as Page exposing (ActivePage)


{- Page
   Union type to store corresponding Page with its
   model (if it has one)
-}


type alias IsLoading =
    Bool


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home Home.Model
    | Login Login.Model
    | Profile PatientId Profile.Model



{--Allow union type to wrap page in its state (whether loaded
or not)
--}


type PageState
    = Loaded Page
    | TransitioningFrom Page



{- GLOBAL MODEL
   Should always hold Session data (if applicable)
   And page state.

   E.g.
-}


type alias Model =
    { session : Session
    , pageState : PageState
    }



{- Init
   Ignore initial location, fire
   Cmd to Load token from LocalStorage...
-}


init : Location -> ( Model, Cmd Msg )
init _ =
    { session = { token = Nothing }
    , pageState = Loaded Blank
    }
        => loadTokenFromStorage



{--MAIN VIEW --

  When a new page is requested, call the view
  function with a model that has the current page
  wrapped in a TransitioningFrom type.

  The viewPage page function will then fade/out
  the page etc.

  At any one time, our model will only hold one Page
  and its associated model.
-}


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page



{- viewPage
   Every time a specific page is requested, use the
   users session to first inject appropriate profile into
   layout wrapper.
-}


viewPage : Session -> IsLoading -> Page -> Html Msg
viewPage session isLoading page =
    let
        layout =
            Page.layout isLoading session.token
    in
        case page of
            NotFound ->
                NotFound.view session
                    |> layout Page.Other

            Blank ->
                -- @TODO: Replace with spinner
                Html.text ""
                    |> layout Page.Other

            Errored subModel ->
                Errored.view session subModel
                    |> layout Page.Other

            Home subModel ->
                Home.view session subModel
                    |> layout Page.Home
                    |> Html.map HomeMsg

            Login subModel ->
                Login.view session subModel
                    |> layout Page.Other
                    |> Html.map LoginMsg

            Profile patientId subModel ->
                Profile.view session subModel
                    |> layout (Page.Profile patientId)
                    |> Html.map ProfileMsg



-- getPage: Parse the Page (no matter what its state)


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page



{--GLOBAL UPDATE --
   1) Hold union types for each pages messages
   2) Set session if user successfully logs in

   Update delegates to updatePage function
   for clarity...
-}


type Msg
    = SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError Home.Model)
    | ProfileLoaded Patient (Result PageLoadError Profile.Model)
    | HomeMsg Home.Msg
    | SetSession (Maybe AuthToken)
    | LoginMsg Login.Msg
    | ProfileMsg Profile.Msg
    | LoadToken (Maybe Authtoken)
    | SaveToken Authtoken
    | NoOp



{- Set Route
   This function is called when a new route is to be transitioned to;
   1) Prepare transition function. Its arguments are;
      toMsg = main Msg (e.g. PatientLoaded "sdf")
      task = transition task (e.g. Patient.init)
   2) Parse Route from Maybe ...
      If it's Nothing, set PageState to NotFound
      else set new PageState in model via transition function
      (for Logout & Login routes, (M,CM) tuples are returned without
      the transition call
   3) For logout route, reset token to Nothing, update session
      storage (Clear it), then redirect to Login route

-}


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Task.attempt toMsg task
    in
        case maybeRoute of
            Nothing ->
                { model | pageState = Loaded NotFound } => Cmd.none

            Just (Route.Home) ->
                -- Check token in model at this point
                -- If its Nothing then redirect to Login
                if model.session.token == Nothing then
                    model => (Route.modifyUrl Route.Login)
                else
                    transition HomeLoaded (Home.init model.session)

            Just (Route.Login) ->
                { model | pageState = Loaded (Login Login.initialModel) } => Cmd.none

            Just (Route.Logout) ->
                let
                    session =
                        model.session
                in
                    { model | session = { session | token = Nothing } }
                        => Cmd.batch
                            [ Storage.storeSession Nothing
                            , Route.modifyUrl Route.Login
                            ]

            -- Call init function for Profile
            Just (Route.Profile patientId) ->
                transition (ProfileLoaded patientId) (Profile.init model.session patientId)



{- pageErrored
   Combine unique error messages into the corresponding
   pages error field in its model.

   The view will render the Error page passing in the Pages
   model (and extracting its errors)
-}


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
        { model | pageState = Loaded (Errored error) } => Cmd.none



{- update
   1) Find the matching model for the page
   2) Use it as the primary model for updatePage ...

   The untouched msg & model are passed still, to
   preserve the global model itself (on new page transitions,
   the current model/page-model can be used for CherryPicking state to
   use in the next page-model
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model



{- updatePage
   1) Begin by parsing the session from the model
   2) Prepare toPage function with arguments;
      toModel   - Page Type
      toMsg     - (Page) Message Type
      subUpdate - (Page) Update function
      subMsg    - actual sub message for page
      subModel    - actual sub model for page

      to then return (M,CM)

   3) Allow msgs for setting new sessions
-}


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            -- Session (Maybe AuthToken)
            -- e.g { token : Just "hsdaox023" }
            model.session

        -- Prep function
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored : String -> ( Model, Cmd msg )
        errored =
            pageErrored model
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( HomeLoaded (Ok subModel), _ ) ->
                { model | pageState = Loaded (Home subModel) } => Cmd.none

            ( HomeLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( ProfileLoaded patientId (Ok subModel), _ ) ->
                { model | pageState = Loaded (Profile patientId subModel) } => Cmd.none

            ( ProfileLoaded patientId (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            -- (For Logging out mainly)
            ( SetSession maybeToken, _ ) ->
                let
                    session =
                        model.session

                    cmd =
                        -- In the event we have a token, but we're explicity
                        -- calling update here to set Nothing as the
                        -- token value...
                        -- Set cmd to redirect to Login
                        if session.token /= Nothing && maybeToken == Nothing then
                            Route.modifyUrl Route.Login
                        else
                            Cmd.none
                in
                    { model | session = { token = maybeToken } }
                        => cmd

            ( LoginMsg subMsg, Login subModel ) ->
                let
                    -- extract the model & cmd from Login page
                    -- E.g. Login.update (Login.InputUsername "jo") {username = "j" ...}
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Login.update subMsg subModel

                    -- msgFromPage == ExternalMsg (after subUpdate completes)
                    ( newModel, newCmd ) =
                        case msgFromPage of
                            Login.NoOp ->
                                ( model, cmd )

                            -- Success result after POST request to server
                            Login.SetSession token ->
                                let
                                    session =
                                        model.session
                                in
                                    { model | session = { token = Just token } }
                                        => saveTokenToStorage token
                in
                    { newModel | pageState = Loaded (Login pageModel) }
                        -- If msgFromPage is SetSession (with token)
                        -- We should also fire a message to set LocalStorage
                        -- because LoginPage doesn't care about saving to storage
                        =>
                            Cmd.map LoginMsg newCmd

            ( HomeMsg subMsg, Home subModel ) ->
                toPage Home HomeMsg (Home.update session) subMsg subModel

            ( ProfileMsg subMsg, Profile patientId subModel ) ->
                toPage (Profile patientId) ProfileMsg (Profile.update session) subMsg subModel

            -- (After init & token loaded)
            -- Attempt to navigate to home via
            -- setRoute function
            ( LoadToken maybeToken, _ ) ->
                setRoute (Just (Route.Home))
                    { model | session = { token = maybeToken } }

            ( _, NotFound ) ->
                -- Disregard incoming messages when we're on the
                -- NotFound page.
                model => Cmd.none

            ( _, _ ) ->
                -- Disregard incoming messages that arrived for the wrong page
                model => Cmd.none



{-
   loadTokenFromStorage
   Called on first page load
-}


loadTokenFromStorage : Cmd Msg
loadTokenFromStorage =
    Storage.get "token"
        |> attempt
            (\res ->
                case res of
                    Ok token ->
                        LoadToken (Just token)

                    Err _ ->
                        LoadToken Nothing
            )



{- saveTokenToStorage
   Use NoOp for throwaway message
-}


saveTokenToStorage : AuthToken -> Cmd LoginMsg
saveTokenToStorage token =
    Storage.set "token" token
        |> attempt (always NoOp)



-- MAIN --


main : Program Value Model Msg
main =
    -- Set initial route on load
    -- Navigation.program expects first argument as
    -- route function : (Location -> msg)
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
