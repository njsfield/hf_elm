module Page.Errored exposing (PageLoadError, pageLoadError, view)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.
-}

import Data.Session as Session exposing (Session)
import Html exposing (Html, div, h1, img, main_, p, text)
import Html.Attributes exposing (alt, class, id, tabindex)
import Views.Page as Page exposing (ActivePage)
import Styles.Page.Errored as Styles


-- MODEL --


type PageLoadError
    = PageLoadError Model


type alias Model =
    { activePage : ActivePage
    , errorMessage : String
    }


pageLoadError : ActivePage -> String -> PageLoadError
pageLoadError =
    PageLoadError Model



-- VIEW --


view : Session -> PageLoadError -> Html msg
view _ (PageLoadError model) =
    main_ [ id "content", class Styles.mainContainer, tabindex -1 ]
        [ h1 [] [ text "Error Loading Page" ]
        , div [ Styles.messageContainer ]
            [ p [] [ text model.errorMessage ] ]
        ]
