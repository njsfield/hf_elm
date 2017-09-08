module Data.Session exposing (Session, attempt)

import Data.AuthToken exposing (AuthToken)
import Util exposing ((=>))


{- Store user details
   E.g. { user: }
-}


type alias Session =
    { token : Maybe AuthToken }



{- Attempt a Cmd with session token

   e.g.
   "Access patient records"
   getPatientData
   {token : Just "sdf290x92..."}

-}


attempt : String -> (AuthToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
attempt attemptedAction toCmd session =
    -- Attempt to use session in module
    case Maybe.map .token session.token of
        -- When user logs out
        Nothing ->
            [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ] => Cmd.none

        Just token ->
            -- Use the token to make a
            -- Request for data request on initial
            [] => toCmd token
