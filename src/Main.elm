module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Browser.Navigation exposing (Key)
import Routing exposing (Route)
import Random exposing (generate)
import Url
import Html
-- import GejmOfLajf as GejmOfLajf

type alias Model =
  { navKey : Key
  , route : Route
  }

type Msg
    = UrlUpdate Url.Url
    | RequestedUrl Browser.UrlRequest
    | Animate Float
    | RandomGen (List ( Int, Int ))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onAnimationFrameDelta Animate ]

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlUpdate -- \url -> UrlUpdate url
    , onUrlRequest = RequestedUrl --\urlReq -> RequestedUrl urlReq
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ location key =
  let
    route = Routing.routeLocation location
  in
  ( { navKey = key
    , route = route
    }
  , Cmd.none -- generate RandomGen (GejmOfLajf.randomBrojevi GejmOfLajf.defaultBoardSize)
  )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )

view : Model -> Browser.Document Msg
view model =
  Browser.Document
    "Game of Elm"
    [ Html.div []
      [ Html.text "wat"
      ]
    ]

