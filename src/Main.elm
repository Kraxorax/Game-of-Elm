module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Browser.Navigation exposing (Key, pushUrl, load)
import Routing exposing (Route)
import Random exposing (generate)
import Url
import Html
import GameOfLife as GOL

type alias Model =
  { navKey : Key
  , route : Route
  , clock : Int
  , gol : GOL.Model
  }

type Msg
    = UrlUpdate Url.Url
    | RequestedUrl Browser.UrlRequest
    | Animate Float
    | RandomGen (List ( Int, Int ))
    | Gol GOL.Msg


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
    , clock = 0
    , gol = GOL.init []
    }
  , generate RandomGen (GOL.randomBrojevi GOL.defaultBoardSize)
  )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlUpdate location ->
      let
        route = Routing.routeLocation location
      in
        ( { model | route = route }, Cmd.none )

    RequestedUrl urlReq ->
      case urlReq of
        Browser.Internal url ->
          ( model, pushUrl model.navKey (Url.toString url) )

        Browser.External href ->
          ( model, load href )
    Animate diff ->
      ( { model
        | clock = model.clock
        , gol = GOL.tick diff model.gol
        }
      , Cmd.none
      )

    Gol golMsg ->
      let
        modCmd =
            GOL.update golMsg model.gol
      in
        ( { model
          | gol = Tuple.first modCmd
          }
        , Cmd.map (\gm -> Gol gm) (Tuple.second modCmd)
        )

    RandomGen zivi ->
      ( { model
        | gol = GOL.init zivi
        }
      , Cmd.none
      )

view : Model -> Browser.Document Msg
view model =
  Browser.Document
    "Game of Elm"
    [ Html.map (\m -> Gol m) (GOL.view model.gol)
    ]

