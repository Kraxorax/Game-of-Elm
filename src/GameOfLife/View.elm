module GameOfLife.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Array exposing (toList)
import Matrix exposing (indexedMap, toArray)
import Neighbours exposing (MatrixTopology(..))
import GameOfLife.Model exposing (Model, Cell(..))
import GameOfLife.Msg exposing (Msg(..))


boardPxWH : Float
boardPxWH =
    700


celToEle : Cell -> ( Int, Int ) -> List (Html.Attribute Msg) -> Html Msg
celToEle c ( x, y ) pos =
    let
        attrs =
            List.append
                [ class (celToString c ++ " Cell")
                , onClick (Klik ( x, y ) c)
                ]
                pos
    in
    div attrs
        []


celToString : Cell -> String
celToString cel =
    case cel of
        Dead ->
            "Dead"

        Alive ->
            "Alive"



cellSize : Int -> Float
cellSize boardSize =
    boardPxWH / toFloat boardSize


istocifra : String -> Float -> String
istocifra a rt =
    let
        pr =
            logBase 10 (rt + 1) |> ceiling
    in
    String.padLeft pr '0' a


topologyToString : MatrixTopology -> String
topologyToString mt =
    case mt of
        Plane ->
            "Plane"

        Torus ->
            "Torus"

        StripHorizontal ->
            "Horizontal Strip"

        StripVertical ->
            "Vertical Strip"



matrixToBoard : Int -> Int -> Int -> Cell -> Html Msg
matrixToBoard boardSize x y c =
    let
        x_ =
            toFloat x * cellSize boardSize

        y_ =
            toFloat y * cellSize boardSize

        bgColor =
            case c of
                Alive ->
                    "lawngreen"

                Dead ->
                    "rgb(160, 160, 160)"

        cellStyle =
            [ style "border" "1px solid lightgray"
            , style "position" "absolute"
            , style "top" (String.fromFloat y_ ++ "px")
            , style "left" (String.fromFloat x_ ++ "px")
            , style "width" (String.fromFloat (cellSize boardSize) ++ "px")
            , style "height" (String.fromFloat (cellSize boardSize) ++ "px")
            , style "background-color" bgColor
            ]
    in
    celToEle c ( x, y ) cellStyle


view : Model -> Html Msg
view model =
    let
        boardStyle =
            [ style "position" "relative"
            , style "width" "700px"
            , style "height" "700px"
            ]
    in
    div []
        [ div boardStyle
            (indexedMap
                (matrixToBoard model.boardSize)
                model.board
                |> Matrix.toArray
                |> Array.toList
            )
        , table []
            [ tr []
                [ td []
                    [ text
                        ("gen: "
                            ++ String.fromInt model.genNumb
                            ++ ":"
                            ++ istocifra (String.fromInt model.counter) model.refreshTime
                            ++ "/"
                            ++ String.fromFloat model.refreshTime
                        )
                    ]
                , td [] []
                , td []
                    [ text ("size: " ++ String.fromInt model.boardSize ++ " ^2")
                    ]
                , td [] [ text (topologyToString model.topology) ]
                ]
            , tr []
                [ td []
                    [ button [ onClick ToggleRunning ]
                        [ text
                            (if not model.running then
                                "START"

                             else
                                "STOP"
                            )
                        ]
                    ]
                , td []
                    [ button [ onClick Step ] [ text "Step" ]
                    ]
                , td []
                    [ button [ onClick (Zoom 1) ] [ text "Zoom Out" ]
                    ]
                , td []
                    [ button [ onClick (ChangeTopology Torus) ] [ text "Torus" ]
                    ]
                , td []
                    [ button [ onClick (ChangeTopology Plane) ] [ text "Plane" ]
                    ]
                ]
            , tr []
                [ td []
                    [ button [ onClick KillAll ] [ text "KILL ALL" ]
                    ]
                , td []
                    [ button [ onClick (Accelerate -150) ] [ text "Spd +" ]
                    , button [ onClick (Accelerate 150) ] [ text "Spd -" ]
                    ]
                , td []
                    [ button [ onClick (Zoom -1) ] [ text "Zoom In" ]
                    ]
                , td []
                    [ button [ onClick (ChangeTopology StripVertical) ] [ text "Vertical Strip" ]
                    ]
                , td []
                    [ button [ onClick (ChangeTopology StripHorizontal) ] [ text "Horizontal Strip" ]
                    ]
                ]
            , tr []
                [ td []
                    [ button [ onClick Reseed ] [ text "RESEED" ]
                    ]
                ]
            ]
        ]
