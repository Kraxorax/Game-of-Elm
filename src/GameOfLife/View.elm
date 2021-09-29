module GameOfLife.View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Element exposing 
    (Element, spaceEvenly, fill, rgba, html, el, text, row, column, padding, layout, spacing, width)
import Element.Font as Font exposing (center)
import Element.Border as Border
import Element.Input exposing (button)
import Array exposing (toList)
import Matrix exposing (indexedMap, toArray)
import Neighbours exposing (MatrixTopology(..))
import GameOfLife.Model exposing (Model, Cell(..))
import GameOfLife.Msg exposing (Msg(..))
import Element.Background exposing (color)


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
        
        board = column [ padding 5 ] 
            [ html 
                ( div boardStyle 
                    ( indexedMap (matrixToBoard model.boardSize) model.board
                        |> Matrix.toArray
                        |> Array.toList
                    )
                )
            ]

        bigColumnStyle = 
            [ padding 20
            , spacing 10
            , width fill
            , Font.family 
                [ Font.typeface "Monotype"
                , Font.sansSerif
                ]
            , Border.rounded 5
            , Border.shadow { offset = (2, 2)
                            , size = 2
                            , blur = 4
                            , color = rgba 0 0 0 0.2
                            }
            ]

        generationAndTime = 
            ( String.fromInt model.genNumb ) ++ " / "
            ++ ( istocifra (String.fromInt model.counter) model.refreshTime) 

        stats = column bigColumnStyle
            [ text "Generation / time:"
            , text generationAndTime
            , text ("size: " ++ String.fromInt model.boardSize ++ " ^2")
            , text ("Topology: " ++ (topologyToString model.topology))
            ]

        startStopBttnLabel = if not model.running then "START" else "STOP"

        bigBttnStyle = 
            [ padding 4
            , Border.rounded 2
            , Border.shadow { offset = (1, 1)
                            , size = 1
                            , blur = 2
                            , color = rgba 0 0 0 0.1
                            }
            , width fill
            , center
            , color (rgba 255 255 255 255)
            , Element.mouseOver 
                [ color (rgba 0.9 0.9 0.9 0.2) ]
            ]

        mainControl = column [ width fill, spacing 5 ]
            [ button bigBttnStyle { onPress = Just ToggleRunning, label = text startStopBttnLabel }
            , button bigBttnStyle { onPress = Just KillAll, label = text "Kill all" } 
            , button bigBttnStyle { onPress = Just Reseed, label = text "Reseed" }
            ]

        secondaryControl = column [ width fill, spacing 5 ]
            [ button bigBttnStyle { onPress = Just Step, label = text "Step" }
            , button bigBttnStyle { onPress = Just (Zoom 1), label = text "Zoom out" } 
            , button bigBttnStyle { onPress = Just (Zoom -1), label = text "Zoom in" }
            , button bigBttnStyle { onPress = Just (Accelerate -150), label = text "Accelerate" }
            , button bigBttnStyle { onPress = Just (Accelerate 150), label = text "Decelerate" }
            ]

        topologies = column [ width fill, spacing 5 ]
            [ button bigBttnStyle { onPress = Just (ChangeTopology Torus), label = text "Torus" }
            , button bigBttnStyle { onPress = Just (ChangeTopology Plane), label = text "Plane" }
            , button bigBttnStyle { onPress = Just (ChangeTopology StripVertical), label = text "Vertical Strip" }
            , button bigBttnStyle { onPress = Just (ChangeTopology StripHorizontal), label = text "Horizontal Strip" }
            ]

        controlColumn = column bigColumnStyle
            [ row [ width fill ] [mainControl]
            , row [ width fill ] [secondaryControl]
            , row [ width fill ] [topologies]
            ]

    in
    layout [ padding 10]
        (row [ spaceEvenly, width fill ]
            [ stats
            , board
            , controlColumn
            ]
        )
