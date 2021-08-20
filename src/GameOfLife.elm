module GameOfLife exposing (Model, Msg, view
    , defaultBoardSize, init, randomBrojevi
    , tick, update)

import Array
import Matrix exposing (Matrix, concatHorizontal, concatVertical, generate, indexedMap, repeat)
import Neighbours exposing (MatrixTopology(..), neighbours)
import Random exposing (Generator, generate, int, list, pair)
import GameOfLife.View as GOLView exposing (view)
import GameOfLife.Model as GOLModel exposing (Model, Board, Cell(..))
import GameOfLife.Msg as GOLMsg exposing (Msg(..))

type alias Model = GOLModel.Model
type alias Msg = GOLMsg.Msg
view = GOLView.view

defaultBoardSize : Int
defaultBoardSize = 60


defaultRefreshTime : Float
defaultRefreshTime =
    30


flyer : List ( Int, Int )
flyer =
    [ ( 5, 5 )
    , ( 6, 5 )
    , ( 7, 5 )
    , ( 7, 6 )
    , ( 6, 7 )
    ]


init : List ( Int, Int ) -> Model
init livingCells =
    Model
        (bringToLife livingCells defaultBoardSize)
        defaultBoardSize
        0
        0
        0
        True
        defaultRefreshTime
        Torus


bringToLife : List ( Int, Int ) -> Int -> Matrix Cell
bringToLife livingCells boardSize =
    repeat boardSize boardSize Dead
        |> indexedMap
            (\x y _ ->
                if List.member ( x, y ) livingCells then
                    Alive

                else
                    Dead
            )


resize : Board -> Int -> Board
resize t d =
    if d > 0 then
        enlarge t d

    else if d < 0 then
        ensmallen t (abs d)

    else
        t


enlarge : Board -> Int -> Board
enlarge t d =
    let
        hsides =
            repeat d (Matrix.height t) Dead

        nt =
            Result.withDefault t
                (concatHorizontal hsides t)

        nt1 =
            Result.withDefault t
                (concatHorizontal nt hsides)

        vsides =
            repeat (Matrix.width nt1) d Dead

        nt2 =
            Result.withDefault t
                (concatVertical vsides nt1)

        nt3 =
            Result.withDefault nt2
                (concatVertical nt2 vsides)
    in
    nt3


ensmallen : Board -> Int -> Board
ensmallen t delta =
    let
        d =
            abs delta

        ts =
            Matrix.width t

        r =
            ts - 2 * d

        sm =
            Matrix.generate r r (\x y -> Matrix.get (x + d) (y + d) t |> Result.withDefault Dead)
    in
    sm


trimList : Int -> List a -> List a
trimList d l =
    List.drop d l |> List.take (List.length l - 2 * d)


numberOfLiving : Int -> Int -> Model -> Int
numberOfLiving x y m =
    neighbours m.topology x y m.board |> Array.toList |> List.filter isAlive |> List.length


modulo : Int -> Int -> Int
modulo a m =
    if a < 0 then
        m + a

    else if a >= m then
        m - a

    else
        a


newState : Model -> Board
newState m =
    m.board
        |> indexedMap
            (\x y c ->
                let
                    neighbours =
                        numberOfLiving x y m
                in
                survival neighbours c
            )


survival : Int -> Cell -> Cell
survival neighbours cell =
    if neighbours < 2 then
        Dead

    else if neighbours == 2 then
        cell

    else if neighbours == 3 then
        Alive

    else
        Dead


isAlive : Cell -> Bool
isAlive c =
    case c of
        Alive ->
            True

        Dead ->
            False


toggleCell : Cell -> Cell
toggleCell c =
    case c of
        Alive ->
            Dead

        Dead ->
            Alive


tick : Float -> Model -> Model
tick dt model =
    let
        t =
            toFloat model.counter + dt

        t1 =
            if t > model.refreshTime then
                0

            else
                t

        m =
            if t1 == 0 then
                newState model

            else
                model.board

        gn =
            if t1 == 0 then
                model.genNumb + 1

            else
                model.genNumb
    in
    if not model.running then
        model

    else
        { model
            | counter = round t1
            , clock = model.clock + round dt
            , genNumb = gn
            , board = m
        }


step : Model -> Model
step m =
    { m
        | board = newState m
        , genNumb = m.genNumb + 1
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Tick t ->
            ( tick t model, Cmd.none )

        Reseed ->
            ( model, Random.generate Recreate (randomBrojevi model.boardSize) )

        Recreate zivi ->
            ( { model
                | board = bringToLife zivi model.boardSize
                , genNumb = 0
                , counter = 0
              }
            , Cmd.none
            )

        Zoom d ->
            let
                newBoardSize =
                    model.boardSize + d * 2
            in
            ( { model
                | board = resize model.board d
                , boardSize = newBoardSize
              }
            , Cmd.none
            )

        Klik ( x, y ) _ ->
            ( { model
                | board =
                    indexedMap
                        (\cx cy c ->
                            if (cx == x) && (cy == y) then
                                toggleCell c

                            else
                                c
                        )
                        model.board
              }
            , Cmd.none
            )

        KillAll ->
            ( { model
                | board = repeat model.boardSize model.boardSize Dead
              }
            , Cmd.none
            )

        Step ->
            ( step model
            , Cmd.none
            )

        Accelerate d ->
            ( { model
                | refreshTime = model.refreshTime + d
              }
            , Cmd.none
            )

        ChangeTopology t ->
            ( { model
                | topology = t
              }
            , Cmd.none
            )


randomBrojevi : Int -> Generator (List ( Int, Int ))
randomBrojevi boardSize =
    list ((toFloat boardSize ^ 2 / 4) |> floor) <|
        pair (int 0 boardSize) (int 0 boardSize)