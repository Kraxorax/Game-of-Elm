module GameOfLife.Model exposing (Model, Board, Cell(..))

import Matrix exposing (Matrix)
import Neighbours exposing (MatrixTopology(..))

type alias Model =
    { board : Board
    , boardSize : Int
    , clock : Int
    , counter : Int
    , genNumb : Int
    , running : Bool
    , refreshTime : Float
    , topology : MatrixTopology
    }


type Cell
    = Alive
    | Dead


type alias Board =
    Matrix Cell