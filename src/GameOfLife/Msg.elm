module GameOfLife.Msg exposing (Msg(..))

import Neighbours exposing (MatrixTopology(..))
import GameOfLife.Model exposing (Cell)

type Msg
    = Tick Float
    | ToggleRunning
    | Reseed
    | Recreate (List ( Int, Int ))
    | Zoom Int
    | KillAll
    | Klik ( Int, Int ) Cell
    | Step
    | Accelerate Float
    | ChangeTopology MatrixTopology
