module Messages exposing (Msg(..))

import Time exposing (Time)
import Window exposing (Size)
import Model exposing (..)


type Msg
    = Start
    | Tick Time
    | PlantBomb
    | RandomizedField (List Coordinate)
    | RandomizedMonster Square Square
    | PlayerStartedMoving Direction Int
    | PlayerStopped
    | Noop
