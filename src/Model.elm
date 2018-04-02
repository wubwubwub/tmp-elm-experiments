module Model exposing (Model, playFieldMatrixSize,MatrixShape, Coordinate, Direction(..), Square(..), State(..), initial)

import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Color exposing (Color)
import Time exposing (Time)

type State
    =  Playing
    | Stopped

type alias Coordinate =
    { x : Int
    , y: Int}

type alias MatrixShape =
    {
        columns: Int
        ,rows : Int
    }

playFieldMatrixSize: MatrixShape
playFieldMatrixSize =
    {columns= 20, rows=10}

type Direction
    = Vertical
    | Horizontal

type Square
    = Wall Coordinate
    | Pillar Coordinate
    | Bomb Coordinate Int
    | Monster Coordinate Direction Int

type alias Model =
    { cells: List Square
    , player: Square
    , playerMoving: Bool
    , seed : Random.Seed
    , state : State
    }


initial : Model
initial =
            { cells = []
            , player = Monster {x=0, y=0} Vertical 1
            , playerMoving=False
            , seed = Random.initialSeed 0
            , state = Stopped
            }
