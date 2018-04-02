module View exposing (view)

import Array
import Color
import Html exposing (div, Html, text, button)
import Collage
import Element
import Bitmaps
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, on)
import Model exposing (Model, playFieldMatrixSize, MatrixShape, State(..), Square(..), Coordinate)
import Messages exposing (Msg(..))

type alias Dimentions =
 {
    width: Float
    , height: Float
 }

fieldSize: Dimentions
fieldSize =
    {width = Bitmaps.bitmap16Size*toFloat playFieldMatrixSize.columns
    ,height = Bitmaps.bitmap16Size*toFloat playFieldMatrixSize.rows}


position1: Coordinate->Collage.Form->Collage.Form
position1 coords pic=
    let
        toScreen cellIndex modelExtent screenExtent =
            (toFloat cellIndex)/(toFloat modelExtent) * screenExtent - screenExtent/2
    in
    pic
        |> Collage.move (toScreen coords.x playFieldMatrixSize.columns fieldSize.width
        ,  toScreen coords.y playFieldMatrixSize.rows fieldSize.height )

renderField:Model.State-> Collage.Form
renderField state =
    let
        fieldColor = if state== Playing then Color.green else Color.red
    in
     Collage.rect fieldSize.width fieldSize.height
     |> Collage.filled fieldColor

renderCell: Square->Collage.Form
renderCell square=
    case square of
        Wall coordinate->Bitmaps.wall |> position1 coordinate
        Pillar coordinate->Bitmaps.pillar |> position1 coordinate
        Bomb coordinate _ ->Bitmaps.bomb |> position1 coordinate
        Monster coordinate direction _-> Bitmaps.monster |> position1 coordinate

renderPlayer: Square->Collage.Form
renderPlayer player=
    case player of
        Monster coord Model.Vertical 1 -> Bitmaps.playerUp |> position1 coord
        Monster coord Model.Vertical -1 -> Bitmaps.playerDown |> position1 coord
        Monster coord Model.Horizontal 1 -> Bitmaps.playerRight |> position1 coord
        Monster coord Model.Horizontal -1 -> Bitmaps.playerLeft |> position1 coord
        other->renderCell other

view : Model -> Html Msg
view model =
            div
                [
                ]
                [
                [renderPlayer model.player]
                  |> List.append (List.map renderCell model.cells)
                  |> List.append [renderField model.state]
                  |> Collage.collage (floor fieldSize.width) (floor fieldSize.height)
                  |> Element.toHtml
                ]
