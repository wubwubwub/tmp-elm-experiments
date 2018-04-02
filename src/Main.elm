module Flatris exposing (main)

import Model exposing (..)
import Update
import View
import Keyboard exposing (KeyCode)
import Window exposing (Size)
import Html
import Messages exposing (Msg(..))
import AnimationFrame
import Json.Encode exposing (Value)
import Task


main : Program Value Model Msg
main =
    Html.programWithFlags
        { init = \value -> ( Model.initial, Task.succeed Start |> Task.perform identity)
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.state == Model.Playing then
            AnimationFrame.diffs Tick
          else
            Sub.none
        , Keyboard.ups (\k->PlayerStopped )
        , Keyboard.downs (key model)
        ]


key : Model -> KeyCode -> Msg
key model keycode =
    case keycode of
       37 ->
           PlayerStartedMoving Horizontal -1

       39 ->
           PlayerStartedMoving Horizontal 1

       40 ->
           PlayerStartedMoving Vertical -1

       38 ->
           PlayerStartedMoving Vertical 1
       32 ->
           PlantBomb
       _ ->
            Noop
