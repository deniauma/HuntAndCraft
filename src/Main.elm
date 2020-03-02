module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, h2)
import Html.Events exposing (onClick)
import Time
import Task

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model = Int
type alias Fighter = 
        { name : String
        , hp : Int
        , max_hp : Int
        , attack : Int 
        }
type alias GameState = 
        { player : Fighter
        , monster : Fighter
        , current_time : Time.Posix
        , prev_time : Time.Posix
        }

player = Fighter "Player" 100 100 10
monster = Fighter "Monster" 500 500 5

init : () -> (GameState, Cmd Msg)
init _ =
        ({ player = player
        , monster = monster
        , current_time = Time.millisToPosix 0
        , prev_time = Time.millisToPosix 0
        }, Cmd.none)


type Msg
  = StartBattle
  | PlayerTurn
  | MonsterTurn
  | Tick Time.Posix


update : Msg -> GameState -> (GameState, Cmd Msg)
update msg model =
        case msg of
                Tick tick ->
                        ({ model | current_time = tick, prev_time = model.current_time}, Cmd.none)

                StartBattle ->
                        let
                            model_player = model.player
                            new_player = { model_player | hp = 200 }
                        in
                            ({ model | player = new_player }, Cmd.none)

                PlayerTurn ->
                        let
                            model_player = model.player
                            model_monster = model.monster
                            new_monster = { model_monster | hp = model_monster.hp - model_player.attack }
                        in
                            ({ model | monster = new_monster }, Cmd.none)

                MonsterTurn ->
                        let
                            model_player = model.player
                            model_monster = model.monster
                            new_player = { model_player | hp = model_player.hp - model_monster.attack }
                        in
                            ({ model | player = new_player }, Cmd.none)


subscriptions : GameState -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


view : GameState -> Html Msg
view model =
  div []
    [ h2 [] [ text (String.fromInt model.player.hp) ]
    , h2 [] [ text (String.fromInt model.monster.hp) ]
    , button [ onClick StartBattle ] [ text "Start battle" ]
    ]
