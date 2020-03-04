module Game exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, text, h2, h3, span, p)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Explo exposing (..)
import Battle exposing (..)


type alias Model =
    { accumulator : Float 
    , explo_accumulator: Float
    , exploration : ExploState
    , quests : List Quest
    , battle : BattleState
    , player_stats : FightingStats }


init _ =
    ( { accumulator = 0 
    , explo_accumulator = 0
    , exploration = NoExplo
    , quests = []
    , battle = NoBattle
    , player_stats = FightingStats 300 300 10 0 }, Cmd.none )


type Msg
    = Frame Float
    | StartExplo
    | ExploProgress
    | CancelExplo
    | StartBattle


subscriptions _ =
    onAnimationFrameDelta Frame


exploTick = 100
battleTick = 500

monster_stats = FightingStats 200 200 20 0

update msg model =
    case msg of
        Frame delta -> 
            let
                (new_explo, new_explo_acc) = updateExplo model.exploration (model.explo_accumulator + delta)
                (new_battle, new_battle_acc) = updateBattle model.battle (model.accumulator + delta)
            in
                case new_explo of
                    ExploComplete z q -> ({ model | exploration = ExploInProgress z 0, explo_accumulator = new_explo_acc, quests = addQuest model.quests q, battle = new_battle, accumulator = new_battle_acc }, Cmd.none)
                    _ -> ({ model | exploration = new_explo, explo_accumulator = new_explo_acc, battle = new_battle, accumulator = new_battle_acc }, Cmd.none)
        StartBattle -> ({ model | battle = PlayerTurn model.player_stats monster_stats, exploration = NoExplo }, Cmd.none)
        StartExplo -> ( { model | exploration = ExploInProgress zone1 0, explo_accumulator = 0, battle = NoBattle }, Cmd.none )
        ExploProgress -> ( model, Cmd.none )
        CancelExplo -> ( { model | exploration = NoExplo, explo_accumulator = 0 }, Cmd.none )



view model =
    div [ id "game"]
        [ div [ id "quests"]  [ h2 [] [ text "Quests" ], viewQuests model.quests ]
        , div [ id "explo" ] [ h2 [] [ text "Exploration" ], viewExploration model.exploration ]
        , div [ id "battle" ] [ h2 [] [ text "Fight monster" ]
            , viewPlayerStats model.player_stats model.battle
            , viewMonsterStats model.battle, button [ onClick StartBattle ] [ text "Fight!" ] ]
        , div [ id "debug" ] [ viewDebug model ]
        ]

viewExploration : ExploState -> Html Msg
viewExploration explo =
    case explo of
        ExploInProgress _ progress -> 
            div [] [ p [] [ text "Progress ... ", 
                span [] [ text (String.fromFloat progress ++ "%")], 
                button [onClick CancelExplo] [ text "Cancel" ] ] ]
        NoExplo -> div [] [ text "No exploration in progress.", button [ onClick StartExplo ] [ text "Start exploring" ] ]
        ExploComplete zone q -> div [] [ text ("Exploration of " ++ zone.name ++ " is complete.") ]


viewQuest : Quest -> Html Msg
viewQuest q = 
    div [] 
        [ p [] [ text ("Monster: " ++ monsterTypeToString q.monster_type) ]
        , p [] [ text ("Experience: " ++ String.fromInt q.exp) ]
        , p [] [ text ("Gold: " ++ String.fromInt q.gold) ]
        , p [] [ text ("Remaining: " ++ String.fromInt q.remaining) ]
        ]

viewQuests : List Quest -> Html Msg
viewQuests quests =
    div [] (List.map viewQuest quests)

viewMonsterStats : BattleState -> Html Msg
viewMonsterStats state =
    let 
        output_text = case state of
            NoBattle -> ""
            PlayerTurn _ m -> String.fromInt m.hp ++ "/" ++ String.fromInt m.max_hp
            MonsterTurn _ m -> String.fromInt m.hp ++ "/" ++ String.fromInt m.max_hp
            Win _ m -> String.fromInt m.hp ++ "/" ++ String.fromInt m.max_hp
            Lose _ m -> String.fromInt m.hp ++ "/" ++ String.fromInt m.max_hp
    in 
        div []
            [ p [] [ text "Monster" ], span [] [ text output_text ]
            ]

viewPlayerStats : FightingStats -> BattleState -> Html Msg
viewPlayerStats player battle_state =
    let
        current_hp = case battle_state of
            PlayerTurn p m -> p.hp
            MonsterTurn p m -> p.hp
            _ -> player.hp
    in  
        div []
            [ p [] [ text "Player", viewBattleOutcome battle_state ], span [] [ text (String.fromInt current_hp ++ "/" ++ String.fromInt player.max_hp) ]
            ]

viewBattleOutcome : BattleState -> Html Msg
viewBattleOutcome battle_state =
    case battle_state of
        Win _ _ -> span [] [ text " - You win!" ]
        Lose _ _ -> span [] [ text " - You lose!" ]
        _ -> span [] []

viewDebug : Model -> Html Msg
viewDebug model =
    div [] [ 
        case model.battle of
            NoBattle -> div [] [ text "No battle" ] 
            PlayerTurn p m -> div [] [ text ("Player turn: current player HP = " ++ String.fromInt p.hp ++ ", current monster HP = " ++ String.fromInt m.hp) ]
            MonsterTurn p m -> div [] [ text ("Monster turn: current player HP = " ++ String.fromInt p.hp ++ ", current monster HP = " ++ String.fromInt m.hp) ]
            Win _ _ -> div [] [ text "Win" ] 
            Lose _ _ -> div [] [ text "Lose" ] 
    , text ("battle acc: " ++ String.fromFloat model.accumulator), text (", Explo acc: " ++ String.fromFloat model.explo_accumulator) ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

