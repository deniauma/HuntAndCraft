module Game exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, text, h2, h3, span, p, br, select, option)
import Html.Attributes exposing (id, class, style, disabled)
import Html.Events exposing (onClick)
import Random
import Explo exposing (..)
import Battle exposing (..)
import Equip exposing (Equip)
import Inventory exposing (Inventory)

type PlayerAction = Exploring
    | Battling
    | Idle

type alias Model =
    { accumulator : Float 
    , explo_accumulator: Float
    , exploration : ExploState
    , quests : List Quest
    , current_quest : Maybe Quest
    , battle : BattleState
    , player_stats : FightingStats
    , player_action : PlayerAction
    , inventory : Inventory }


init _ =
    ( { accumulator = 0 
    , explo_accumulator = 0
    , exploration = NoExplo
    , quests = []
    , current_quest = Nothing
    , battle = NoBattle
    , player_stats = FightingStats 300 300 10 0
    , player_action = Idle
    , inventory = Inventory.newForTest }, Cmd.none )


type Msg
    = Frame Float
    | StartExplo
    | CancelExplo
    | StartBattle
    | EndQuestWithRewards
    | EndQuestNoRewards
    | AddNewQuest Quest
    | SelectQuest Quest


subscriptions _ =
    onAnimationFrameDelta Frame


update msg model =
    case msg of
        Frame delta -> 
            case model.player_action of
                Exploring -> 
                    let
                        (new_explo, new_explo_acc) = updateExplo model.exploration (model.explo_accumulator + delta)
                    in
                        case new_explo of
                            ExploComplete z -> (model, Random.generate AddNewQuest (createQuestFromZone z))
                            _ -> ({ model | exploration = new_explo
                                , explo_accumulator = new_explo_acc }, Cmd.none)
                Battling -> 
                    let
                        (new_battle, new_battle_acc) = updateBattle model.battle (model.accumulator + delta)
                    in
                        ({ model | battle = new_battle
                        , accumulator = new_battle_acc }, Cmd.none)
                Idle -> (model, Cmd.none)

        StartBattle -> 
            let
                (new_quests, monster_stats) = case model.current_quest of
                    Just q -> (removeQuest model.quests q, getMonsterStats q) 
                    Nothing -> (model.quests, FightingStats 200 200 20 0)
            in
                ({ model | battle = PlayerTurn model.player_stats monster_stats
                , exploration = NoExplo
                , quests = new_quests
                , player_action = Battling }, Cmd.none)
        StartExplo -> ( { model | exploration = ExploInProgress (getZonefromBiome Forest) 0
            , explo_accumulator = 0
            , battle = NoBattle
            , player_action = Exploring }, Cmd.none )
        CancelExplo -> ( { model | exploration = NoExplo, explo_accumulator = 0, player_action = Idle }, Cmd.none )
        SelectQuest q -> ( { model | current_quest = Just q }, Cmd.none)
        EndQuestWithRewards -> ( { model | current_quest = Nothing, player_action = Idle, battle = NoBattle }, Cmd.none )
        EndQuestNoRewards -> ( { model | current_quest = Nothing, player_action = Idle, battle = NoBattle }, Cmd.none )
        AddNewQuest q -> ({ model | exploration = ExploInProgress (getZonefromBiome q.biome) 0
                                , explo_accumulator = 0
                                , quests = addQuest model.quests q }, Cmd.none)



view : Model -> Html Msg
view model =
    div [ id "game"]
        [ div [ id "quests"]  [ h2 [] [ text "Quests" ], viewQuests model.quests ]
        , div [ id "explo" ] [ h2 [] [ text "Exploration" ], viewExploration model.exploration model.player_action ]
        , div [ id "battle" ] [ h2 [] [ text "Battle" ]
            , viewBattle model model.player_action
            ]
        , div [ id "gear"] [ h2 [] [ text "Equipment" ], viewEquipment ]
        , div [ id "debug" ] [ viewDebug model ]
        ]

viewExploration : ExploState -> PlayerAction -> Html Msg
viewExploration explo player_action =
    case explo of
        ExploInProgress _ progress -> 
            div [] 
                [ p [] [ text "Progress ... "
                , span [] [ text (String.fromFloat progress ++ "%")]
                , div [ class "progress" ] [ div [ class "progress-current", style "width" (String.fromFloat progress ++ "%") ] [] ]
                , button [onClick CancelExplo] [ text "Cancel" ] ] 
                ]
        NoExplo -> div [] [ text "No exploration in progress.", br [] [], button [ onClick StartExplo, disabled (player_action /= Idle) ] [ text "Start exploring" ] ]
        ExploComplete zone -> div [] [ text ("Exploration of " ++ zone.name ++ " is complete.") ]


viewQuest : Quest -> Html Msg
viewQuest q = 
    div [ class "quest-item", onClick (SelectQuest q) ] 
        [ p [] [ text ("Monster: " ++ monsterTypeToString q.monster_type) ]
        , p [] [ text ("Experience: " ++ String.fromInt q.exp) ]
        , p [] [ text ("Gold: " ++ String.fromInt q.gold) ]
        , p [] [ text ("Remaining: " ++ String.fromInt q.remaining) ]
        , br [][]
        ]

viewQuests : List Quest -> Html Msg
viewQuests quests =
    div [] (List.map viewQuest quests)

viewBattle : Model -> PlayerAction -> Html Msg
viewBattle model player_action =
    case model.current_quest of
        Nothing -> div [][]
        Just quest -> div [] 
                        [ div [ class "battle-stats" ] [ 
                            viewPlayerStats model.player_stats model.battle
                            , viewMonsterStats model.battle quest
                            ]
                        , viewBattleButtons model.battle player_action
                        ]

viewMonsterStats : BattleState -> Quest -> Html Msg
viewMonsterStats state quest =
    case state of
        NoBattle -> viewMonsterLife (getMonsterStats quest) (monsterTypeToString quest.monster_type)
        PlayerTurn _ m -> viewMonsterLife m (monsterTypeToString quest.monster_type)
        MonsterTurn _ m -> viewMonsterLife m (monsterTypeToString quest.monster_type)
        Win _ m -> viewMonsterLife m (monsterTypeToString quest.monster_type)
        Lose _ m -> viewMonsterLife m (monsterTypeToString quest.monster_type)

viewMonsterLife : FightingStats -> String -> Html Msg
viewMonsterLife m name =
    div [ class "battle-fighter" ]
        [ p [] [ text name ]
        , span [] [ text (String.fromInt m.hp ++ "/" ++ String.fromInt m.max_hp) ]
        , div [ class "progress" ] [ div [ class "progress-current lifebar", style "width" (String.fromFloat (toFloat m.hp / toFloat m.max_hp * 100) ++ "%") ] [] ]
        ]

viewPlayerStats : FightingStats -> BattleState -> Html Msg
viewPlayerStats player battle_state =
    let
        current_hp = case battle_state of
            PlayerTurn p _ -> p.hp
            MonsterTurn p _ -> p.hp
            Win p _ -> p.hp
            Lose p _ -> p.hp
            _ -> player.hp
        hp_percent = String.fromFloat (toFloat current_hp / toFloat player.max_hp * 100) ++ "%"
    in  
        div [ class "battle-fighter" ]
            [ p [] [ text "Player", viewBattleOutcome battle_state ]
            , span [] [ text (String.fromInt current_hp ++ "/" ++ String.fromInt player.max_hp) ]
            , div [ class "progress" ] [ div [ class "progress-current lifebar", style "width" hp_percent ] [] ]
            ]

viewBattleOutcome : BattleState -> Html Msg
viewBattleOutcome battle_state =
    case battle_state of
        Win _ _ -> span [] [ text " - You win!" ]
        Lose _ _ -> span [] [ text " - You lose!" ]
        _ -> span [] []

viewBattleButtons : BattleState -> PlayerAction -> Html Msg
viewBattleButtons battle_state player_action =
    case battle_state of
        NoBattle -> button [ onClick StartBattle, disabled (player_action /= Idle) ] [ text "Fight!" ]
        Win _ _ -> button [ onClick EndQuestWithRewards ] [ text "End quest and get rewards" ]
        Lose _ _ -> button [ onClick EndQuestNoRewards ] [ text "End quest (no rewards)" ] 
        _ -> text ""

viewEquipment : Html Msg
viewEquipment =
    div [ class "equips" ] 
        [ select [] [ option [] [ text "Weapon" ] ]
        , select [] [ option [] [ text "Head" ] ]
        , select [] [ option [] [ text "Arms" ] ]
        , select [] [ option [] [ text "Chest" ] ]
        , select [] [ option [] [ text "Waist" ] ]
        , select [] [ option [] [ text "Legs" ] ]
        ]

viewEquipForType : Inventory -> Html Msg
viewEquipForType inventory =
    div [] []


viewDebug : Model -> Html Msg
viewDebug model =
    div [] [ 
        case model.battle of
            NoBattle -> div [] [ text "No battle" ] 
            PlayerTurn p m -> div [] [ text ("Player turn: current player HP = " ++ String.fromInt p.hp ++ ", current monster HP = " ++ String.fromInt m.hp) ]
            MonsterTurn p m -> div [] [ text ("Monster turn: current player HP = " ++ String.fromInt p.hp ++ ", current monster HP = " ++ String.fromInt m.hp) ]
            Win _ _ -> div [] [ text "Win" ] 
            Lose _ _ -> div [] [ text "Lose" ] 
    , text ("battle acc: " ++ String.fromFloat model.accumulator), text (", Explo acc: " ++ String.fromFloat model.explo_accumulator)
    , div [] (model.inventory 
        |> Inventory.getAllEquips 
        |> List.map viewEquip)
    ]

viewEquip : (Int, Equip) -> Html Msg
viewEquip (eq_id, equip) =
    span [ id (String.fromInt eq_id) ] [ text equip.name ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

