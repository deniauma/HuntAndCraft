module Explo exposing (..)

import Battle exposing (FightingStats)
import Random


type ExploState
    = NoExplo
    | ExploInProgress Zone Float
    | ExploComplete Zone

type Biome
    = Forest
    | Mountains

type alias Zone = 
    { name : String
    , biome : Biome
    , explo_rate : Float }

exploTick = 10

getZonefromBiome : Biome -> Zone
getZonefromBiome biome =
    case biome of
        Forest -> Zone "Forest" Forest 1
        Mountains -> Zone "Mountains" Mountains 0.5

getAvailableZones = [getZonefromBiome Forest, getZonefromBiome Mountains]

updateExplo : ExploState -> Float -> (ExploState, Float)
updateExplo explo acc =
    case explo of
        NoExplo -> (explo, 0)
        ExploInProgress zone progress -> if acc >= exploTick then
            let
                new_progress = min (progress + zone.explo_rate) 100
            in
                if new_progress == 100 then
                    (ExploComplete zone, 0)
                else updateExplo (ExploInProgress zone new_progress) (acc - exploTick)
            else (explo, acc)
        ExploComplete zone -> (explo, 0)



type MonsterType
    = Wolf
    | Boar
    | BrownBear

type alias Quest = 
    { monster_type : MonsterType
    , biome : Biome
    , remaining: Int
    , exp : Int
    , gold : Int }

monsterTypeToString m =
    case m of
        Wolf -> "Wolf"
        Boar -> "Boar"
        BrownBear -> "Brown bear"

createQuestFromZone : Zone -> Random.Generator Quest
createQuestFromZone zone =
    case zone.biome of
        Forest -> Random.map (\x -> Quest x zone.biome 1 10 50) (Random.weighted (50, Boar) [(30, Wolf), (20, BrownBear)])
        Mountains -> Random.map (\x -> Quest x zone.biome 1 10 50) (Random.weighted (50, Boar) [(30, Wolf), (20, BrownBear)])

createWolfQuest : Quest
createWolfQuest = Quest Wolf Forest 1 10 50

getMonsterStats : Quest -> FightingStats
getMonsterStats q = 
    case q.monster_type of
        Wolf -> FightingStats 200 200 20 0
        Boar -> FightingStats 100 100 5 0
        BrownBear -> FightingStats 500 500 50 0

incrementQuest : Quest -> MonsterType -> Quest
incrementQuest quest monster_type =
    if quest.monster_type == monster_type then { quest | remaining = quest.remaining + 1 }
    else quest

decrementQuest : Quest -> MonsterType -> Quest
decrementQuest quest monster_type =
    if quest.monster_type == monster_type then { quest | remaining = quest.remaining - 1 }
    else quest

addQuest : List Quest -> Quest -> List Quest
addQuest quests q = 
    if (List.any (\x -> x.monster_type == q.monster_type) quests) then
        List.map (\x -> incrementQuest x q.monster_type) quests
    else (quests ++ [q])

removeQuest : List Quest -> Quest -> List Quest
removeQuest quests q =
    let
        new_quests = 
            if (List.any (\x -> x.monster_type == q.monster_type) quests) then
                List.map (\x -> decrementQuest x q.monster_type) quests
            else quests
    in
        List.filter (\x -> x.remaining > 0) new_quests