module Explo exposing (..)


type ExploState
    = NoExplo
    | ExploInProgress Zone Float
    | ExploComplete Zone Quest


type alias Zone = 
    { name : String
    , explo_rate : Float
    , monster_types : Int }

exploTick = 10

zone1 = Zone "Forest" 1 5
zone2 = Zone "Mountains" 0.5 5

getAvailableZones = [zone1, zone2]

updateExplo : ExploState -> Float -> (ExploState, Float)
updateExplo explo acc =
    case explo of
        NoExplo -> (explo, 0)
        ExploInProgress zone progress -> if acc >= exploTick then
            let
                new_progress = min (progress + zone.explo_rate) 100
            in
                if new_progress == 100 then
                    (ExploComplete zone createWolfQuest, 0)
                else updateExplo (ExploInProgress zone new_progress) (acc - exploTick)
            else (explo, acc)
        ExploComplete zone q -> (explo, 0)



type MonsterType
    = Wolf
    | Boar

type MonsterDrop
    = Fur
    | Tail
    | Fang
    | Tooth
    | Bone

wolfDrops = [Fur, Tail, Fang]
boarDrops = [Fur, Tooth, Bone]

type alias Quest = 
    { monster_type : MonsterType
    , drops :  List MonsterDrop
    , remaining: Int
    , exp : Int
    , gold : Int }

monsterTypeToString m =
    case m of
        Wolf -> "Wolf"
        Boar -> "Boar"

createWolfQuest : Quest
createWolfQuest = Quest Wolf wolfDrops 1 10 50

incrementQuest : Quest -> MonsterType -> Quest
incrementQuest quest monster_type =
    if quest.monster_type == monster_type then { quest | remaining = quest.remaining + 1 }
    else quest

addQuest : List Quest -> Quest -> List Quest
addQuest quests q = 
    if (List.any (\x -> x.monster_type == q.monster_type) quests) then
        List.map (\x -> incrementQuest x q.monster_type) quests
    else (quests ++ [q])