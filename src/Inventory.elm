module Inventory exposing (..)

import Dict exposing (Dict)
import Equip exposing (Equip, EquipType)
import Battle exposing (FightingStats)


type alias Inventory =
    { next_id: Int
    , equips: Dict Int Equip
    }

new : Inventory
new = Inventory 0 Dict.empty

newForTest : Inventory
newForTest =
    Inventory 3 (Dict.fromList 
        [ (0, Equip.createHelmet  "Helmet (test)" (FightingStats 10 10 0 5))
        , (1, Equip.createHelmet  "Helmet2 (test)" (FightingStats 10 10 0 5))
        , (2, Equip.createHelmet  "Helmet3 (test)" (FightingStats 10 10 0 5))
        ] )

addEquip : Equip -> Inventory -> Inventory
addEquip equip inventory =
    Inventory (inventory.next_id + 1) (Dict.insert inventory.next_id equip inventory.equips)

getEquip : Int -> Inventory -> Maybe Equip
getEquip id inventory =
    Dict.get id inventory.equips

removeEquip : Int -> Inventory -> Inventory
removeEquip id inventory =
    { inventory | equips =  Dict.remove id inventory.equips }

getAllEquips : Inventory -> List (Int, Equip)
getAllEquips inventory =
    inventory.equips
        |> Dict.toList

getEquipsFromType : EquipType -> Inventory -> List (Int, Equip)
getEquipsFromType equip_type inventory =
    inventory.equips
        |> Dict.filter (\k -> \v -> v.kind == equip_type)
        |> Dict.toList
    