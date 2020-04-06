module Equip exposing (..)

import Battle exposing (FightingStats)

type EquipType
    = Weapon
    | Head
    | Arms
    | Chest
    | Waist
    | Leg

type alias Equip =
    { kind: EquipType
    , name: String
    , stats: FightingStats
    , equipped: Bool
    }

createHelmet : String -> FightingStats -> Equip
createHelmet name stats =
    Equip Head name stats False