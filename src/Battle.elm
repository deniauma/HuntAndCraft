module Battle exposing (..)


type alias FightingStats =
    { hp : Int
    , max_hp : Int
    , atk : Int
    , def : Int }

type BattleState
    = NoBattle
    | PlayerTurn FightingStats FightingStats
    | MonsterTurn FightingStats FightingStats
    | Win FightingStats FightingStats
    | Lose FightingStats FightingStats


battleTick = 500

updateFighterHP : FightingStats -> FightingStats -> FightingStats
updateFighterHP attacker defender =
    { defender | hp = max (defender.hp - attacker.atk) 0 }

updateBattle : BattleState -> Float -> (BattleState, Float)
updateBattle battle acc =
    case battle of
        NoBattle -> (battle, 0)
        PlayerTurn player monster -> if acc >= 500 then
            let new_monster = { monster | hp = max (monster.hp - player.atk) 0 }
            in 
                if new_monster.hp == 0 then
                    updateBattle (Win player new_monster) (acc - 500)    
                else updateBattle (MonsterTurn player new_monster) (acc - 500)
            else (battle, acc)
        MonsterTurn player monster -> if acc >= 500 then
            let new_player = updateFighterHP monster player
            in
                if new_player.hp == 0 then
                    updateBattle (Lose new_player monster) (acc - 500)
                else updateBattle (PlayerTurn new_player monster) (acc - 500)
            else (battle, acc)
        _ -> (battle, 0)

resetFighterHP : FightingStats -> FightingStats
resetFighterHP fighter =
    { fighter | hp = fighter.max_hp }