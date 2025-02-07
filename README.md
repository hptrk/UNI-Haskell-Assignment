# Haskell Army Battle Simulation

This project is a **university assignment** for Haskell, implementing an army battle simulation with different unit types, spells, and combat mechanics.

## 📖 Description

The program models a battle system where units (mages and entities) fight using spells and direct damage. It includes functionalities for simulating fights, applying spells, healing, and checking battle outcomes.

### ✅ Features:
- **Entity and Mage System**: Each unit has health and can perform actions.
- **Battle Mechanics**: Units take damage, cast spells, and fight until one army is eliminated.
- **Healing System**: Supports single-unit healing and chain healing effects.
- **Spell Effects**: Different mages have unique spell behaviors.
- **Army Management**: Supports unit sorting, filtering, and health checking.

## 📜 Implemented Concepts

- **Algebraic Data Types** (`data` for different entities and states)
- **Pattern Matching** (for different cases in combat logic)
- **Recursion** (handling repeated battle actions)
- **Higher-Order Functions** (spells applied dynamically)
- **Custom Typeclasses** (`Show` and `Eq` instances for custom data types)

## 🛠️ How It Works

### 1. Define Units and Spells  
- Mages (`Master`) have unique spells.  
- Entities include `Golem` and `HaskellElemental`.  

### 2. Battle Mechanics  
- The `fight` function processes battles turn by turn.  
- Units attack based on predefined rules.  
- The battle continues until one army is defeated.  

### 3. Additional Features  
- `haskellBlast`: Targets multiple enemies with area damage.  
- `multiHeal`: Heals units sequentially.  
- `chain`: Applies healing and damage alternately.  

## 📌 Example Code Snippets

### Define a mage:
```haskell
papi = Master "Papi" 126 (\enemyHP -> if enemyHP < 8 then 0 else if even enemyHP then div (enemyHP * 3) 4 else enemyHP - 3)
```
### Check if a unit is alive:
```haskell
isUnitAlive :: Unit -> Bool
isUnitAlive (M (Alive _)) = True
isUnitAlive (E (Alive _)) = True
isUnitAlive _ = False
```
### Simulate a battle:
```haskell
battle :: Army -> EnemyArmy -> Maybe Army
battle x y
  | countAliveUnits x == 0 && countAliveUnits y == 0 = Nothing
  | countAliveUnits x == 0 = Just y
  | countAliveUnits y == 0 = Just x
  | otherwise = battle (formationFix (multiHeal 20 (haskellBlast (fight y x)))) (formationFix(fight x y))
```
---

⭐ This was a university project and is not actively maintained.
