showState a = show a
showMage a = show a
eqMage a b =  a == b
showUnit a = show a
showOneVOne a = show a 

type Name = String
type Health = Integer
type Spell = (Integer -> Integer)
type Army = [Unit]
type EnemyArmy = Army
type Amount = Integer

papi = let 
    tunderpor enemyHP
        | enemyHP < 8 = 0
        | even enemyHP = div (enemyHP * 3) 4
        | otherwise = enemyHP - 3
    in Master "Papi" 126 tunderpor
java = Master "Java" 100 (\x ->  x - (mod x 9))
traktor = Master "Traktor" 20 (\x -> div (x + 10) ((mod x 4) + 1))
jani = Master "Jani" 100 (\x -> x - div x 4)
skver = Master "Skver" 100 (\x -> div (x+4) 2)
potionMaster = 
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7 
  in Master "PotionMaster" 170 plx

-- 1
-- a)
data State a = Alive a | Dead
  deriving (Eq)

instance Show a => Show (State a) where
 show (Alive a) = show a
 show Dead = "Dead"

-- b)
data Entity = Golem Health | HaskellElemental Health
  deriving(Eq, Show)

-- c)
data Mage = Master Name Health Spell

instance Show Mage where
  show (Master x hp _) 
    | hp < 5 = "Wounded " ++ x
    | otherwise = x

instance Eq Mage where
  (Master name hp _) == (Master name2 hp2 _) 
    | name == name2 && hp == hp2 = True
    | otherwise = False

-- d)
data Unit = M (State Mage) | E (State Entity) 
  deriving(Eq)

instance Show Unit where
  show (M (Alive x)) = show x
  show (M (Dead)) = "Dead"
  show (E (Alive x)) = show x
  show (E (Dead)) = "Dead"

-- -- 2
-- -- a)
isUnitAlive :: Unit -> Bool
isUnitAlive (M (Alive _)) = True
isUnitAlive (E (Alive _)) = True
isUnitAlive _ = False

getAliveUnits :: Army -> Army
getAliveUnits [] = []
getAliveUnits (x:xs)
  | isUnitAlive x = x : (getAliveUnits xs)
  | otherwise = getAliveUnits xs

getDeadUnits :: Army -> Army
getDeadUnits [] = []
getDeadUnits (x:xs)
  | isUnitAlive x = getDeadUnits xs
  | otherwise = x : (getDeadUnits xs)

formationFix :: Army -> Army
formationFix x = getAliveUnits x ++ getDeadUnits x

-- 3
over :: Army -> Bool
over x = getAliveUnits x == []

-- másik megoldás
-- over' :: Army -> Bool
-- over' x = filter isUnitAlive x == []

-- 4
damageUnitBy :: Integer -> Unit -> Unit
-- Ha mágus
damageUnitBy dmg (M (Alive (Master name hp spell)))
  | hp > dmg = (M (Alive (Master name (hp-dmg) spell)))
  | otherwise = (M (Dead))
-- Ha golem
damageUnitBy dmg (E (Alive (Golem hp)))
  | hp > dmg = (E (Alive (Golem (hp-dmg))))
  | otherwise = (E (Dead))
-- Ha HaskellElemental
damageUnitBy dmg (E (Alive (HaskellElemental hp)))
  | hp > dmg = (E (Alive (HaskellElemental (hp-dmg))))
  | otherwise = (E (Dead))
damageUnitBy _ x = x

damageUnitBySpell :: Unit -> Spell -> Unit
-- Ha mágus
damageUnitBySpell (M (Alive (Master name hp s))) spell
  | spell hp > 0 = (M (Alive (Master name (spell hp) s)))
  | otherwise = (M (Dead))
-- Ha golem
damageUnitBySpell (E (Alive (Golem hp))) spell
  | spell hp > 0 = (E (Alive (Golem (spell hp))))
  | otherwise = (E (Dead))
-- Ha HaskellElemental
damageUnitBySpell (E (Alive (HaskellElemental hp))) spell
  | spell hp > 0 = (E (Alive (HaskellElemental (spell hp))))
  | otherwise = (E (Dead))
damageUnitBySpell x _ = x


damageArmyBySpell :: Army -> Spell -> Army
damageArmyBySpell [] _ = []
damageArmyBySpell (x:xs) spell = (damageUnitBySpell x spell) : damageArmyBySpell xs spell 


fight :: EnemyArmy -> Army -> Army
fight [] x = x
fight _ [] = []
fight (E (Alive (Golem _)):xs) (y:ys) = (damageUnitBy 1 y) : fight xs ys
fight (E (Alive (HaskellElemental _)):xs) (y:ys) = (damageUnitBy 3 y) : fight xs ys
fight (M (Alive (Master _ hp spell)):xs) (y:ys) = (damageUnitBySpell y spell) : (fight xs (damageArmyBySpell ys spell))
fight (x:xs) (y:ys) = y : fight xs ys

-- 5
getHealth :: Unit -> Health
-- Ha mágus
getHealth (M (Alive (Master _ hp _))) = hp
-- Ha golem
getHealth (E (Alive (Golem hp))) = hp
-- Ha HaskellElemental
getHealth (E (Alive (HaskellElemental hp))) = hp
-- Ha halott
getHealth (E Dead) = 0
getHealth (M Dead) = 0

-- hány damaget tud kiosztani a blast
-- az 5-öt paraméterként kell megadni, hogy aztán tudjon levonni belőle rekurzióval
dmgOnArea :: Army -> Integer -> Health
dmgOnArea _ 0 = 0
dmgOnArea [] _ = 0
dmgOnArea (x:xs) y
  | getHealth x > 5 = (dmgOnArea xs (y-1)) + 5
  | otherwise = (getHealth x) + (dmgOnArea xs (y-1))

maxBlast :: Army -> Health -> Health
maxBlast [] x = x
maxBlast army curr
  | (dmgOnArea army 5) > curr = maxBlast (tail army) (dmgOnArea army 5)
  | curr == 25 = 25
  | otherwise = maxBlast (tail army) curr

--ha ugyanannyit damagelt az aktuális armyba mint amennyi a maxBlast, és counter==0, akkor damagel
--ha counter > 0, azt jelenti hogy megtaláltuk az 5 karaktert, tehát csak damageljen
--egyébként meg csak fűzze hozzá
haskellBlast :: Army -> Army
haskellBlast [] = []
haskellBlast army = helper army 0 where
  helper :: Army -> Int -> Army
  helper [] _ = []
  helper (x:xs) counter
    | counter == 0 && (dmgOnArea (x:xs) 5) == (maxBlast (x:xs) 0) = (damageUnitBy 5 x) : (helper xs (counter+1))
    | counter > 0 && counter < 5 = (damageUnitBy 5 x) : (helper xs (counter+1))
    | otherwise = x : helper xs counter

-- 6
-- végtelen listára nem működik
countAliveUnits :: Army -> Integer
countAliveUnits xs = toInteger $ length $ getAliveUnits xs

applyHealing :: Integer -> Unit -> Unit
-- Ha mágus
applyHealing heal (M (Alive (Master name hp spell))) = (M (Alive (Master name (hp+heal) spell)))
-- Ha golem
applyHealing heal (E (Alive (Golem hp))) = (E (Alive (Golem (hp+heal))))
-- Ha HaskellElemental
applyHealing heal (E (Alive (HaskellElemental hp))) = (E (Alive (HaskellElemental (hp+heal))))
applyHealing heal x = x

allDead :: Army -> Bool
allDead = all isUnitDead

isUnitDead :: Unit -> Bool
isUnitDead (E Dead) = True
isUnitDead (M Dead) = True
isUnitDead _ = False

multiHeal :: Health -> Army -> Army
multiHeal _ [] = []
multiHeal heal xs
  | heal <= 0 = xs
  | allDead xs = xs
multiHeal heal xs = helper heal xs [] where
  helper :: Health -> Army -> Army -> Army
  helper 0 xs ys = ys ++ xs
  helper heal [] ys = helper heal ys []
  helper heal (x:xs) ys
    | isUnitAlive x = helper (heal-1) xs (ys++[applyHealing 1 x])
    | otherwise = helper heal xs (ys++[x])



getCurrentHealth :: Unit -> Health
getCurrentHealth (M (Alive (Master _ hp _))) = hp
getCurrentHealth (E (Alive (Golem hp))) = hp
getCurrentHealth (E (Alive (HaskellElemental hp))) = hp
getCurrentHealth _ = 0

-- 7
battle :: Army -> EnemyArmy -> Maybe Army
battle [] [] = Nothing
battle [] x = Just x
battle x [] = Just x
battle x y
  | countAliveUnits x == 0 && countAliveUnits y == 0 = Nothing
  | countAliveUnits x == 0 = Just y
  | countAliveUnits y == 0 = Just x
  | otherwise = battle (formationFix (multiHeal 20 (haskellBlast (fight y x)))) (formationFix(fight x y))

-- 8
damageArmyBy :: Integer -> Army -> Army
damageArmyBy _ [] = []
damageArmyBy dmg (x:xs) = (damageUnitBy dmg x) : damageArmyBy dmg xs

hasDeadUnit :: Army -> Bool
hasDeadUnit xs = (length $ getDeadUnits xs) /= 0

applyChainHealing :: Amount -> Army -> Army
applyChainHealing _ [] = []
applyChainHealing 0 x = x
applyChainHealing amount (x:xs) = applyHealing amount x : applyChainHealing (amount-2) xs

applyChainDamage :: Amount -> Army -> Army
applyChainDamage _ [] = []
applyChainDamage 0 x = x
applyChainDamage amount (x:xs) = damageUnitBy amount x : applyChainDamage (amount-2) xs

chain :: Amount -> (Army, EnemyArmy) -> (Army, EnemyArmy)
chain _ ([], y) = ([], y)
chain amount x 
  | amount <= 0 = x
chain amount x = helper amount x 1 [] [] where
  helper :: Amount -> (Army, EnemyArmy) -> Int -> Army -> Army -> (Army, EnemyArmy)
  helper amount ([],[]) counter myTeam enemyTeam = (reverse $ myTeam, reverse $ enemyTeam)
  -- ha már 0 az amount, csak adja vissza a maradékot
  helper 0 (xs,ys) counter myTeam enemyTeam = (reverse $ xs ++ myTeam, reverse $ ys ++ enemyTeam)
  -- ha valamelyik lista üres, akkor még utoljára végezzen el egy műveletet, aztán 0 legyen az amount (csak feltölti a maradékot)
  helper amount ([],y:ys) counter myTeam enemyTeam = helper 0 ([], ys) (counter + 1) myTeam ((damageUnitBy amount $ y) : enemyTeam) 
  helper amount (x:xs,[]) counter myTeam enemyTeam = helper 0 (xs, []) (counter + 1) ((applyHealing amount $ x) : myTeam) enemyTeam
  -- ha a listában 1 halott van (E vagy M-re lekezelve) akkor még folytatódjon a dmg/heal
  helper amount ([E Dead],y:ys) counter myTeam enemyTeam = helper (amount-1) ([], (ys)) (counter + 1) ((E Dead) : myTeam) ((damageUnitBy amount $ y) : enemyTeam) 
  helper amount ([M Dead],y:ys) counter myTeam enemyTeam = helper (amount-1) ([], (ys)) (counter + 1) ((E Dead) : myTeam) ((damageUnitBy amount $ y) : enemyTeam) 
  helper amount (x:xs,[E Dead]) counter myTeam enemyTeam = helper (amount-1) (xs, []) (counter + 1) ((applyHealing amount $ x) : myTeam) ((E Dead) : enemyTeam)
  helper amount (x:xs,[M Dead]) counter myTeam enemyTeam = helper (amount-1) (xs, []) (counter + 1) ((applyHealing amount $ x) : myTeam) ((E Dead) : enemyTeam)
  --ha nem üresek a listák, akkor turnonként healel/damagel karaktereket
  helper amount ((x:xs),(y:ys)) counter myTeam enemyTeam
  -- ha dead valamelyik
    | not (isUnitAlive x) && counter `mod` 2 /= 0 = helper (amount-1) ((xs),(y:ys)) (counter+1) (x : myTeam) enemyTeam
    | not (isUnitAlive y) && counter `mod` 2 == 0 = helper (amount-1) ((x:xs),(ys)) (counter+1) myTeam (y : enemyTeam)
  -- saját csapat turnje
    | counter `mod` 2 /= 0 && amount /= 0 = helper (amount-1) ((xs),(y:ys)) (counter+1) ((applyHealing amount x) : myTeam) enemyTeam
    | counter `mod` 2 /= 0 && amount == 0 = helper (amount-1) ((xs),(y:ys)) (counter+1) (x : myTeam) enemyTeam
  -- enemy team turnje
    | counter `mod` 2 == 0 && amount /= 0 = helper (amount-1) ((x:xs),(ys)) (counter+1) myTeam ((damageUnitBy amount y) : enemyTeam)
    | counter `mod` 2 == 0 && amount == 0 = helper (amount-1) ((x:xs),(ys)) (counter+1) myTeam (y : enemyTeam)

-- 9
battleWithChain :: Army -> EnemyArmy -> Maybe Army
battleWithChain [] [] = Nothing
battleWithChain [] x = Just x
battleWithChain x [] = Just x
battleWithChain x y
  | countAliveUnits x == 0 && countAliveUnits y == 0 = Nothing 
  | countAliveUnits x == 0 = Just y
  | countAliveUnits y == 0 = Just x
  | otherwise = battleWithChain (formationFix (fst (chain 5 ((multiHeal 20 (haskellBlast (fight y x))),y)))) (formationFix (snd (chain 5 (x,(fight x y)))))

-- 10
data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne deriving Eq
instance Show OneVOne where
  show x = helper (show' x) where
    helper :: String -> String
    helper xs = "<" ++ xs ++ ">"
    show' :: OneVOne -> String
    show' (Winner name) = "|| Winner " ++ name ++ " ||"
    show' (You health rest) = "You " ++ show health ++ "; " ++ show' rest
    show' (HaskellMage health rest) = "HaskellMage " ++ show health ++ "; " ++ show' rest