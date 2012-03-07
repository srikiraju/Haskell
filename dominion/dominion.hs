module Dominion where


data Phase = PhaseAction | PhaseBuy | PhaseCleanup deriving (Eq, Show, Read, Enum, Ord)
data Victory = Estate | Duchy | Province deriving (Eq, Show, Read, Enum, Ord)
data Action = Mine | Cellar | Market | Remodel | Smithy | Village | Woodcutter | Workshop | Militia | Moat  deriving (Eq, Show, Read, Enum)
data Treasure = Copper | Silver | Gold deriving (Eq, Show, Read, Enum, Ord)
data Card = Treasure Treasure | Victory Victory | Action Action deriving (Eq, Read)
data State = State { players :: [String],
                     supply :: [Card],
                     trash :: [Card],
                     actions :: Int,
                     buys :: Int,
                     coins :: Int,
                     deck :: [Card],
                     hand :: [Card],
                     plays :: [Card],
                     discards ::[Card] } deriving (Show);

data GlobalState = GlobalState { gplayers :: [String],
                     gsupply :: [Card],
                     gtrash :: [Card],
                     playerdecks :: [[Card]],
                     playerhands :: [[Card]],
                     playsinprogress :: [Card],
                     playerdiscards ::[[Card]],
                     phase :: Phase } deriving (Show);

data Notification = Move State  |
                    Moved String Play |
                    Attacked Play String State |
                    Defended String Defense deriving (Show);

data Defense = MoatDef |
               Discard [Card]


data Play = ActMine Treasure Treasure |
            ActCellar [Card] |
            ActMarket |
            ActRemodel Card Card |
            ActSmithy |
            ActVillage |
            ActWoodcutter |
            ActWorkshop Card |
            ActMilitia |
            ActMoat |
            Add Treasure |
            Buy Card |
            Clean | Cleanc Card


instance Show Card where
    show (Victory Estate) = "estate"
    show (Victory Duchy) = "duchy"
    show (Victory Province) = "province"
    show (Action Mine) = "mine"
    show (Action Cellar) = "cellar"
    show (Action Market) = "market"
    show (Action Remodel) = "remodel"
    show (Action Smithy) = "smithy"
    show (Action Village) = "village"
    show (Action Woodcutter) = "woodcutter"
    show (Action Workshop) = "workshop"
    show (Action Moat) = "moat"
    show (Action Militia) = "militia"
    show (Treasure Copper) = "copper"
    show (Treasure Silver) = "silver"
    show (Treasure Gold) = "gold"


instance Show Play where
    show (ActMine x y) = "(act mine " ++ show (Treasure x) ++ " " ++ show (Treasure y) ++ ")"
    show (ActCellar x) = "(act cellar " ++ foldl (++) "" (map (\x -> x ++ " ") (map show x)) ++ ")"
    show (ActMarket) = "(act market)"
    show (ActRemodel x y) = "(act remodel " ++ show x ++ " " ++ show y ++ ")"
    show (ActSmithy) = "(act smithy)"
    show (ActVillage) = "(act village)"
    show (ActWoodcutter) = "(act woodcutter)"
    show (ActWorkshop x) = "(act workshop " ++ show x ++ ")"
    show (ActMoat) = "(act moat)"
    show (ActMilitia) = "(act militia)"
    show (Add x) = "(add " ++ show (Treasure x) ++ ")"
    show (Buy x) = "(buy " ++ show x ++ ")"
    show Clean = "(clean)"
    show (Cleanc x) = "(clean " ++ show x ++ ")"

instance Show Defense where
    show (MoatDef) = "(moat)"
    show (Discard x) = "(discard " ++ foldl (++) "" (map (\x -> x ++ " ") (map show x)) ++ ")"

instance Ord Card where
    compare x y = compare (cardCost x) (cardCost y)

--Buying cost
cardCost :: Card -> Int
cardCost (Treasure Copper) = 0
cardCost (Treasure Silver) = 3
cardCost (Treasure Gold) = 6
cardCost (Victory Estate) = 2
cardCost (Victory Duchy) = 5
cardCost (Victory Province) = 8
cardCost (Action Mine) = 5
cardCost (Action Cellar) = 2
cardCost (Action Market) = 5
cardCost (Action Remodel) = 4
cardCost (Action Smithy) = 4
cardCost (Action Village) = 3
cardCost (Action Woodcutter) = 3
cardCost (Action Workshop) = 3
cardCost (Action Moat) = 2
cardCost (Action Militia) = 4
cardCost _ = error "Card cost of what?"

getCard :: String -> Maybe Card
getCard "estate" = Just $ Victory Estate
getCard "duchy" = Just $ Victory Duchy
getCard "province" = Just $ Victory Province
getCard "mine" = Just $ Action Mine
getCard "cellar" = Just $ Action Cellar
getCard "market" = Just $ Action Market
getCard "remodel" = Just $ Action Remodel
getCard "smithy" = Just $ Action Smithy
getCard "village" = Just $ Action Village
getCard "woodcutter" = Just $ Action Woodcutter
getCard "workshop" = Just $ Action Workshop
getCard "moat" = Just $ Action Moat
getCard "militia" = Just $ Action Militia
getCard "copper" = Just $ Treasure Copper
getCard "silver" = Just $ Treasure Silver
getCard "gold" = Just $ Treasure Gold
getCard _ = Nothing

getTreasure :: Card -> Maybe Treasure
getTreasure (Treasure x) = Just x
getTreasure _ = Nothing

isVictory :: Card -> Bool 
isVictory (Victory _) = True 
isVictory _ = False 
 
isAction :: Card -> Bool 
isAction (Action _) = True 
isAction _ = False 
 
isTreasure :: Card -> Bool 
isTreasure (Treasure _) = True 
isTreasure _ = False 

isCopper :: Card -> Bool
isCopper (Treasure Copper) = True
isCopper _ = False
