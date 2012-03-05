module Dominion where


data Phase = PhaseAction | PhaseBuy | PhaseCleanup deriving (Eq, Show, Read, Enum, Ord)
data Victory = Estate | Duchy | Province deriving (Eq, Show, Read, Enum, Ord)
data Action = Mine | Woodcutter  deriving (Eq, Show, Read, Enum)
data Treasure = Copper | Silver | Gold deriving (Eq, Show, Read, Enum, Ord)
data Card = Treasure Treasure | Victory Victory | Action Action deriving (Eq, Show, Read)
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



data Play = ActMine Treasure Treasure |
            Add Treasure |
            Buy Card |
            Clean | Cleanc Card deriving (Show)


