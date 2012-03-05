{-| Dominion player v1
 -  Srikanth Raju
 -}
import System.Random ( Random, RandomGen, randoms, newStdGen )
import System.Random.Mersenne.Pure64
import System.IO
import Debug.Trace
import Data.List
import Data.Maybe
import System.Random
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe

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


--Player
--
--
getCard :: String -> Maybe Card
getCard "estate" = Just $ Victory Estate
getCard "duchy" = Just $ Victory Duchy
getCard "province" = Just $ Victory Province
getCard "mine" = Just $ Action Mine
getCard "copper" = Just $ Treasure Copper
getCard "silver" = Just $ Treasure Silver
getCard "gold" = Just $ Treasure Gold
getCard _ = Nothing
 
showCard :: Card -> String
showCard (Victory Estate) = "estate"
showCard (Victory Duchy) = "duchy"
showCard (Victory Province) = "province"
showCard (Action Mine) = "mine"
showCard (Treasure Copper) = "copper"
showCard (Treasure Silver) = "silver"
showCard (Treasure Gold) = "gold"

--Full string needs to be a move
parseState :: String -> Maybe State -> Maybe State
--parseState str state | trace ( "parseState: str: " ++ show str ++ "\nState: " ++ show state ) False = undefined
parseState [] st = st
parseState (')':[]) st = st
parseState (' ':[]) st = st
parseState (' ':xs) st = parseState xs st
parseState (')':xs) st = parseState xs st
parseState ('\t':xs) st = parseState xs st
parseState ('\n':xs) st = parseState xs st
parseState string Nothing
    | Just xs <- stripPrefix "(moved" string = Nothing
parseState string Nothing
    | Just xs <- stripPrefix "(move" string = parseState xs (Just State{players=[], supply=[],trash=[],deck=[],hand=[],discards=[],plays=[],actions=0,buys=0,coins=0})
parseState string (Just st)
    | Just xs <- stripPrefix "(players" string = parseState (drop 1 $ snd process) (Just st {players = words $ fst process }) 
    | Just xs <- stripPrefix "(trash" string = parseState (drop 1 $ dropWhile (/= ')') xs) (Just st {trash = mapMaybe getCard $ words $ takeWhile (/= ')') xs})
    | Just xs <- stripPrefix "(supply" string = parseState (drop 1 $ dropWhile (/= ')') xs) (Just st {supply = mapMaybe getCard $ words $ takeWhile (/= ')') xs})
    | Just xs <- stripPrefix "(deck" string = parseState (drop 1 $ dropWhile (/= ')') xs) (Just st {deck = mapMaybe getCard $ words $ takeWhile (/= ')') xs})
    | Just xs <- stripPrefix "(hand" string = parseState (drop 1 $ dropWhile (/= ')') xs) (Just st {hand = mapMaybe getCard $ words $ takeWhile (/= ')') xs})
    | Just xs <- stripPrefix "(plays" string = parseState (drop 1 $ dropWhile (/= ')') xs) (Just st {plays = mapMaybe getCard $ words $ takeWhile (/= ')') xs})
    | Just xs <- stripPrefix "(discards" string = parseState (drop 1 $ dropWhile (/= ')') xs) (Just st {discards = mapMaybe getCard $ words $ takeWhile (/= ')') xs})
    | Just xs <- stripPrefix "(actions" string = parseState (drop 1 $ snd process) (Just st {actions = read $ fst process })
    | Just xs <- stripPrefix "(coins" string = parseState (drop 1 $ snd process) (Just st {coins = read $ fst process })
    | Just xs <- stripPrefix "(buys" string = parseState (drop 1 $ snd process) (Just st {buys = read $ fst process })
    where process = span ( /= ')' ) $ unwords $ drop 1 $ words string
parseState ('(':xs) st = parseState xs st 

moved :: String -> Play -> Bool
moved _ _ = True 

move :: State -> Play
--move state | trace ( "move: state: " ++ show state ) False = undefined
move state@State{actions=a, buys=b, coins=c, hand=h, supply=s}
--Play best possible action
    | a >= 1 && b >= 1 && (Action Mine) `elem` h && (Treasure Silver) `elem` h && (Treasure Gold) `elem` s = ActMine Silver Gold
    | a >= 1 && b >= 1 && (Action Mine) `elem` h && (Treasure Copper) `elem` h && (Treasure Silver) `elem` s = ActMine Copper Silver
--Play all treasure cards
    | b >= 1 && (Treasure Copper) `elem` h = Add Copper
    | b >= 1 && (Treasure Silver) `elem` h = Add Silver
    | b >= 1 && (Treasure Gold) `elem` h = Add Gold
--Added all coins, now use them wisely
    | b >= 1 && c >= 8 && (Victory Province) `elem` s = Buy (Victory Province)
    | b >= 1 && c >= 6 && (Treasure Gold) `elem` s = Buy (Treasure Gold)
    | b >= 1 && c >= 5 && (Action Mine) `elem` s = Buy (Action Mine)
    | b >= 1 && c >= 5 && (Victory Duchy) `elem` s = Buy (Victory Duchy)
    | b >= 1 && c >= 3 && (Treasure Silver) `elem` s = Buy (Treasure Silver)
    | b >= 1 && c >= 2 && (Victory Estate) `elem` s = Buy (Victory Estate)
    | b >= 1 && c >= 0 && (Treasure Copper) `elem` s = Buy (Treasure Copper)
--Bummer
    | length h > 0 = (Cleanc (head h))
    | otherwise = Clean

showPlay :: Play -> String
showPlay (ActMine x y) = "(act mine " ++ showCard (Treasure x) ++ " " ++ showCard (Treasure y) ++ ")"
showPlay (Add x) = "(add " ++ showCard (Treasure x) ++ ")"
showPlay (Buy x) = "(buy " ++ showCard x ++ ")"
showPlay Clean = "(clean)"
showPlay (Cleanc x) = "(clean " ++ showCard x ++ ")"



main :: IO()
main = do 
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    interact playerProcess

playerProcess :: String -> String
playerProcess input = unlines $ map showPlay $ map move $ parseStates (0,[]) input

getMove :: Char -> (Int, String) -> (Int, String)
--joinMove now ( lvl, prev ) | trace ( "joinMove: " ++ now ++ ":" ++ show lvl ++ ":" ++ prev ) False = undefined
getMove now ( lvl, prev ) = if now == '(' then
                                ( lvl + 1, prev ++ [now] )
                             else if now == ')' then
                                ( lvl - 1, prev ++ [now] )
                             else
                                ( lvl, prev ++ [now] )



parseStates :: (Int, String) -> String -> [State]
--parseStates (lvl, prev) (str:xs)  | trace ( "parseStates: " ++ show lvl ++ ":" ++ prev ++ ":" ++ str) False = undefined
parseStates (a,b) (input:xs) = let res = getMove input (a,b) in
                        if fst res == 0 then
                            let state = ( parseState (snd res) Nothing ) in
                                if isJust state then
                                    fromJust state : parseStates (0,[]) xs
                                else
                                    parseStates(0,[]) xs
                        else
                            parseStates res xs
                            

