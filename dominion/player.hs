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
import Dominion

--Player
--
--
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
getCard "copper" = Just $ Treasure Copper
getCard "silver" = Just $ Treasure Silver
getCard "gold" = Just $ Treasure Gold
getCard _ = Nothing
 
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


--helper functions
isVictory :: Card -> Bool
isVictory (Victory _) = True
isVictory _ = False

isAction :: Card -> Bool
isAction (Action _) = True
isAction _ = False

isTreasure :: Card -> Bool
isTreasure (Treasure _) = True
isTreasure _ = False

isCost :: Int -> Card -> Bool
isCost cost card = if cardCost card == cost then
                        True
                    else
                        False

bestBuyable :: Int -> [Card] -> Card
bestBuyable maxprice supply = let cardscosting = filter (isCost maxprice) supply in
    if length cardscosting == 0 then
        bestBuyable (maxprice - 1) supply
    else
        if isJust $ find isAction cardscosting then
            fromJust $ find isAction cardscosting
        else if isJust $ find isTreasure cardscosting then
            fromJust $ find isTreasure cardscosting
        else if isJust $ find isVictory cardscosting then
            fromJust $ find isVictory cardscosting
        else
            head cardscosting


doRuthlessRemodel :: [Card] -> [Card] -> (Card,Card)
doRuthlessRemodel hand supply = let hand_cheapest = minimumBy (\a b -> compare (cardCost a) (cardCost b) ) hand in 
    ( hand_cheapest, bestBuyable ((cardCost hand_cheapest) + 2) supply )


--Actual move function
move :: State -> Play
--move state | trace ( "move: state: " ++ show state ) False = undefined
move state@State{actions=a, buys=b, coins=c, hand=h, supply=s}
--Play best possible action
    | a >= 1 && b >= 1 && (Action Market) `elem` h = ActMarket
    | a >= 1 && b >= 1 && (Action Village) `elem` h = ActVillage
    | a >= 1 && b >= 1 && (Action Cellar) `elem` h && (length $ filter isVictory h) > 0 = ActCellar $ filter isVictory h
    | a >= 1 && b >= 1 && (Action Mine) `elem` h && (Treasure Copper) `elem` h && (Treasure Silver) `elem` s = ActMine Copper Silver
    | a >= 1 && b >= 1 && (Action Mine) `elem` h && (Treasure Silver) `elem` h && (Treasure Gold) `elem` s = ActMine Silver Gold
    | a >= 1 && b >= 1 && (Action Mine) `elem` h && (Treasure Copper) `elem` h && (Treasure Silver) `elem` s = ActMine Copper Silver
    | a >= 1 && b >= 1 && (Action Remodel) `elem` h = let cards = doRuthlessRemodel ( delete (Action Remodel) h ) s in
        (ActRemodel (fst cards) (snd cards) )
    | a >= 1 && b >= 1 && (Action Smithy) `elem` h = ActSmithy
    | a >= 1 && b >= 1 && (Action Woodcutter) `elem` h = ActWoodcutter
    | a >= 1 && b >= 1 && (Action Workshop) `elem` h = ActWorkshop (bestBuyable 4 s)
--Play all treasure cards
    | b >= 1 && (Treasure Copper) `elem` h = Add Copper
    | b >= 1 && (Treasure Silver) `elem` h = Add Silver
    | b >= 1 && (Treasure Gold) `elem` h = Add Gold
--Added all coins, now use them wisely
    | b >= 1 && c >= 8 && (Victory Province) `elem` s = Buy (Victory Province)
    | b >= 1 && c >= 6 && (Treasure Gold) `elem` s = Buy (Treasure Gold)
    | b >= 1 && c >= 5 && (Action Market) `elem` s = Buy (Action Market)
    | b >= 1 && c >= 5 && (Action Mine) `elem` s = Buy (Action Mine)
    | b >= 1 && c >= 5 && (Victory Duchy) `elem` s = Buy (Victory Duchy)
    | b >= 1 && c >= 4 && (Action Remodel) `elem` s = Buy (Action Remodel)
    | b >= 1 && c >= 4 && (Action Smithy) `elem` s = Buy (Action Smithy)
    | b >= 1 && c >= 3 && (Treasure Silver) `elem` s = Buy (Treasure Silver)
    | b >= 1 && c >= 3 && (Action Workshop) `elem` s = Buy (Action Workshop)
    | b >= 1 && c >= 3 && (Action Village) `elem` s = Buy (Action Village)
    | b >= 1 && c >= 3 && (Action Woodcutter) `elem` s = Buy (Action Woodcutter)
    | b >= 1 && c >= 2 && (Action Cellar) `elem` s = Buy (Action Cellar)
    | b >= 1 && c >= 2 && (Victory Estate) `elem` s = Buy (Victory Estate)
    | b >= 1 && c >= 0 && (Treasure Copper) `elem` s = Buy (Treasure Copper)
--Bummer
    | length h > 0 = (Cleanc (head h))
    | otherwise = Clean


main :: IO()
main = do 
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    interact playerProcess

playerProcess :: String -> String
playerProcess input = unlines $ map show $ map move $ parseStates (0,[]) input

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
                            

