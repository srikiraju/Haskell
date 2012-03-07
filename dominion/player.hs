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

data LocalState = LocalState { needMoat :: Bool, moatBump :: Float, end :: Float } deriving (Show);

--Assuming it gets full play including "(play"
parsePlay :: String -> Play 
--parsePlay str | trace ( "parsePlay: " ++ str ) False = undefined
parsePlay [] = error "Empty play??"
parsePlay str | Just xs <- stripPrefix "(act mine " str = let cards = mapMaybe getTreasure $ mapMaybe getCard $ words $ takeWhile (/= ')') xs in
            if length cards == 2 then
                ActMine (cards !! 0) (cards !! 1)
            else
                error "parsePlay failed to parse act mine"
parsePlay str | Just xs <- stripPrefix "(act cellar " str = let cards = mapMaybe getCard $ words $ takeWhile (/= ')') xs in
            if length cards /= 0 then
                ActCellar cards
            else
                error "parsePlay failed to parse act cellar"
parsePlay str | Just xs <- stripPrefix "(act market)" str = ActMarket
parsePlay str | Just xs <- stripPrefix "(act remodel " str = let cards = mapMaybe getCard $ words $ takeWhile (/= ')') xs in
            if length cards == 2 then
                ActRemodel (cards !! 0) (cards !! 1)
            else
                error "parsePlay failed to parse act remodel"
parsePlay str | Just xs <- stripPrefix "(act smithy)" str = ActSmithy
parsePlay str | Just xs <- stripPrefix "(act village)" str = ActVillage
parsePlay str | Just xs <- stripPrefix "(act woodcutter)" str = ActWoodcutter
parsePlay str | Just xs <- stripPrefix "(act workshop " str =  let card = getCard $ takeWhile (/= ')') xs in
            if isJust card then
                ActWorkshop (fromJust card)
            else
                error "parsePlay failed to parse act workshop"
parsePlay str | Just xs <- stripPrefix "(act militia)" str = ActMilitia
parsePlay str | Just xs <- stripPrefix "(act moat)" str = ActMoat
parsePlay str | Just xs <- stripPrefix "(add " str = let card = getCard $ takeWhile (/= ')') xs in
            if isJust card && (isTreasure $ fromJust card) then
                Add (fromJust $ getTreasure $ fromJust card)
            else
                error "parsePlay failed to parse add" 
parsePlay str | Just xs <- stripPrefix "(buy " str = let card = getCard $ takeWhile (/= ')') xs in
            if isJust card then
                Buy (fromJust card)
            else
                error "parsePlay failed to parse buy"
parsePlay str | Just xs <- stripPrefix "(clean)" str = Clean
parsePlay str | Just xs <- stripPrefix "(clean " str = let card = getCard $ takeWhile (/= ')') xs in
            if isJust card then
                Cleanc (fromJust card)
            else
                error "parsePlay failed to parse clean"
parsePlay _ = error "Couldn't parse the play"


parseDefense :: String -> Defense
parseDefense str | Just xs <- stripPrefix "(moat)" str = MoatDef
parseDefense str | Just xs <- stripPrefix "(discard " str = let cards = mapMaybe getCard $ words $ takeWhile (/= ')') xs in
            if length cards > 0 then
                Discard cards
            else
                error "parseDefense failed to parse discard"
parseDefense _ = error "parseDefense failed to parse"

--Assumes rest of attacked, excluding "(attacked "
parseAttacked :: String -> Notification
--parseAttacked str | trace ( "parseAttacked: " ++ str ++ "\n" ++ show nests ) False = undefined
--    where nests = getMatchedBrackets (0,[]) str
parseAttacked string = let nests = getMatchedBrackets (0,[]) string in
            if length nests == 2 then
                Attacked (parsePlay $ nests !! 0) "oops" (parseState (nests !! 1) State{players=[], supply=[],trash=[],deck=[],hand=[],discards=[],plays=[],actions=0,buys=0,coins=0})
            else
                error "parseAttacked failed"

parseDefended :: String -> Notification
--parseDefended str | trace ( "parseDefended: " ++ str ++ "\n" ++ show nests ) False = undefined
--    where nests = getMatchedBrackets (0,[]) str
parseDefended string = let name = (words string) !! 0
                           brackets = getMatchedBrackets (0,[]) string in
                    if length brackets == 1 then
                        Defended name $ parseDefense (brackets !! 0)
                    else
                        error "parseDefended failed"


--Assuming it gets the rest of moved, excluding "(moved "
parseMoved :: String -> Notification
--parseMoved str | trace ( "parseMoved: " ++ str ) False = undefined
parseMoved str = let wrds = words str in 
                    Moved (head wrds) (parsePlay $ unwords $ drop 1 $ wrds)

parseNotif :: Maybe Notification -> String -> Maybe Notification
--parseNotif Nothing str | trace ( "parseNotif : " ++ str ++ "\n" ) False = undefined
parseNotif Nothing [] = Nothing
parseNotif Nothing (' ':xs) = parseNotif Nothing xs
parseNotif Nothing ('\t':xs) = parseNotif Nothing xs
parseNotif Nothing ('\n':xs) = parseNotif Nothing xs
parseNotif Nothing string
    | Just xs <- stripPrefix "(moved " string = Just $ parseMoved xs
parseNotif Nothing string
    | Just xs <- stripPrefix "(move " string = Just $ Move (parseState xs State{players=[], supply=[],trash=[],deck=[],hand=[],discards=[],plays=[],actions=0,buys=0,coins=0})
parseNotif Nothing string
    | Just xs <- stripPrefix "(attacked " string = Just $ parseAttacked xs
parseNotif Nothing string
    | Just xs <- stripPrefix "(defended " string = Just $ parseDefended xs
parseNotif _ _ = Nothing


--Full string needs to be a move
parseState :: String -> State -> State
--parseState str state | trace ( "parseState: str: " ++ show str ++ "\nState: " ++ show state ) False = undefined
parseState [] st = st
parseState (')':[]) st = st
parseState (' ':[]) st = st
parseState (' ':xs) st = parseState xs st
parseState (')':xs) st = parseState xs st
parseState ('\t':xs) st = parseState xs st
parseState ('\n':xs) st = parseState xs st
parseState string st
    | Just xs <- stripPrefix "(players" string = parseState (drop 1 $ snd process) st {players = words $ fst process }
    | Just xs <- stripPrefix "(trash" string = parseState (drop 1 $ dropWhile (/= ')') xs) st {trash = mapMaybe getCard $ words $ takeWhile (/= ')') xs}
    | Just xs <- stripPrefix "(supply" string = parseState (drop 1 $ dropWhile (/= ')') xs) st {supply = mapMaybe getCard $ words $ takeWhile (/= ')') xs}
    | Just xs <- stripPrefix "(deck" string = parseState (drop 1 $ dropWhile (/= ')') xs) st {deck = mapMaybe getCard $ words $ takeWhile (/= ')') xs}
    | Just xs <- stripPrefix "(hand" string = parseState (drop 1 $ dropWhile (/= ')') xs) st {hand = mapMaybe getCard $ words $ takeWhile (/= ')') xs}
    | Just xs <- stripPrefix "(plays" string = parseState (drop 1 $ dropWhile (/= ')') xs) st {plays = mapMaybe getCard $ words $ takeWhile (/= ')') xs}
    | Just xs <- stripPrefix "(discards" string = parseState (drop 1 $ dropWhile (/= ')') xs) st {discards = mapMaybe getCard $ words $ takeWhile (/= ')') xs}
    | Just xs <- stripPrefix "(actions" string = parseState (drop 1 $ snd process) st {actions = read $ fst process }
    | Just xs <- stripPrefix "(coins" string = parseState (drop 1 $ snd process) st {coins = read $ fst process }
    | Just xs <- stripPrefix "(buys" string = parseState (drop 1 $ snd process) st {buys = read $ fst process }
    where process = span ( /= ')' ) $ unwords $ drop 1 $ words string
parseState ('(':xs) st = parseState xs st 

moved :: LocalState -> String -> Play -> LocalState
moved ls@LocalState{moatBump = mb} _ (Buy (Action Militia)) = ls{ needMoat = True, moatBump = mb }
moved ls _ _ = ls

chooseDiscards :: Int -> [Card] -> [Card]
chooseDiscards num hand | length hand < 4 = []
chooseDiscards num hand | num <= 0 = []
chooseDiscards num hand | length victories > 0 = (take 1 victories) ++ chooseDiscards (num-1) (delete (head victories) hand)
    where victories = sort $ filter isVictory hand
chooseDiscards num hand = take num $ sort hand

attacked :: LocalState -> Play -> String -> State -> (LocalState, Defense)
attacked ls@LocalState{needMoat=nm, moatBump=mb} _ _ state@State{hand=h} =
    if (Action Moat) `elem` h then
        (ls{moatBump=0.0}, MoatDef)
    else
        (ls{needMoat=True, moatBump=0.4}, Discard (chooseDiscards (length h - 3) h ) )

defended :: String -> Defense -> String
defended _ _ = []

handleNotifs :: LocalState -> Notification -> (LocalState, String)
--handleNotifs notif | trace ( "handleNotifs: " ++ show notif ++ "\n") False = "" 
handleNotifs ls (Move s) = let res = move ls s in ( fst res, show $ snd res )
handleNotifs ls (Moved str play) = (moved ls str play, "")
handleNotifs ls (Attacked play plyr state) = let res = attacked ls play plyr state in
                                             ( fst res, show $ snd res )
handleNotifs ls (Defended plyr def) = (ls, defended plyr def)

isCost :: Int -> Card -> Bool
isCost cost card = if cardCost card == cost then
                        True
                    else
                        False

bestBuyable :: Int -> [Card] -> [Card] -> LocalState -> Maybe Card
--bestBuyable max supply all_cards | trace ( "max = " ++ show max ++ ", supply = " ++ show supply ++  ", all_cards= " ++ show all_cards ++ "\n" ) False = undefined
bestBuyable 0 supply all_cards _ = Nothing
bestBuyable maxprice supply all_cards ls@LocalState{end=e}
    | length affordables == 0 = Nothing
    | (isJust $ find isVictory affordables ) && e > 0.5 = find isVictory affordables
    | e > 0.75 = bestBuyable (maxprice-1) supply all_cards ls
    | Just sometreasure <- find isTreasure affordables = if (length $ filter ( == sometreasure ) all_cards) < 2 then Just sometreasure else bestBuyable maxprice (delete sometreasure supply) all_cards ls
    | Just someaction <- find isAction affordables = if (length $ filter ( == someaction ) all_cards) < 2 then Just someaction else bestBuyable maxprice (delete someaction supply) all_cards ls
    | otherwise = Just $ head affordables
    where affordables = filter (isCost maxprice) supply 


doRuthlessRemodel :: [Card] -> [Card] -> [Card] ->LocalState -> Maybe (Card,Card)
doRuthlessRemodel [] _ _ _ = Nothing
doRuthlessRemodel hand supply all_cards ls =
    let hand_cheapest = minimumBy (\a b -> compare (cardCost a) (cardCost b) ) hand 
        buy = bestBuyable ((cardCost hand_cheapest) + 2) (nub supply) all_cards ls in
            if isJust buy then 
                Just ( hand_cheapest, fromJust buy )
            else
                doRuthlessRemodel (delete hand_cheapest hand) (nub supply) all_cards ls


--Actual move function
move :: LocalState -> State -> (LocalState, Play)
--move state | trace ( "move: state: " ++ show state ) False = undefined
move ls@LocalState{needMoat=nm, moatBump=mb, end=e} state@State{actions=a, buys=b, coins=c, hand=h, supply=s, deck=d, discards=di, plays=p}
--Play best possible action
    | a >= 1 && b >= 1 && (Action Market) `elem` h =(newls, ActMarket)
    | a >= 1 && b >= 1 && (Action Village) `elem` h = (newls, ActVillage)
    | a >= 1 && b >= 1 && (Action Cellar) `elem` h && (length $ filter isVictory h) > 0 = (newls, ActCellar $ filter isVictory h)
    | a >= 1 && b >= 1 && (Action Mine) `elem` h && (Treasure Copper) `elem` h && (Treasure Silver) `elem` s = (newls, ActMine Copper Silver)
    | a >= 1 && b >= 1 && (Action Mine) `elem` h && (Treasure Silver) `elem` h && (Treasure Gold) `elem` s = (newls, ActMine Silver Gold)
    | a >= 1 && b >= 1 && (Action Militia) `elem` h = (newls, ActMilitia)
    | a >= 1 && b >= 1 && (Action Remodel) `elem` h && isJust remodel_cards = 
        (newls, (ActRemodel (fst $ fromJust remodel_cards) (snd $ fromJust remodel_cards)))
    | a >= 1 && b >= 1 && (Action Smithy) `elem` h = (newls, ActSmithy)
    | a >= 1 && b >= 1 && (Action Woodcutter) `elem` h = (newls, ActWoodcutter)
    | a >= 1 && b >= 1 && (Action Workshop) `elem` h && isJust workshop_cards = (newls, ActWorkshop $ fromJust workshop_cards)
    | a >= 1 && b >= 1 && (Action Moat) `elem` h = (newls, ActMoat)
--Play all treasure cards
    | b >= 1 && (Treasure Copper) `elem` h = (newls, Add Copper)
    | b >= 1 && (Treasure Silver) `elem` h = (newls, Add Silver)
    | b >= 1 && (Treasure Gold) `elem` h = (newls, Add Gold)
--Added all coins, now use them wisely
--Using a deck cycler strategy, focusing on Market/Village/Cellar
    | b >= 1 && c >= 8 && (Victory Province) `elem` s = (newls, Buy (Victory Province))
    | b >= 1 && c >= 5 && (Victory Duchy) `elem` s && e > 0.7 = (newls, Buy (Victory Duchy))
    | b >= 1 && c >= 2 && (Victory Estate) `elem` s && e > 0.9 = (newls, Buy (Victory Estate))
    | b >= 1 && c >= 6 && (Treasure Gold) `elem` s = (newls, Buy (Treasure Gold))
    | b >= 1 && c >= 5 && (Action Mine) `elem` s && percent_action < 0.30 = (newls, Buy (Action Mine))
    | b >= 1 && c >= 5 && (Action Market) `elem` s && percent_action < 0.40 = (newls, Buy (Action Market))
-- | b >= 1 && c >= 5 && (Victory Duchy) `elem` s = (newls, Buy (Victory Duchy))
    | b >= 1 && c >= 4 && (Action Militia) `elem` s && ( length $ filter (== (Action Militia)) all_cards ) < 3  = (newls, Buy (Action Militia))
    | b >= 1 && c >= 4 && (Action Smithy) `elem` s && percent_action < 0.25 = (newls, Buy (Action Smithy))
--    | b >= 1 && c >= 4 && (Action Remodel) `elem` s && percent_action < 0.18  = (newls, Buy (Action Remodel))
--    | b >= 1 && c >= 3 && (Action Workshop) `elem` s && percent_action < 0.10 && ( length $ filter (== (Action Workshop)) all_cards ) < 2 = (newls, Buy (Action Workshop))
    | b >= 1 && c >= 3 && (Action Village) `elem` s && percent_action < 0.20 && ( length $ filter (== (Action Village)) all_cards ) < 3 = (newls, Buy (Action Village))
   -- | b >= 1 && c >= 3 && (Action Woodcutter) `elem` s && percent_action < 0.10 && (length $ filter (== (Action Woodcutter)) all_cards) < 2 = (newls, Buy (Action Woodcutter))
    | b >= 1 && c >= 3 && (Treasure Silver) `elem` s = (newls, Buy (Treasure Silver))
    | b >= 1 && c >= 2 && (Action Moat) `elem` s && percent_action < (0.05 + mb) && nm = (newls, Buy (Action Moat))
    | b >= 1 && c >= 2 && (Action Cellar) `elem` s && percent_action < 0.20 && (length $ filter (==(Action Cellar)) all_cards) < 5 = (newls, Buy (Action Cellar))
-- | b >= 1 && c >= 2 && (Victory Estate) `elem` s = (newls, Buy (Victory Estate))
    | b >= 1 && c >= 0 && (Treasure Copper) `elem` s && percent_copper < 0.05 = (newls, Buy (Treasure Copper))
--Bummer
    | length h > 0 = (newls, (Cleanc (head h)))
    | otherwise = (newls, Clean)
    where all_cards = h ++ d ++ di ++p
          remodel_cards = doRuthlessRemodel ( delete (Action Remodel) h ) s all_cards ls
          workshop_cards = bestBuyable 4 s all_cards ls
          percent_action = (realToFrac $ length $ filter isAction all_cards) / (realToFrac $ length all_cards)
          percent_copper = (realToFrac $ length $ filter (==(Treasure Copper)) all_cards) / (realToFrac $ length all_cards)
          newls = ls{end = endCalc s}

endCalc :: [Card] -> Float
endCalc supply = 1 - ( (realToFrac $ length $ filter (== (Victory Province)) supply) / 12 )

main :: IO()
main = do 
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    interact playerProcess


foldl'' :: (b -> a -> (b,[c])) -> b -> [a] -> [c]
foldl'' f z [] = []
foldl'' f z (x:xs) = let rest = f z x in
                snd rest ++ (foldl'' f (fst rest) xs)

playerProcess :: String -> String
playerProcess input = foldl'' handleNotifs LocalState{needMoat=False,moatBump=0,end=0} $ mapMaybe ( parseNotif Nothing ) $ getMatchedBrackets (0,[]) input

getMatchedBrackets :: (Int, String) -> String -> [String]
--getMatchedBrackets (lvl, prev) (str:xs)  | trace ( "getMatchedBrackets: " ++ show lvl ++ ":" ++ prev ++ ":" ++ [str] ++ ":" ) False = undefined
getMatchedBrackets (0,[]) [] = []
getMatchedBrackets (0,b) [] = [b]
getMatchedBrackets (a,b) [] = []
getMatchedBrackets (a,b) ('(':xs) = getMatchedBrackets (a+1, b ++ ['(']) xs
getMatchedBrackets (0,[]) (x:xs) = getMatchedBrackets (0,[]) xs
getMatchedBrackets (0,b) (')':xs) = getMatchedBrackets (0, b) xs
getMatchedBrackets (1,b) (')':xs) = (b ++ [')']): getMatchedBrackets (0,[]) xs
getMatchedBrackets (a,b) (')':xs) = getMatchedBrackets (a-1, b ++ [')']) xs
getMatchedBrackets (a,b) (x:xs) = getMatchedBrackets (a, b ++ [x]) xs


