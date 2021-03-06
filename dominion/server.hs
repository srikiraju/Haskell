 
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs



--Server portion
--
--

--Initial supply based on number of players
initSupply :: Int -> [Card]
initSupply numPlayers = take (60 - 7 * numPlayers) (repeat (Treasure Copper))
                      ++ take 40 (repeat (Treasure Silver)) ++ take 30 (repeat (Treasure Gold))
                      ++ take (24 - 3 * numPlayers) (repeat (Victory Estate)) 
                      ++ take 12 (repeat (Victory Duchy))
                      ++ take 12 (repeat (Victory Province))
                      ++ take 10 (repeat (Action Mine))

--Per player
initDeck :: [Card]
initDeck = take 3 (repeat (Victory Estate)) ++ take 7 (repeat (Treasure Copper))


initGlobalState :: [String] -> GlobalState
initGlobalState a | trace ( "initGlobalState: Players " ++ show a ) False = undefined
initGlobalState players = let startdeckss = unsafePerformIO (mapM shuffle (take (length players) (repeat initDeck)) ) in
                         GlobalState { gplayers=players, gsupply = initSupply (length players), gtrash=[], playsinprogress =[], playerdiscards=take (length players) (repeat []), playerhands = map ( take 5 ) startdeckss, playerdecks = map ( drop 5 ) startdeckss, phase = PhaseAction }


--Gold val
cardValue :: Card -> Int
cardValue (Treasure Copper) = 1
cardValue (Treasure Silver) = 2
cardValue (Treasure Gold) = 3
cardValue _ = 0

--Count value of set of cards
coinCount :: [Card] -> Int
coinCount x = sum (map cardValue x)

--Pulls in n cards from deck + discard pile into current hand
pickFromDeck :: GlobalState -> Int -> GlobalState
pickFromDeck global n = error "oops"

--Get state for current player
getPlayerState :: GlobalState -> State
getPlayerState global = State{ players = gplayers global,
                            supply = gsupply global,
                            trash = gtrash global,
                            actions = 1, buys = 1,
                            coins = coinCount( head (playerhands global) ),
                            deck = head (playerdecks global),
                            hand = head (playerhands global),
                            plays = [],
                            discards = head (playerdiscards global) }
 
leftShift :: [a] -> [a]
leftShift [] = []
leftShift x = (tail x) ++ [head x]

isDuchy :: Card -> Bool
isDuchy (Victory Duchy) = True
isDuchy _ = False

isProvince :: Card -> Bool
isProvince (Victory Province) = True
isProvince _ = False

isEstate :: Card -> Bool
isEstate (Victory Estate) = True
isEstate _ = False

isMine :: Card -> Bool
isMine (Action Mine) = True
isMine _ = False

prettyCards :: [Card] -> String
prettyCards x = "Gold: " ++ show (coinCount x) ++ ", Mines: " ++ show (length( filter isMine x ))
    ++ "\nEstates: " ++ show( length( filter isEstate x ) )
    ++ "\nDuchy: " ++ show( length( filter isDuchy x ) )
    ++ "\nProvince: " ++ show( length( filter isProvince x ) )

prettyGlobalState :: GlobalState -> String
prettyGlobalState global = "GlobalState: Current player = " ++ head (gplayers global) ++
    "\nSupply: " ++ prettyCards (gsupply global) ++ "\nTrash: " ++ prettyCards( gtrash global ) ++
    "\nGlobal Decks: " ++ show (playerdecks global) ++ "\nGlobal Hands: " ++
    show( playerhands global ) ++ "\nGlobal Discards: " ++ show( playerdiscards global ) ;

prettyState :: State -> String
prettyState global = "Local State: Current player = " ++ head (players global) ++
    "\nSupply: " ++ prettyCards (supply global) ++ "\nTrash: " ++ prettyCards( trash global ) ++
    "\nPlayer Deck: " ++ show (deck global) ++ "\nPlayer Hand: " ++
    show( hand global ) ++ "\nPlayer Discard: " ++ show( discards global ) ++
    "\nActions: " ++ show (actions global) ++ " Buys: " ++ show (buys global) ++
    " Coins: " ++ show( coins global )


--TODO: succ x = y constraint too strong
handlePlay :: GlobalState -> State -> Play -> GlobalState
handlePlay a b c | trace ( "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nhandlePlay: Global: " ++ prettyGlobalState a ++ "\nState: " ++ prettyState b ++ "\nPlay: " ++ show c ) False = undefined
handlePlay global@GlobalState{playerhands = h:hs, gsupply = s, phase = ph} ps (ActMine x y) 
    | succ x == y  && Treasure x `elem` h
      && Treasure y `elem` s && actions ps >= 1
      && ph == PhaseAction =
            let new_global = GlobalState{ gplayers = gplayers global,
                              gsupply = delete (Treasure y) s, 
                              gtrash = gtrash global ++ [(Treasure x)],
                              playerdecks = playerdecks global,
                              playerhands = ( delete (Action Mine) ((delete (Treasure x) h)) ++ [(Treasure y)]):hs,
                              playsinprogress = [(Action Mine)] ++ playsinprogress global,
                              playerdiscards = playerdiscards global,
                              phase = PhaseAction }
                new_state = State{ players = players ps,
                               supply = gsupply new_global,
                               trash = gtrash new_global,
                               actions = actions ps - 1,
                               buys = buys ps,
                               coins = coinCount( head (playerhands new_global) ),
                               deck = head (playerdecks new_global),
                               hand = head (playerhands new_global),
                               plays = playsinprogress new_global,
                               discards = head (playerdiscards new_global) } in
            handlePlay new_global new_state (move new_state)
    | otherwise = error "Problem at Act Mine"

handlePlay _ _ (Add x) = error "You already know what you have!"

handlePlay global@GlobalState{phase=ph, gsupply=s} ps@State{coins=x,buys=y} (Buy z)
    | y >= 1 &&  z `elem` s && cardCost z <= x && ph <= PhaseBuy =
            let new_global = GlobalState{ gplayers = gplayers global,
                                  gsupply = delete z s,
                                  gtrash = gtrash global,
                                  playerdecks = playerdecks global,
                                  playerhands = playerhands global,
                                  playsinprogress = playsinprogress global,
                                  playerdiscards = [( head (playerdiscards global) ++ [z] )] ++ tail( playerdiscards global),
                                  phase = PhaseBuy }
                new_state = State{ players = players ps,
                                   supply = gsupply new_global,
                                   trash = gtrash new_global,
                                   actions = 0,
                                   buys = buys ps - 1,
                                   coins = x - cardCost z,
                                   deck = head (playerdecks new_global),
                                   hand = head (playerhands new_global),
                                   plays = playsinprogress new_global,
                                   discards = head (playerdiscards new_global) } in
            handlePlay new_global new_state (move new_state)
    | otherwise = error "Problem at Buy card"

handlePlay _ _ (Clean) = error "Unimplemented"

--TODO: Check if top card in hand
--TODO: Stop killing kittens
handlePlay global@GlobalState{playerdecks=d:ds} ps@State{hand=h} (Cleanc x) = let dis = (h ++ head (playerdiscards global)) in
                if 5 <= length d then
                          let newhand = take 5 d
                              newdeck = drop 5 d in
                        GlobalState{  gplayers = leftShift( gplayers global ),
                                  gsupply = gsupply global,
                                  gtrash = gtrash global,
                                  playerdecks = ds ++ [newdeck],
                                  playerhands = (tail (playerhands global)) ++ [newhand],
                                  playsinprogress = [],
                                  playerdiscards = tail( playerdiscards global) ++ [dis],
                                  phase = PhaseAction }
                else 
                          let newdeck_f = d ++ unsafePerformIO( shuffle( dis ) )
                              newdeck = drop 5 newdeck_f
                              newhand = take 5 newdeck_f in
                        GlobalState{  gplayers = leftShift( gplayers global ),
                                  gsupply = gsupply global,
                                  gtrash = gtrash global,
                                  playerdecks = ds ++ [newdeck],
                                  playerhands = (tail (playerhands global)) ++ [newhand],
                                  playsinprogress = [],
                                  playerdiscards = tail( playerdiscards global) ++ [[]],
                                  phase = PhaseAction }


--TODO: need to send to different players on different processes
letPlay :: GlobalState -> String -> GlobalState
letPlay a b | trace ( "=============================================\nletPlay: Global: " ++ prettyGlobalState a ++ "\nPlayer: " ++ show b ) False = undefined
letPlay global@GlobalState{} player = handlePlay global (getPlayerState global) (move (getPlayerState global))


foldl'' :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldl'' test f z []     = z
foldl'' test f z (x:xs) = if test z then z else foldl'' test f (f z x) xs

--TODO: Full victory condition
gameOver :: GlobalState -> Bool
gameOver global = if length( filter isEstate (gsupply global) ) == 0 then True else False

runServer :: [String] -> GlobalState
runServer a | trace ( "runServ: Players: " ++ show a ) False = undefined
runServer players = foldl'' gameOver letPlay (initGlobalState players) (cycle players)

{-|
main :: IO()
main = let final = runServer( ["a", "b", "c", "d"] ) in
    do
        putStrLn( "============================" )
        putStrLn( prettyGlobalState ( final ) )
        let cards = zipWith3 (\x y z -> x ++ y ++ z) (playerdecks final) (playerhands final) (playerdiscards final) in
            mapM_ putStrLn (zipWith (\x y -> "Player: " ++ x ++ "\n" ++ y) (gplayers final) (map prettyCards cards) )
-}

