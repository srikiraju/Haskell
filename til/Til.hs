--Till Irrevocably Lost
import Hist
import System.FilePath.Posix
import System.Directory
import Control.Monad.Error
import Data.Maybe
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock
import Data.List
import System.Locale
import System.Environment
import Debug.Trace

--readup the Stage file
readStage :: MonadIO m => ErrorT TilError m Stage
readStage = do
                tilDir <- findTilDirectory
                stage <- return (tilDir ++ "/stage")
                stage_contents <- liftIO $ readFile stage
                return $ read stage_contents


writeStage :: MonadIO m => Stage -> ErrorT TilError m ()
writeStage stageContents | trace ( "writeStage: " ++ show stageContents ) False = undefined
writeStage stageContents = do
            tilDir <- findTilDirectory
            stage <- return $ tilDir ++ "/stage"
            liftIO $ writeFile stage $ show stageContents


dispatch :: [(String, [String] -> IO (Either TilError ()))]  
dispatch =  [ ("commit", commit)  
            , ("checkout", checkout)  
            , ("log", log_)
            , ("add", add)  
            , ("rm", rm)  
            , ("status", status)  
            , ("goto", goto)  --reset
            , ("branch", branch)
            , ("init", init_)
            ]

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    --eithVal <- action args
    --putStrLn $ show eithVal
    (either (\x -> putStrLn $ "Error: " ++ show x) (\x -> putStrLn "Done")) =<< action args


checkout args = return $ Right ()
log_ args = return $ Right ()
status args = return $ Right  ()
goto args = return $ Right ()
branch args = return $ Right ()

--Remove from stage - resets stage to whatever is in parent commit
rm args = return $ Right ()

verifyAdds :: MonadIO m => Tree -> [FilePath] -> ErrorT TilError m [(Char,FilePath)]
verifyAdds parent_tree [] = do return []
verifyAdds parent_tree (x:xs) = do
                rest <- verifyAdds parent_tree xs
                baseDir <- findBaseDirectory 
                fullPath <- return $ baseDir ++ "/" ++ x
                res <- liftIO $ doesFileExist $ fullPath
                if containsFile parent_tree x then do --File is in parent tree, so make sure its modif
                    if res then do
                        hash <- getBlobHash parent_tree x
                        new_hash <- getBlobHashFromFS x
                        if hash == new_hash then
                            throwError $ AddInvalidError $ "File hasn't changed: " ++ x
                        else
                            return (('u',x) : rest)
                    else
                        return (('d',x) : rest)
                else do --Is this a new valid file?
                    if res
                        then return (('c',x) : rest)
                    else
                        throwError $ AddInvalidError $ "File does not exist: " ++ x



--Add to stage - compares working tree against what's in parent commit
add args = do
    runErrorT $ do
        tilDir <- findTilDirectory
        stage <- readStage
        commit <- return $ parent_commit stage
        tree <- case commit of
            InitCommit _ -> return $ Tree {subtrees=[]}
            Commit _ _ _ _ _ _ _ -> readTree $ tree_hash commit
        curDir <- liftIO $ getCurrentDirectory
        canon <- liftIO $ mapM (\x -> do res <- doesFileExist x; if res then canonicalizePath x else return $ curDir ++ "/" ++ x ) args
        baseDir <- findBaseDirectory
        base_adds <- return $ map (makeRelative baseDir) canon
        typed_adds <- verifyAdds tree base_adds
        old_adds <- return $ deleteFirstsBy (\x y -> snd x == snd y ) (adds stage) typed_adds
        stage <- return $ stage { adds = old_adds ++ typed_adds }
        --TODO: Save files into .til/stage/...
        writeStage stage


init_ args = runErrorT $ do
        curDir <- liftIO $ getCurrentDirectory
        --TODO: check if dir already exists
        liftIO $ createDirectory $ curDir ++ "/.til"
        liftIO $ createDirectory $ curDir ++ "/.til/index/"
        liftIO $ writeFile (curDir ++ "/.til/stage") $ show Stage{ parent_commit = InitCommit{children=[]}, adds=[] } 


commit args = runErrorT $ do
        stage <- readStage
        if (length $ adds stage) > 0
        then do
            --Everything on stage is changed, so we only need to update those blobs
            new_hashes <- mapM (\x -> if fst x /= 'd' then (do hash <- (writeFileIntoIndex $ snd x); return (fst x, snd x, hash)) else (return ('d', snd x, "")) ) (adds stage)
            parent_tree <- case parent_commit stage of
                InitCommit _ -> return $ Tree {subtrees=[]}
                Commit _ _ _ _ _ _ _ -> readTree $ tree_hash $ parent_commit stage
            new_tree <- updateTree parent_tree new_hashes
            liftIO $ putStrLn $ "New tree:" ++ show new_tree
            tree_hash <- return $ hashGen new_tree
            liftIO $ putStrLn "Enter commit message: "
            commitMsg <- liftIO $ getLine
            time <- liftIO $ getCurrentTime
            parents1 <- return $ case parent_commit stage of
                                    InitCommit _ -> []
                                    otherwise -> [commit_hash $ parent_commit stage]

            commit <- return $ Commit{ commit_hash = hashGenCommit (hashGen new_tree) parents1 commitMsg,
                                tree_hash = hashGen new_tree,
                                parents = parents1,
                                children = [],
                                author = "Test",
                                date = time,
                                message = commitMsg }
            liftIO $ putStrLn $ show commit
            writeCommit (commit_hash commit) commit
            writeTree (hashGen new_tree) new_tree
            writeStage Stage{ parent_commit = commit, adds=[] }
            return ()
        else
            throwError $ CommitFailError "Nothing in stage to commit. Use til add."

--Check stage if something is actually there
--Read commit message 
--Actually commit
