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

dispatch :: [(String, [String] -> IO (Either TilError ()))]  
dispatch =  [ ("commit", commit)  
            , ("log", log_)
            , ("add", add)  
            , ("rm", rm)  
            , ("checkout", checkout)  
            , ("diff", diff)  
            , ("status", status)  
            , ("reset", reset)  --reset
            , ("branch", branch)
            , ("merge", merge)
            , ("init", init_)
            ]

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    --eithVal <- action args
    --putStrLn $ show eithVal
    (either (\x -> putStrLn $ "Error: " ++ show x) (\x -> return ())) =<< action args


--Remove from stage - resets stage to whatever is in parent commit
rm args = return $ Right ()

verifyAdds :: MonadIO m => Tree -> [FilePath] -> ErrorT TilError m [(Char,FilePath)]
verifyAdds parent_tree [] = do return []
verifyAdds parent_tree (x:xs) = do
                rest <- verifyAdds parent_tree xs
                baseDir <- findBaseDirectory 
                fullPath <- return $ baseDir </>  x
                res <- liftIO $ doesFileExist $ fullPath
                if containsFile parent_tree x then do --File is in parent tree, so make sure its modif
                    if res then do
                        hash <- getBlobHash parent_tree x
                        new_hash <- getBlobHashFromFS fullPath
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

logHelper (InitCommit _) _ = return ()
logHelper commit x = do
        liftIO $ putStrLn $ "commit " ++ commit_hash commit 
        if ((length $ parents commit) == 2) then
            liftIO $ putStrLn $ "merged " ++ parents commit !! 1 ++ " into " ++ (parents commit !! 0)
        else
            return ()
        liftIO $ putStrLn $ "Author: " ++ author commit 
        liftIO $ putStrLn $ "Date: " ++ (show $ date commit)
        liftIO $ putStrLn $ message commit 
        liftIO $ putStrLn ""
        if (((length $ parents commit) == 0) || x == 1) then
            return ()
        else do
            nextCommit <- readCommit (parents commit !! 0)
            logHelper nextCommit (x-1)

log_ args = do
    runErrorT $ do
        stage <- readStage
        commit <- if length args == 0 then do
            return $ parent_commit stage
        else do
            (readCommit (args !! 0))
        liftIO $ putStrLn $ "Branch " ++ current_branch stage
        logHelper commit 10
        
status args = do
    runErrorT $ do
        stage <- readStage
        liftIO $ putStrLn $ "In branch: " ++ current_branch stage
        if merge_mode stage == False then do
            if (length $ adds stage) > 0 then do
                liftIO $ putStrLn "These changes are staged: "
                liftIO $ putStrLn $ show $ adds stage
            else
                liftIO $ putStrLn "No changes are staged"
        else do
            md <- readMergeData
            liftIO $ putStrLn $ "Preparing to merge " ++ from md ++ " into " ++ current_branch stage
            if (length $ adds stage) > 0 then do
                liftIO $ putStrLn "Merge conflicts exist:"
                liftIO $ putStrLn $ show $ adds stage
            else
                liftIO $ putStrLn "Ready to merge"
        

--Add to stage - compares working tree against what's in parent commit
add args = do
    runErrorT $ do
        if length args == 0 then do
            throwError $ MiscError "Not enough arguments"
        else do
            tilDir <- findTilDirectory
            baseDir <- findBaseDirectory
            curDir <- liftIO $ getCurrentDirectory
            stage <- readStage
            canon <- liftIO $ mapM (\x -> do res <- doesFileExist x; if res then canonicalizePath x else return $ curDir ++ "/" ++ x ) args
            base_adds <- return $ map (makeRelative baseDir) canon
            if merge_mode stage == False then do
                commit <- return $ parent_commit stage
                tree <- case commit of
                    InitCommit _ -> return $ Tree {subtrees=[]}
                    Commit _ _ _ _ _ _ _ -> readTree $ tree_hash commit
                typed_adds <- verifyAdds tree base_adds
                old_adds <- return $ deleteFirstsBy (\x y -> snd x == snd y ) (adds stage) typed_adds
                stage <- return $ stage { adds = old_adds ++ typed_adds }
                liftIO $ putStrLn $ show typed_adds
                liftIO $ mapM_ (\(t,a) ->
                            if t /= 'd' then do
                                createDirectoryIfMissing True (dropFileName $ tilDir </> "stage_store" </> a)
                                (copyFile (baseDir </> a) (tilDir </> "stage_store" </> a))
                            else
                                return ()
                            ) typed_adds
                writeStage stage
            else do
                --TODO: more checks, deleted files
                mapM_ (\x -> do 
                        res <- liftIO $ doesFileExist x
                        if res then (return ()) else (throwError $ MiscError $ "Invalid file:" ++ x)
                      ) base_adds
                liftIO $ mapM_ (\a -> do createDirectoryIfMissing True (dropFileName $ tilDir </> "stage_store" </> a) >> (copyFile (baseDir </> a) (tilDir </> "stage_store" </> a))) base_adds
                writeStage $ stage{ adds = (adds stage \\ zip (take (length base_adds) $ repeat 'm') base_adds) }
                 

init_ args = runErrorT $ do
        curDir <- liftIO $ getCurrentDirectory
        --TODO: check if dir already exists
        liftIO $ createDirectory $ curDir ++ "/.til"
        liftIO $ createDirectory $ curDir ++ "/.til/index/"
        liftIO $ createDirectory $ curDir ++ "/.til/stage_store/"
        liftIO $ writeFile (curDir ++ "/.til/stage") $ show Stage{ parent_commit = InitCommit{children=[]}, adds=[], current_branch = "master", merge_mode = False } 
        liftIO $ writeFile (curDir ++ "/.til/hist") $ show History{ heads = [("master","")] } 
        liftIO $ putStrLn $ "Initialized in " ++ curDir </> ".til"


commit args = runErrorT $ do
        stage <- readStage
        tilDir <- findTilDirectory
        if merge_mode stage then do
            if (length $ adds stage) > 0 then do
                liftIO $ putStrLn "Conflicts still exist, do you want to continue? y/n"
                resp <- liftIO $ getLine
                if resp == "y" then do
                    return ()
                else
                    throwError $ MiscError "User stopped"
            else
                return ()
            md <- readMergeData
            new_tree <- writeStagedTreeIntoIndex "" (mtree md)  
            tree_hash <- return $ hashGen new_tree
            liftIO $ putStrLn "Enter commit message: "
            commitMsg <- liftIO $ getLine
            time <- liftIO $ getCurrentTime
            commit <- return $ Commit{ commit_hash = hashGenCommit (hashGen new_tree) (mparents md) commitMsg,
                                    tree_hash = hashGen new_tree,
                                    parents = mparents md,
                                    children = [],
                                    author = "Srikanth Raju",
                                    date = time,
                                    message = commitMsg }
            liftIO $ putStrLn $ show commit
            writeCommit (commit_hash commit) commit
            writeTree (hashGen new_tree) new_tree
            writeStage Stage{ parent_commit = commit, adds=[], current_branch = current_branch stage, merge_mode = False }
            hist <- readHist
            new_heads <- return $ (deleteBy (\x y -> fst x == fst y) (current_branch stage,"") (heads hist))
            writeHist $ hist{ heads =  new_heads ++ [(current_branch stage, commit_hash commit)] }
            liftIO $ removeDirectoryRecursive $ tilDir </> "stage_store"
            liftIO $ createDirectory $ tilDir </> "stage_store"
            liftIO $ removeFile $ tilDir </> "mdata" 
        else
            if (length $ adds stage) > 0 then do
                --Everything on stage is changed, so we only need to update those blobs
                new_hashes <- mapM (\x -> if fst x /= 'd' then (do hash <- (writeStagedFileIntoIndex $ snd x); return (fst x, snd x, hash)) else (return ('d', snd x, "")) ) (adds stage)
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
                                    author = "Srikanth Raju",
                                    date = time,
                                    message = commitMsg }
                liftIO $ putStrLn $ show commit
                writeCommit (commit_hash commit) commit
                writeTree (hashGen new_tree) new_tree
                writeStage Stage{ parent_commit = commit, adds=[], current_branch = current_branch stage, merge_mode = False }
                hist <- readHist
                writeHist $ hist{ heads = (deleteBy (\x y -> fst x == fst y) (current_branch stage,"") (heads hist)) ++ [(current_branch stage, commit_hash commit)] }
                liftIO $ removeDirectoryRecursive $ tilDir </> "stage_store"
                liftIO $ createDirectory $ tilDir </> "stage_store"
                return ()
            else
                throwError $ CommitFailError "Nothing in stage to commit. Use til add."

--checkout branchname -> move to HEAD on that branch/HEAD
checkout args = do
    runErrorT $ do
        baseDir <- findBaseDirectory
        tilDir <- findTilDirectory
        
        stage <- readStage
        hist <- readHist
        liftIO $ putStrLn $ show hist
        other_commit_hash <- maybe (throwError $ MiscError "No such branch") (return . snd) $ find (\x -> fst x == (args !! 0 )) (heads hist)
        other_commit <- readCommit other_commit_hash
    
        parentTree <- readTree $ tree_hash $ parent_commit stage 
        liftIO $ clearDirToTree baseDir parentTree

        baseTree <- readTree $ tree_hash other_commit
        liftIO $ setDirToTree tilDir baseDir baseTree 
        writeStage Stage{ parent_commit = other_commit, adds=[], current_branch = (args !! 0), merge_mode = False}


    
--reset commitid
--reset --soft -> Only changes stage pointer and cleans stage
--reset --hard -> reset tree and stage, make commit the HEAD on branch. you will lose data.
--reset -> resets tree and stage, but doesnt make this the HEAD commit
reset args = do
    runErrorT $ do
        baseDir <- findBaseDirectory
        tilDir <- findTilDirectory
        stage <- readStage
        if length args == 0 then
            liftIO $ putStrLn "Not enough args"
        else if length args > 1 then
            case (args !! 0) of
                "--soft" -> do 
                        commit <- readCommit (args !! 1)
                        writeStage Stage{ parent_commit = commit, adds=[], current_branch = current_branch stage, merge_mode = False}
                "--hard" -> do
                        parentTree <- readTree $ tree_hash $ parent_commit stage 
                        liftIO $ clearDirToTree baseDir parentTree
                        commit <- readCommit (args !! 1)
                        baseTree <- readTree $ tree_hash commit
                        liftIO $ setDirToTree tilDir baseDir baseTree 
                        writeStage Stage{ parent_commit = commit, adds=[], current_branch = current_branch stage, merge_mode = False}
                        hist <- readHist
                        writeHist $ hist{ heads = (deleteBy (\x y -> fst x == fst y) (current_branch stage,"") (heads hist)) ++ [(current_branch stage, commit_hash commit)] }
        else do
            parentTree <- readTree $ tree_hash $ parent_commit stage 
            commit <- readCommit (args !! 0)
            liftIO $ clearDirToTree baseDir parentTree
            baseTree <- readTree $ tree_hash commit
            liftIO $ setDirToTree tilDir baseDir baseTree 
            writeStage Stage{ parent_commit = commit, adds=[], current_branch = current_branch stage, merge_mode = False}

diff args = do
    runErrorT $ do
        if length args == 0 then do
            baseDir <- findBaseDirectory
            stage <- readStage
            base_tree <- readTree $ tree_hash $ parent_commit stage
            liftIO $ diffAgainstWorkingTree "" baseDir base_tree 
        else if length args == 2 then do
            tilDir <- findTilDirectory
            from_commit <- readCommit (args !! 0)
            from_tree <- readTree $ tree_hash from_commit
            to_commit <- readCommit (args !! 1)
            to_tree <- readTree $ tree_hash to_commit
            liftIO $ diffIndexedTrees "" tilDir from_tree to_tree
        else if length args == 1 then do
            tilDir <- findTilDirectory
            from_commit <- readCommit (args !! 0)
            from_tree <- readTree $ tree_hash from_commit
            to_commit <- readCommit $ ((parents from_commit) !! 0)
            to_tree <- readTree $ tree_hash to_commit
            liftIO $ diffIndexedTrees "" tilDir from_tree to_tree
        else
            liftIO $ putStrLn "Incorrect format"



--branch name -> create new branch
--branch -> list branches
branch args = do
    runErrorT $ do
        hist <- readHist 
        if length args == 1 then do
            stage <- readStage 
            writeHist $ hist{ heads = (heads hist) ++ [(args !! 0, commit_hash $ parent_commit stage)] }
            writeStage $ stage{ current_branch = (args !! 0) }
        else
            liftIO $ mapM_ (\(x,y) -> (putStrLn $ x ++ ":" ++ y)) (heads hist)



mergeStageHelper :: FilePath -> FilePath -> Tree -> IO ()
mergeStageHelper baseDir path File = copyFile (baseDir </> path) (baseDir </> ".til/stage_store/" </> path)
mergeStageHelper baseDir path tree = do
        createDirectoryIfMissing True (baseDir </> path)
        mapM_ (\x -> mergeStageHelper baseDir (path </> fst1 x) (snd1 x)) (subtrees tree)


--merge branch1 -> merge branch1 onto curbranch, set cur branch to branch1
merge args = do
    runErrorT $ do
        stage <- readStage
        baseDir <- findBaseDirectory
        tilDir <- findTilDirectory
        if (length (adds stage)) > 0 then do --Ensure empty stage
            throwError $ MiscError "You will need to have nothing on stage"
        else do
            hist <- readHist
            this_tree <- readTree $ tree_hash $ parent_commit stage
            other_commit_hash <- maybe (throwError $ MiscError "No such branch") (return . snd) $ find (\x -> fst x == args !! 0 ) (heads hist)
            other_commit <- readCommit other_commit_hash
            other_tree <- readTree $ tree_hash $ other_commit
            common_commit <- findCommonAncestor (parent_commit stage) (other_commit)
            
            if ((commit_hash common_commit) == (commit_hash $ parent_commit stage)) then do
                --FAST FORWARD! JOY    
                liftIO $ clearDirToTree baseDir this_tree
                liftIO $ setDirToTree tilDir baseDir other_tree
                hist <- readHist
                writeHist $ hist{ heads = (deleteBy (\x y -> fst x == fst y) (current_branch stage,"") (heads hist)) ++ [(current_branch stage, commit_hash common_commit)] }
                writeStage Stage{ parent_commit = other_commit, adds=[], current_branch = current_branch stage, merge_mode = False}
                liftIO $ putStrLn $ "Fast forwarded to " ++ commit_hash other_commit
            else do
                common_tree <- readTree $ tree_hash common_commit 
                (conflicts, merged_tree) <- mergeTrees "" (baseDir </> ".til") this_tree other_tree common_tree 
                if length conflicts > 0 then do
                    liftIO $ putStrLn "Conflicts:"
                    liftIO $ mapM_ (\x -> putStrLn x) conflicts
                    liftIO $ putStrLn "Use add and then commit as usual"

                    parents1 <- return $[commit_hash $ parent_commit stage, other_commit_hash]
                    writeMergeData $ MergeData{ mtree = merged_tree, mparents = parents1, from = args !! 0 }
                    writeStage $ stage{ merge_mode = True, adds = zip (take  (length conflicts) $ repeat 'm') conflicts } 
                    liftIO $ removeDirectoryRecursive $ baseDir </> ".til" </> "stage_store"
                    liftIO $ createDirectory $ baseDir </> ".til" </> "stage_store"
                    liftIO $ mergeStageHelper baseDir "" merged_tree
                    
                else do
                    liftIO $ putStrLn $ "New tree:" ++ show merged_tree
                    tree_hash <- return $ hashGen merged_tree
                    liftIO $ putStrLn "Enter commit message: "
                    commitMsg <- liftIO $ getLine
                    time <- liftIO $ getCurrentTime
                    parents1 <- return $[commit_hash $ parent_commit stage, other_commit_hash]
                    commit <- return $ Commit{ commit_hash = hashGenCommit (hashGen merged_tree) parents1 commitMsg,
                                        tree_hash = hashGen merged_tree,
                                        parents = parents1,
                                        children = [],
                                        author = "Srikanth Raju",
                                        date = time,
                                        message = commitMsg }
                    liftIO $ putStrLn $ show commit
                    writeCommit (commit_hash commit) commit
                    writeTree (hashGen merged_tree) merged_tree
                    writeStage Stage{ parent_commit = commit, adds=[], current_branch = current_branch stage, merge_mode = False}
                    hist <- readHist
                    new_heads <- return $ (deleteBy (\x y -> fst x == fst y) (current_branch stage,"") (heads hist))
                    writeHist $ hist{ heads =  new_heads ++ [(current_branch stage, commit_hash commit)] }
                    liftIO $ removeDirectoryRecursive $ baseDir </> ".til" </> "stage_store"
                    liftIO $ createDirectory $ baseDir </> ".til" </> "stage_store"
                    return ()
         
