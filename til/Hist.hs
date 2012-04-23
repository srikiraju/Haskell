module Hist where

import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import System.FilePath.Posix
import Text.Printf
import Data.Maybe
import System.Directory
import Control.Monad.Error
import Crypto.Hash.SHA1
import Data.Time.Format
import Data.Time.Clock
import Data.List
import System.Process
import System.Exit

data History = History {
                heads :: [(String, String)] --BranchName/CommitHash
                } deriving (Show,Read);

data Stage = Stage {
                parent_commit :: Commit, --HEAD
                current_branch :: String, --branch-name + HEAD
                merge_mode :: Bool,
                adds :: [(Char,FilePath)]
                } deriving (Show,Read);

data Commit = Commit {
                commit_hash :: String,
                tree_hash :: String, --Points to the tree
                parents :: [String],      
                children :: [String],
                author :: String,
                date :: UTCTime,
                message :: String
                } | InitCommit {
                children :: [String]
                } deriving (Show, Read);

data Tree = Tree { 
                subtrees :: [(String,Tree,String)] --Name,Tree,Hash
            } | File
                deriving (Show, Read, Eq);

data TilError = TilNotFoundError |
                AddInvalidError String |
                CommitFailError String |
                MiscError String |
                BlobNotFoundError String deriving (Show);

instance Error TilError

isValidHash :: String -> Bool
isValidHash hash = (length hash == 40) --TODO: more?


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

--readup the history file
readHist :: MonadIO m => ErrorT TilError m History
readHist = do
            tilDir <- findTilDirectory
            hist <- return (tilDir ++ "/hist")
            hist_contents <- liftIO $ readFile hist
            return $ read hist_contents


writeHist :: MonadIO m => History -> ErrorT TilError m ()
writeHist histContents | trace ( "writeHist: " ++ show histContents ) False = undefined
writeHist histContents = do
            tilDir <- findTilDirectory
            hist <- return $ tilDir ++ "/hist"
            liftIO $ writeFile hist $ show histContents


readCommit :: MonadIO m => String -> ErrorT TilError m Commit
readCommit hash = do
            tilDir <- findTilDirectory
            subDir <- return $ tilDir ++ "/index/" ++ take 5 hash ++ "/"
            res <- liftIO $ doesFileExist (subDir ++ drop 5 hash) 
            if res
                then do commit_contents <- (liftIO $ readFile (subDir ++ drop 5 hash))
                        return $ read commit_contents
                else throwError $ BlobNotFoundError "readCommit: Can't find Blob"

writeCommit :: MonadIO m => String -> Commit -> ErrorT TilError m ()
writeCommit hash content = do
            tilDir <- findTilDirectory
            subDir <- return $ tilDir ++ "/index/" ++ take 5 hash ++ "/"
            liftIO $ createDirectoryIfMissing False (subDir)
            liftIO $ writeFile (subDir ++ drop 5 hash) $ show content


readTree :: MonadIO m => String -> ErrorT TilError m Tree
readTree "" = return $ Tree{subtrees=[]}
readTree hash = do
            tilDir <- findTilDirectory
            subDir <- return $ tilDir ++ "/index/" ++ take 5 hash ++ "/"
            res <- liftIO $ doesFileExist (subDir ++ drop 5 hash) 
            if res
                then do commit_contents <- (liftIO $ readFile (subDir ++ drop 5 hash))
                        return $ read commit_contents
                else throwError $ BlobNotFoundError "readTree: Can't find Blob"

writeTree :: MonadIO m => String -> Tree -> ErrorT TilError m ()
writeTree hash content = do
            tilDir <- findTilDirectory
            subDir <- return $ tilDir ++ "/index/" ++ take 5 hash ++ "/"
            liftIO $ createDirectoryIfMissing False (subDir)
            liftIO $ writeFile (subDir ++ drop 5 hash) $ show content


--recursively traverse upwards until we find a .til or throw up
findTilDirectory_ :: FilePath -> IO (Maybe FilePath)
--findTilDirectory_ dir | trace ( "findTilDirectory_: " ++ show dir ) False = undefined
findTilDirectory_ "" = return Nothing
findTilDirectory_ dir = do res <- doesDirectoryExist $ dir ++ "/.til"
                           if res then return $ Just $ dir ++ "/.til"
                           else findTilDirectory_ $ reverse $ dropWhile (\x -> x /= '/') $ drop 1 $ reverse dir

findTilDirectory :: MonadIO m =>  ErrorT TilError m FilePath
findTilDirectory = do
                    curDir <- liftIO $ getCurrentDirectory
                    maybeTil <- liftIO $ findTilDirectory_ curDir
                    maybe (throwError TilNotFoundError) return $ maybeTil

findBaseDirectory :: MonadIO m => ErrorT TilError m FilePath
findBaseDirectory = do
                    tilDir <- findTilDirectory
                    return $ reverse $ drop 5 $ reverse $ tilDir


--Checks a Tree if it contains a FilePath
containsFile :: Tree -> FilePath -> Bool
containsFile _ "" = False
containsFile File{} path = False
containsFile tree path = let folders = span (/= '/') path in
                           maybe False (\stree -> if ((drop 1 $ snd folders) == "") then True else containsFile (snd1 stree) (drop 1 $ snd folders)
                                ) (find (\x -> fst1 x == fst folders) (subtrees tree))

--get hash from Tree object, path relative to base
getBlobHash :: MonadIO m => Tree -> FilePath -> ErrorT TilError m String
getBlobHash _ "" = throwError $ BlobNotFoundError "getBlobHash: asking for blob of nothing"
getBlobHash File{} path = throwError $ BlobNotFoundError "getBlobHash: non existant file"
getBlobHash tree path = let folders = span (/= '/') path in
                           maybe (throwError $ BlobNotFoundError "getBlobHash: can't find file") 
                                 (\stree -> if ((drop 1 $ snd folders) == "") then return $ trd1 stree else getBlobHash (snd1 stree) (drop 1 $ snd folders))
                                 (find (\x -> fst1 x == fst folders) (subtrees tree))

getBlobHashFromFS :: MonadIO m => FilePath -> ErrorT TilError m String
getBlobHashFromFS fullPath = do
            hash <- liftIO $ readProcess "openssl" ["sha1", fullPath] []
            --liftIO $ putStrLn $ take 40 $ drop 1 $ dropWhile (/= ' ') hash
            return $ take 40 $ drop 1 $ dropWhile (/= ' ') hash 

--FilePath must always be relative to baseDirectory
writeFileIntoIndex :: MonadIO m => FilePath -> ErrorT TilError m String
writeFileIntoIndex file | trace ("writeFileIntoIndex: " ++ show file) False = undefined
writeFileIntoIndex file = do
            baseDir <- findBaseDirectory 
            fullPath <- return $ baseDir </> file
            hash <- getBlobHashFromFS fullPath
            dest_folder <- return $ take 5 hash
            liftIO $ createDirectoryIfMissing False (baseDir ++ "/.til/index/" ++ dest_folder)
            liftIO $ copyFile fullPath (baseDir ++ "/.til/index/" ++ dest_folder ++ "/" ++ (drop 5 hash))
            return hash

writeStagedFileIntoIndex :: MonadIO m => FilePath -> ErrorT TilError m String
writeStagedFileIntoIndex file | trace ("writeStagedFileIntoIndex: " ++ show file) False = undefined
writeStagedFileIntoIndex file = do
            tilDir <- findTilDirectory 
            fullPath <- return $ tilDir </> "stage_store" </> file
            hash <- getBlobHashFromFS fullPath
            dest_folder <- return $ take 5 hash
            liftIO $ createDirectoryIfMissing False (tilDir </> "index" </> dest_folder)
            liftIO $ copyFile fullPath (tilDir </> "index" </> dest_folder </> (drop 5 hash))
            return hash



hashGenCommit :: String -> [String] -> String -> String
hashGenCommit tree_hash parents message = concat $ map (printf "%02x") $ BS.unpack $ hash $ BS.pack $ tree_hash ++ (concat $ sort parents) ++ message

hashGen :: Tree -> String
hashGen File = error "Can't hashGen a File"
hashGen tree = concat $ map (printf "%02x") $ (BS.unpack $ hash $ BS.pack $ concat $ sort $  map trd1 (subtrees tree))

newTreeC_ :: String -> [FilePath] -> Tree
newTreeC_ base_hash x | trace ( "newTreeC_:" ++ show base_hash ++ "  " ++ show x ) False = undefined
newTreeC_ base_hash [] = File
newTreeC_ base_hash (x:xs) = let stree = newTreeC_ base_hash xs in
                                case stree of
                                    File -> Tree{ subtrees = [(x, stree, base_hash)] }
                                    Tree _ -> Tree{ subtrees = [(x, stree, hashGen stree)] }

updateTreeC_ :: Tree -> String -> [FilePath] -> Tree
updateTreeC_ tree blob_hash x | trace ( "updateTreeC_: " ++ show tree ++ ":" ++ show blob_hash ++ ":" ++ show x ) False = undefined
updateTreeC_ _ _ [] = File
updateTreeC_ tree blob_hash (x:xs) =
                    let res = find (\y -> x == fst1 y) (subtrees tree) in
                        if isJust res then
                            let new_tree = updateTreeC_ (snd1 $ fromJust res) blob_hash xs in
                                Tree { subtrees = (delete (fromJust res) (subtrees tree)) ++ [(x, new_tree, hashGen new_tree)] }
                        else
                            let new_tree = newTreeC_ blob_hash xs in
                                case new_tree of
                                    File -> Tree{ subtrees = (subtrees tree) ++ [(x, new_tree, blob_hash)] }
                                    Tree _ -> Tree{ subtrees = (subtrees tree) ++ [(x, new_tree, hashGen new_tree)] }


updateTreeU_ :: Tree -> String -> [FilePath] -> Tree
updateTreeU_ tree blob_hash x | trace ( "updateTreeU_: " ++ show tree ++ ":" ++ show blob_hash ++ ":" ++ show x ) False = undefined
updateTreeU_ _ _ [] = File
updateTreeU_ tree blob_hash (x:xs) =
                    let res = find (\y -> x == fst1 y) (subtrees tree) in
                        if isJust res then
                            let new_tree = updateTreeU_ (snd1 $ fromJust res) blob_hash xs in
                                case new_tree of
                                    File -> Tree { subtrees = (delete (fromJust res) (subtrees tree)) ++ [(x, new_tree, blob_hash)] }
                                    Tree _ -> Tree { subtrees = (delete (fromJust res) (subtrees tree)) ++ [(x, new_tree, hashGen new_tree)] }
                        else
                            error "updateTreeU_: can't find file in tree"


updateTreeD_ :: Tree -> String -> [FilePath] -> Tree
updateTreeD_ tree blob_hash x | trace ( "updateTreeD_: " ++ show tree ++ ":" ++ show blob_hash ++ ":" ++ show x ) False = undefined
updateTreeD_ _ _ [] = File
updateTreeD_ tree blob_hash (x:xs) =
                    let res = find (\y -> x == fst1 y) (subtrees tree) in
                        if isJust res then
                            let new_tree = updateTreeD_ (snd1 $ fromJust res) blob_hash xs in
                                case new_tree of
                                    File -> Tree { subtrees = (delete (fromJust res) (subtrees tree)) }
                                    Tree _ -> Tree { subtrees = (delete (fromJust res) (subtrees tree)) ++ [(x, new_tree, hashGen new_tree)] }
                        else
                            error "updateTreeD_: can't find file in tree"



fst1 (x, _, _) = x
snd1 (_, y, _) = y
trd1 (_, _, z) = z

updateTree :: MonadIO m => Tree -> [(Char, FilePath, String)] -> ErrorT TilError m Tree
updateTree tree x | trace ( "updateTree: " ++ show tree ++ show x ) False = undefined
updateTree tree [] = return tree
updateTree tree ((x,y,z):xs) = do
                case x of
                    'c' -> updateTree (updateTreeC_ tree z (splitDirectories y)) xs
                    'u' -> updateTree (updateTreeU_ tree z (splitDirectories y)) xs
                    'd' -> updateTree (updateTreeD_ tree z (splitDirectories y)) xs

setDirToTree_ :: FilePath -> FilePath -> (String, Tree, String) -> IO ()
setDirToTree_ tilDir dir (x,y,z) = do
                case y of
                    File -> copyFile ( tilDir </> "index" </> (take 5 z) </> (drop 5 z) ) (dir </> x)
                    Tree _ -> setDirToTree tilDir (dir </> x) y



clearDirToTree_ :: FilePath -> (String, Tree, String) -> IO ()
clearDirToTree_ dir (x,y,z) = do
                case y of
                    File -> removeFile (dir </> x)
                    Tree _ -> clearDirToTree (dir </> x) y >> (removeDirectory $ dir </> x)



setDirToTree :: FilePath -> FilePath -> Tree -> IO ()
setDirToTree tilDir dir tree = do 
        mapM_ ((setDirToTree_ tilDir dir)) (subtrees tree)


--clearDirToTree baseDir Tree
clearDirToTree :: FilePath -> Tree -> IO ()
clearDirToTree dir tree = do 
        mapM_ ((clearDirToTree_ dir)) (subtrees tree)

diffFiles fileName from to | trace( "diffFiles: " ++ fileName ++ " " ++ from ++ " " ++ to ) False = undefined
diffFiles fileName from to = do
                (x,y,z) <- readProcessWithExitCode "/usr/bin/diff" ["-N", from, to] ""
                case x of
                    ExitFailure 1 -> putStrLn fileName >> putStrLn y
                    otherwise -> return ()

diffIndexedFiles :: FilePath -> FilePath -> String -> String -> IO ()
diffIndexedFiles fileName tilDir x y = do
                filex <- if length x == 40 then 
                                return $ tilDir </> "index" </> take 5 x </> drop 5 x
                            else
                                return ""
                filey <- if length x == 40 then 
                                return $ tilDir </> "index" </> take 5 y </> drop 5 y
                            else
                                return ""
                diffFiles fileName filex filey

findHashInTree :: Tree -> String -> String
findHashInTree tree name = maybe "" (trd1) (find (\x -> fst1 x == name) (subtrees tree))

findTreeInTree :: Tree -> String -> Tree
findTreeInTree tree name = maybe Tree{subtrees=[]} snd1 (find (\x -> fst1 x == name) (subtrees tree))


diffIndexedTrees :: FilePath -> FilePath -> Tree -> Tree -> IO ()
diffIndexedTrees fileName tilDir from to = do
            --unchanged = intersectBy (\x y -> trd1 x == trd1 y) (subtrees from) (subtrees to)
            common <- return $ intersectBy (\x y -> fst1 x == fst1 y) (subtrees from) (subtrees to)
            --deleted_from = deleteFirstsBy (\x y -> fst1 x == fst1 y) from common
            added_to <- return $ deleteFirstsBy (\x y -> fst1 x == fst1 y) (subtrees to) common
            --updated = deleteFirstsBy (\x y -> fst1 x == fst1 y) common unchanged
            mapM_ (\x -> case snd1 x of
                Tree _ -> diffIndexedTrees (fileName </> fst1 x) tilDir (snd1 x) (findTreeInTree to $ fst1 x)
                File -> diffIndexedFiles (fileName </> fst1 x) tilDir (trd1 x) (findHashInTree to $ fst1 x)) (subtrees from)
            mapM_ (\x -> case snd1 x of
                Tree _ -> diffIndexedTrees (fileName </> fst1 x) tilDir Tree{subtrees=[]} (snd1 x)
                File -> diffIndexedFiles (fileName </> fst1 x) tilDir "" (trd1 x)) added_to


diffAgainstWorkingTree :: FilePath -> FilePath -> Tree -> IO ()
diffAgainstWorkingTree fileName baseDir from = do
            mapM_ (\x -> case snd1 x of
                Tree _ -> diffAgainstWorkingTree (fileName </> fst1 x) baseDir (snd1 x)
                File -> diffFiles (fileName </> fst1 x) (baseDir </> ".til" </> "index" </> (take 5 $ trd1 x) </> (drop 5 $ trd1 x)) (baseDir </> fileName </> fst1 x)) (subtrees from)

findCommonAncestor_ visited a b | trace( "findCommonAncestor_: " ++ show visited ++ " " ++ show a ++ " " ++ show b ) False = undefined
findCommonAncestor_ visited (InitCommit _) (InitCommit _) = return $ InitCommit{children=[]}
findCommonAncestor_ visited (InitCommit a) b = findCommonAncestor_ visited b (InitCommit a)
findCommonAncestor_ visited a b = do
            if commit_hash a `elem` visited then
                return a
            else do
                parents <- return $ parents a
                parent <- if length parents == 0 then do
                    return $ InitCommit{children=[]}
                else do
                    readCommit $ ( parents !! 0 )
                findCommonAncestor_ (visited ++ [commit_hash a]) b parent

findCommonAncestor a b = findCommonAncestor_ [] a b


mergeFiles__ :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO String
mergeFiles__ fileName tilDir a b c | trace ("mergeFiles__:" ++ show fileName ++ " " ++ show tilDir ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined 
mergeFiles__ fileName tilDir a b c = do 
                copyFile a (tilDir </> ".." </> fileName)
                (x,y,z) <- readProcessWithExitCode "merge" [(tilDir </> ".." </> fileName), b, c] ""
                case x of
                    ExitFailure 1 -> return fileName
                    otherwise -> return ""

mergeFiles_ :: MonadIO m => FilePath -> FilePath -> String -> String -> String -> ErrorT TilError m String
mergeFiles_ fileName tilDir from to common | trace ("mergeFiles_: " ++ show from ++ " " ++ show to ++ " " ++ show common) False = undefined
mergeFiles_ fileName tilDir "" to common = mergeFiles_ fileName tilDir (take 40 $ repeat '0') to common
mergeFiles_ fileName tilDir from "" common = mergeFiles_ fileName tilDir from (take 40 $ repeat '0') common
mergeFiles_ fileName tilDir from to "" = mergeFiles_ fileName tilDir from to (take 40 $ repeat '0')
mergeFiles_ fileName tilDir from to common = liftIO $ mergeFiles__ fileName tilDir (tilDir </> "index" </> take 5 from </> drop 5 from) (tilDir </> "index" </> take 5 to </> drop 5 to) (tilDir </> "index" </> take 5 common </> drop 5 common) 

--TODO: handle files becoming folders boohoo
mergeTrees :: MonadIO m => FilePath -> FilePath -> Tree -> Tree -> Tree -> ErrorT TilError m ([String], Tree)
mergeTrees fileName tilDir from to common | trace( "mergeTrees: " ++ show fileName ++ " " ++ show tilDir ++ " " ++ show from ++ " " ++ show to ++ " " ++ show common ) False = undefined
mergeTrees fileName tilDir from to common = do
            updated <- return $ intersectBy (\x y -> fst1 x == fst1 y) (subtrees from) (subtrees to)
            --deleted_from = deleteFirstsBy (\x y -> fst1 x == fst1 y) from common
            added_to <- return $ deleteFirstsBy (\x y -> fst1 x == fst1 y) (subtrees to) updated

            a <- mapM (\x -> (do
                tmp <- case snd1 x of
                    Tree _ -> mergeTrees (fileName </> fst1 x) tilDir (snd1 x) (findTreeInTree to $ fst1 x) (findTreeInTree common $ fst1 x)
                    File -> do
                        conf <- mergeFiles_ (fileName </> fst1 x) tilDir (trd1 x) (findHashInTree to $ fst1 x) (findHashInTree common $ fst1 x)
                        return $ ([conf], File)
                return $ (tmp, fst1 x)
                )) (subtrees from)

            b <- mapM (\x -> (do
                tmp <- case snd1 x of
                    Tree _ -> mergeTrees (fileName </> fst1 x) tilDir (snd1 x) (findTreeInTree to $ fst1 x) (findTreeInTree common $ fst1 x)
                    File -> do
                        conf <- mergeFiles_ (fileName </> fst1 x) tilDir (trd1 x) (findHashInTree to $ fst1 x) (findHashInTree common $ fst1 x)
                        return $ ([conf], File)
                return $ (tmp, fst1 x)
                )) added_to

            subtre <- mapM (\x -> do
                                hash <- case (snd $ fst x) of
                                            File -> getBlobHashFromFS $ tilDir </> ".." </> fileName </> snd x
                                            Tree _-> return $ hashGen $ snd $ fst x 
                                return $ (snd x, snd $ fst x, hash)
                            ) (a ++ b)

            conflicts <- return $ concat $ fst $ unzip $fst $ unzip $ a ++ b
            return ( filter (/= "") conflicts, Tree{subtrees = subtre} )
