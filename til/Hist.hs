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

data History = History {
                all :: [Commit],
                heads :: [(Commit, String)]
                } deriving (Show,Read);

data Stage = Stage {
                parent_commit :: Commit,
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

--from FS, relative to base directory
getBlobHashFromFS :: MonadIO m => FilePath -> ErrorT TilError m String
getBlobHashFromFS file = do
            baseDir <- findBaseDirectory
            fullPath <- return $ baseDir ++ "/" ++ file
            hash <- liftIO $ readProcess "openssl" ["sha1", fullPath] []
            liftIO $ putStrLn $ take 40 $ drop 1 $ dropWhile (/= ' ') hash
            return $ take 40 $ drop 1 $ dropWhile (/= ' ') hash 

--FilePath must always be relative to baseDirectory
writeFileIntoIndex :: MonadIO m => FilePath -> ErrorT TilError m String
writeFileIntoIndex file | trace ("writeFileIntoIndex: " ++ show file) False = undefined
writeFileIntoIndex file = do
            baseDir <- findBaseDirectory 
            fullPath <- return $ baseDir ++ "/" ++ file
            hash <- getBlobHashFromFS file
            dest_folder <- return $ take 5 hash
            liftIO $ createDirectoryIfMissing False (baseDir ++ "/.til/index/" ++ dest_folder)
            liftIO $ copyFile fullPath (baseDir ++ "/.til/index/" ++ dest_folder ++ "/" ++ (drop 5 hash))
            return hash


hashGenCommit :: String -> [String] -> String -> String
hashGenCommit tree_hash parents message = concat $ map (printf "%02x") $ BS.unpack $ hash $ BS.pack $ tree_hash ++ (concat $ sort parents) ++ message

hashGen :: Tree -> String
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
