{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Maybe
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock
import System.Locale
import Debug.Hood.Observe
import Debug.Trace
import Data.Char
import Control.Concurrent
import Network.BSD
import Network.Socket
import Network.TLS
import Network.TLS.Extra
import System.IO
import System.IO.Unsafe
import qualified Crypto.Random.AESCtr as RNG
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Error
import System.Environment
import Data.ConfigFile
import Data.Either.Utils
import Text.ParserCombinators.Parsec as PAR
import Prelude hiding (catch)

import Data.IORef

validateCert = True
debug = True

data MailAll = MailAll {
    seen :: Bool,
    date :: LocalTime,
    subject :: String,
    from :: [String],
    to :: [String],
    cc :: [String],
    fetchid :: Int } deriving (Show);


ciphers :: [Cipher]
ciphers =
    [ cipher_AES128_SHA1
    , cipher_AES256_SHA1
    , cipher_RC4_128_MD5
    , cipher_RC4_128_SHA1
    ]

runTLS params hostname portNumber f = do
    rng  <- RNG.makeSystem
    he   <- getHostByName hostname
    sock <- socket AF_INET Stream defaultProtocol
    let sockaddr = SockAddrInet portNumber (head $ hostAddresses he)
    catch (connect sock sockaddr)
          (\(e :: SomeException) -> sClose sock >> error ("cannot open socket " ++ show sockaddr ++ " " ++ show e))
    dsth <- socketToHandle sock ReadWriteMode
    ctx <- client params rng dsth
    f ctx
    hClose dsth

getDefaultParams sStorage session = defaultParams
    { pConnectVersion    = TLS10
    , pAllowedVersions   = [TLS10,TLS11,TLS12]
    , pCiphers           = ciphers
    , pCertificates      = []
    , pLogging           = logging
    , onCertificatesRecv = crecv
    , onSessionEstablished = \s d -> writeIORef sStorage (s,d)
    , sessionResumeWith  = session
    }
    where
        logging = if not debug then defaultLogging else defaultLogging
            { loggingPacketSent = putStrLn . ("debug: >> " ++)
            , loggingPacketRecv = putStrLn . ("debug: << " ++)
            }
        crecv = if validateCert then certificateVerifyChain else (\_ -> return CertificateUsageAccept)

getFromCfg acc field = runErrorT $ do
    cp <- join $ liftIO $ readfile emptyCP "imap.cfg"
    get cp acc field


dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("remove", remove)  
            , ("view", view)  
            , ("search", search)  
            ]

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args

--add accountname hostname port username password 
add args = do 
        runErrorT $ do
            cp <- join $ liftIO $ readfile emptyCP "imap.cfg"
            cp <- add_section cp $ args !! 0
            cp <- set cp (args !! 0) "hostname" (args !! 1)
            cp <- set cp (args !! 0) "port" (args !! 2)
            cp <- set cp (args !! 0) "username" (args !! 3)
            cp <- set cp (args !! 0) "password" (args !! 4)
            liftIO $ writeFile "imap.cfg" $ to_string $ cp
        return ()

--remove accountname
remove args = do
            runErrorT $ do
                cp <- join $ liftIO $ readfile emptyCP "imap.cfg"
                cp <- remove_section cp $ args !! 0
                liftIO $ writeFile "imap.cfg" $ to_string $ cp
            return ()
 
--search string accountname
--search string
search args = return ()

parseManyAlls :: CharParser () [MailAll]
parseManyAlls = do
            result <- many (parseAll <?> "parseAll failed ya") 
            manyTill (anyChar) (PAR.try $ string "OK FETCH")
            return result

yo = (lookAhead parseAll >> return ()) <|> (PAR.try eol >> return ()) <|> (manyTill anyChar (PAR.try eol) >> yo) <?> "yo failed"

parseAll :: CharParser () MailAll
parseAll = do
        char '*'
        spaces <?> "fail1"
        fetchid <- number
        spaces
        string "FETCH (FLAGS "
        seen <- flagsIsSeen
        manyTill (anyChar) (PAR.try $ string "ENVELOPE")
        spaces
        char '('
        date <- quotedStr
        date_time <- return $ fromMaybe (utcToLocalTime (unsafePerformIO getCurrentTimeZone) $ unsafePerformIO getCurrentTime) $ parseTime defaultTimeLocale "%a, %e %b %Y %T %z %Z" date
        spaces
        subject <- quotedStr
        spaces
        from <- addressStrings
        spaces
        addressStrings --Sender
        spaces
        addressStrings --reply-to
        spaces
        to <- PAR.try addressStrings <|> (string "NIL" >> return [[]])
        spaces
        cc <- PAR.try addressStrings <|> (string "NIL" >> return [[]])
        spaces <?> "fail1"
        bcc <- PAR.try addressStrings <|> (string "NIL" >> return [[]])
        spaces <?> "fail1"
        PAR.try quotedStr <|> string "NIL"
        spaces
        quotedStr
        string "))" <?> "fail1"
        eol
        return MailAll { seen = seen, date = date_time, subject = subject, from = from, to = to,
                   cc = cc, fetchid = fetchid }

flagsIsSeen :: CharParser () Bool
flagsIsSeen = do
        char '('
        flags <- sepBy (notSpaces) spaces
        char ')'
        return $ any (== "\\Seen") flags

notSpaces = many1 (satisfy (\x -> (not $ isSpace x)  && (x /= ')') ))

addressStrings :: CharParser () [String]
addressStrings = do
        char '('
        addresses <- many address
        char ')'
        return addresses

address :: CharParser () String
address = do
        char '('
        PAR.try quotedStr <|> string "NIL"
        spaces
        many ( noneOf " " )
        spaces
        a <- quotedStr
        spaces
        b <- quotedStr
        char ')'
        return $ a ++ "@" ++ b

quotedStr :: CharParser () String
quotedStr = 
    do char '"'
       content <- many (noneOf "\"")
       char '"' <?> "quote at end of cell"
       return content

parseSelect :: CharParser () Int
parseSelect = do
                line
                line
                i <- countLine
                return i

line = do
        many (noneOf "\r\n")
        eol

countLine = do
        char '*'
        spaces
        i <- number
        spaces
        string "EXISTS"
        return i

number = do 
            i <- many digit
            return (read i :: Int)

eol =   PAR.try (string "\n\r")
    <|> PAR.try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"


getMailForAccount accname = do
    sStorage <- newIORef undefined
    hostname_t <- getFromCfg accname "hostname"
    port_t <- getFromCfg accname "port"
    username_t <- getFromCfg accname "username"
    password_t <- getFromCfg accname "password"

    let hostname = forceEither hostname_t
        port = forceEither port_t
        username = forceEither username_t
        password = forceEither password_t
    runTLS (getDefaultParams sStorage Nothing) hostname (fromIntegral $ (read port :: Int)) $ \ctx -> do
        handshake ctx
        d <- recvData ctx
        sendData ctx $ LC.pack ( "a01 login " ++ username ++ " " ++ password ++ "\r\n" )
        d <- recvData ctx
        LC.putStrLn d
        sendData ctx $ LC.pack ( "a02 select inbox\r\n" )
        d <- recvData ctx
        i <- return $ either (\x -> 0 :: Int ) (\x -> x) ( parse parseSelect "(parseSelect)" (LC.unpack d) )

        parseTest parseSelect (LC.unpack d)
        threadDelay 3000
        sendData ctx $ LC.pack ( "a04 fetch " ++ show (i-10) ++ ":" ++ show i ++ " all\r\n" )
        d <- recvData ctx
       
        LC.putStrLn d

        all <- return $ either (\x -> [] ) (\x -> x) (parse parseManyAlls "(parseAlls)" (LC.unpack d) )
        putStrLn $ show all 

        --parseTest flagsIsSeen "(\\Seen $NotJunk NotJunk)"
        sendData ctx $ LC.pack ( "a07 logout\r\n" )
        d <- recvData ctx
        LC.putStrLn d
        bye ctx
        return (d)

--view accountname
view args = getMailForAccount (args !! 0)
                
   
