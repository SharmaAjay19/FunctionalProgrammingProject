
import Control.Arrow                   ( second )
import Control.Concurrent              ( forkIO, threadDelay )
import Control.Exception               ( mask_ )
import Control.Monad                   ( when )
import qualified Data.ByteString as B  ( ByteString )
import qualified Data.ByteString.Char8 as C ( ByteString, putStrLn )
import Data.Char                       ( isDigit )
import Network.HaskellNet.IMAP.Connection as I  ( IMAPConnection )
import Network.HaskellNet.IMAP   as I  ( list, select, search, SearchQuery(..), fetch
                                       , connectIMAPPort, login , capability, close, logout
                                       )
import Network.Socket            as S  ( HostName, PortNumber )
import System.Directory                ( canonicalizePath )
import System.Environment              ( getArgs )
import System.Exit                     ( exitFailure )
import System.IO                       ( Handle )
import System.IO.Error                 ( isDoesNotExistError, catchIOError)

import SSLWrap                         ( mapSSL, myForkIO )

import qualified Codec.MIME.String.QuotedPrintable as QP
import qualified Codec.Text.IConv as Iconv
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Exception (bracket)


main :: IO ()
main = do
  let config_file = "config.txt"
      parseConfig = map (second (drop 1) . break (=='=')) . lines
      defaultConfig = unlines 
                        [ "hostname=imap.gmail.com"
                        , "port=993"
                        , "username=somebody@gmail.com"
                        , "passwd=something"
                        , "ssl_wrap_port=3004"
                        , "cafile=/etc/ssl/certs/ca-certificates.crt" 
                        ]
  opts <- catchIOError (fmap parseConfig $ readFile config_file)$ \e -> do
            when (isDoesNotExistError e)$ do
                writeFile config_file defaultConfig
                putStrLn$ unlines 
                  [ ""
                  , "Thanks for using imapget!"
                  , ""
                  , "You need to edit a configuration file to specify where to connect"
                  , "and which username and password to use."
                  , ""
                  , "I just created a default ./"++config_file++" file."
                  , "Please edit it to set your options."
                  ]
            exitFailure

  {- TODO Where did this repetition come from? Clean up -}
  let readConfig name action = maybe (putStrLn$ "error: missing "++name++" option from "++config_file) action$ lookup name opts

  readConfig "hostname"$ \hostname ->
   readConfig "port"$ \port ->
    readConfig "username"$ \username ->
     readConfig "passwd"$ \passwd ->
      readConfig "ssl_wrap_port"$ \ssl_wrap_port -> 
        readConfig "cafile"$ \cafile -> do

  args <- getArgs

  case args of
    ["list"] | all isDigit port ->
         main' IMAPConf 
                { icHostname = hostname
                , icPort = fromIntegral (read port :: Int)
                , icUsername = username 
                , icPasswd = passwd 
                , icSSLWrapPort = fromIntegral (read ssl_wrap_port :: Int)
                , cafile = cafile
                } 
                Nothing
    ["fetch",label] | all isDigit port ->
         main' IMAPConf 
                { icHostname =hostname
                , icPort = fromIntegral (read port :: Int)
                , icUsername = username 
                , icPasswd = passwd 
                , icSSLWrapPort = fromIntegral (read ssl_wrap_port :: Int)
                , cafile = cafile
                }
                (Just label)
    _ -> putStrLn "USAGE: imapget [list|fetch label]"


type UserName = String
type Password = String
type Label = String

data IMAPConf = IMAPConf 
    { icHostname :: S.HostName
    , icPort :: S.PortNumber
    , icUsername :: UserName
    , icPasswd :: Password
    , icSSLWrapPort :: S.PortNumber
    , cafile :: String
    } deriving (Show)

decodeUtf7 :: String -> String
decodeUtf7 =
  BL.unpack . (Iconv.convert "UTF-7" "UTF-8") . BL.pack 

-- bad way to do different commands
main' :: IMAPConf -> Maybe Label -> IO ()
main' conf mlabel = do 
  case mlabel of
    Nothing -> do
        putStrLn$ "Fetching mailboxes ..."
        withIMAP conf $ \ic -> do
            I.list ic >>= mapM_ (putStrLn . show . decodeUtf7 . snd) -- snd 

    Just label -> do
        putStrLn $ "Getting label " ++ label
        getEmails conf label C.putStrLn 


getEmails :: IMAPConf -> Label -> (B.ByteString -> IO a) -> IO ()
getEmails c label f = withIMAP c$ \ic -> do
  putStrLn$ "Selecting "++label++" ..."
  I.select ic label
  putStrLn$ "Retrieving "++label++" ..."
  I.search ic [ALLs]
     -- >>= mapM_ (\uid -> I.fetch ic uid >>= f)
     -- TODO parameterize this fetch stuff to work with range of Vmail style command
     >>= mapM_ (\uid -> putStrLn $ show uid)
       


withIMAP :: IMAPConf -> (I.IMAPConnection -> IO a) -> IO a
withIMAP c action = do
  -- launch thread for wrapping tcp with SSL
  cafilePath <- canonicalizePath (cafile c)
  putStrLn $ "Using cafile: "++cafilePath

  -- _ <- mask_ $ forkIO $ mapSSL cafilePath (icSSLWrapPort c) (icHostname c) (icPort c)
  forkIO $ mapSSL cafilePath (icSSLWrapPort c) (icHostname c) (icPort c)
  
  -- start imap communication
  threadDelay$ 500*1000
  putStrLn$ "Connecting to "++icHostname c++":"++show (icPort c)++" (wrapping with ssl through localhost:"++show (icSSLWrapPort c)++") ..."

  bracket 
    (connectIMAPPort "localhost" (icSSLWrapPort c))
    I.logout
    (\ic ->  do
      putStrLn$ "Authenticating user "++icUsername c++" ..."
      I.login ic (icUsername c) (icPasswd c)
      action ic
    )



