module MailClientWindow where

import qualified Text.Email.Validate as V(isValid)
import qualified Data.ByteString.Char8 as B(pack)
import qualified Data.Text.Lazy as T 	(Text, pack, fromStrict, toStrict)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network.HaskellNet.IMAP
import Control.Exception
import Data.Char 						(isSpace)
import Data.List 	 					(find)
import Data.List.Split 					(splitOn)
import Data.Text 						(Text, pack)
import qualified  Data.Text.IO as TIO	(putStrLn)
import Data.Text.Encoding				(decodeUtf8, encodeUtf8)
import Network.Mail.Mime hiding 		(simpleMail)
import System.IO.Error                 	(isDoesNotExistError, catchIOError)
import Control.Arrow                   	(second )
import Control.Monad                   	(when )
import System.Exit                     	(exitFailure )
import qualified ComposeWindow as C 	(main)
-- import Codec.MIME.Parse
import Codec.MIME.Utils
-- import Codec.MIME.Type
import qualified Data.ByteString as B
import qualified Data.Text as TT 		(concat)
import Network.HaskellNet.IMAP.Connection (IMAPConnection )
-- import Network.HaskellNet.SSL (Settings (..))
-- import Network.HaskellNet.IMAP.SSL ( connectIMAPSSLWithSettings
--                                    , defaultSettingsIMAPSSL
--                                    )
import Codec.MIME.Type  (MIMEValue (..), MIMEParam (..))
import Codec.MIME.Parse (parseMIMEMessage)

-- addMailbox :: String -> String -> String -> IO ()
addMailbox emailList uid = do -- sender subject time = do
	hbox <- hBoxNew True 0
	-- -- sender
	print uid
	senderlabel <- labelNew (Just (show uid))
	containerAdd hbox senderlabel
	-- -- subject
	-- subjectlabel <- labelNew (Just (show subject))
	-- containerAdd hbox subjectlabel
	-- -- time
	-- timelabel <- labelNew (Just (show time))
	-- containerAdd hbox timelabel
	-- add container to email list
	containerAdd emailList hbox

decode :: B.ByteString -> Text
decode = decodeUtf8

unravel :: (a -> IO b) -> [a] -> IO [b]
unravel func (x:xs) = do
	t <- func x
	ts <- unravel func xs
	return (t:ts)
unravel _ [] = do
	return []

-- fetchMessage :: IMAPConnection -> Word64 -> IO Text
fetchMessage conn uid = do
  content <- fetch conn uid
  return $ decodeUtf8 content

getMessageID :: IO Text -> IO Text
getMessageID raw = do
  content <- raw
  return $ pluckMessageID (parseMIMEMessage content)

pluckMessageID :: MIMEValue -> Text
pluckMessageID = pluckHeaderValue messageIDHeader

messageIDHeader :: Text
messageIDHeader = "message-id"

pluckHeaderValue :: Text -> MIMEValue -> Text
pluckHeaderValue headerName mime_val_headers =
  valueOrDefault $ find headerMatch mime_val_headers
  where
    headerMatch :: MIMEParam -> Bool
    headerMatch (MIMEParam headerName' _) = headerName' == headerName

    valueOrDefault :: Maybe MIMEParam -> Text
    valueOrDefault Nothing = TT.concat ["No ", headerName]
    valueOrDefault (Just (MIMEParam _ value)) = value

putMessageID :: IO Text -> IO ()
putMessageID msgID = msgID >>= TIO.putStrLn
-- parseMailHeaders (mail:mails) = parseHeaders.decode 

main = do
	-- Read config file
	let 
		config_file = "config.txt"
		parseConfig = map (second (drop 1) . break (=='=')) . lines
		defaultConfig = unlines 
	                    [ "imapserver=newmailhost.cc.iitk.ac.in"
	                    , "port=465"
	                    , "username=somebody"
	                    , "password=something"
	                    ]

	opts <- catchIOError (fmap parseConfig $ readFile config_file)$ \e -> do
			when (isDoesNotExistError e) $ do
				writeFile config_file defaultConfig
			exitFailure

	let readConfig name action = maybe (putStrLn$ "error: missing "++name++" option from "++config_file) action$ lookup name opts

	readConfig "imapserver"$ \imapserver ->
		readConfig "port"$ \port ->
			readConfig "username"$ \username ->
				readConfig "password"$ \password -> do

	-- GUI initialize
	initGUI
	Just xml <- xmlNew "glade/readmail.glade"
	window   <- xmlGetWidget xml castToWindow "window1"
	onDestroy window mainQuit
	statusLabel <- xmlGetWidget xml castToLabel "label1"
	composeButton <- xmlGetWidget xml castToButton "button1"
	fetchButton <- xmlGetWidget xml castToButton "button2"
	emailList <- xmlGetWidget xml castToVBox "vbox2"
	onClicked composeButton C.main
	let 
		printStatus message = set statusLabel [labelText := message]

	printStatus "Connecting to IMAP server..."
	con <- connectIMAP imapserver
	printStatus "Logging in..."
	loginResult <- try (login con username password) :: IO (Either SomeException  ())

	case loginResult of
		Left excp -> printStatus "Problems encountered while logging in."
		Right () -> printStatus "Login successful."
	

	onClicked fetchButton $ do
		select con "INBOX"
		msgs <- search con [ALLs]
		mails <- mapM_ (putMessageID . getMessageID . fetchMessage con) (take 1 msgs)
		
		print $ head mails
		-- print $ findMultipartNamed (pack "received") $ parseMIMEMessage $ decode $ head mails
		-- mapM_ (addMailbox emailList) (take 10 msgs)
		-- widgetShowAll emailList

	widgetShowAll window
	mainGUI
