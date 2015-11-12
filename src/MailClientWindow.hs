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
import Codec.MIME.Parse
import Codec.MIME.Utils
import Codec.MIME.Type
import Data.Maybe
import qualified Data.ByteString as B




-- addMailbox :: String -> String -> String -> IO ()
addMailbox emailList mailHeader = do
	let paramMime = mime_val_headers $ parseMIMEMessage $ decode mailHeader
	let	subject = fromJust $ getParam "subject" paramMime
	let	sender = fromJust $ getParam "from" paramMime
	let	time = fromJust $ getParam "date" paramMime

	hbox <- hBoxNew True 0
	-- sender
	senderlabel <- labelNew (Just (show sender))
	containerAdd hbox senderlabel
	-- subject
	subjectlabel <- labelNew (Just (show subject))
	containerAdd hbox subjectlabel
	-- time
	timelabel <- labelNew (Just (show time))
	containerAdd hbox timelabel
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

getParam :: String -> [MIMEParam] -> Maybe Text
getParam attrName (paramMime:paramlist) 
	| (paramName paramMime) == (pack attrName) = Just (paramValue paramMime)
	| otherwise 				= getParam attrName paramlist
getParam attrName [] = Nothing

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
		mails <- unravel (fetchHeader con) (take 10 msgs)
		
		-- print $ head mails 

		-- print $ getParam "subject" $ mime_val_headers $ parseMIMEMessage $ decode $ head mails
		mapM_ (addMailbox emailList) mails
		widgetShowAll emailList

	widgetShowAll window
	mainGUI
