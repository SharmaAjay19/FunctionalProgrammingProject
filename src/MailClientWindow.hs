module MailClientWindow where

import qualified Text.Email.Validate as V(isValid)
import qualified Data.ByteString.Char8 as B(pack)
import qualified Data.Text.Lazy as T 	(Text, pack, fromStrict, toStrict)
import Graphics.UI.Gtk
import qualified  Graphics.UI.Gtk.Gdk.Events as GDK
import Graphics.UI.Gtk.Glade
import Network.HaskellNet.IMAP
import Control.Exception
import Data.Char 						(isSpace)
import Data.List 	 					(find)
import Data.List.Split 					(splitOn)
import Data.Text 						(Text, pack, unpack)
import qualified  Data.Text.IO as TIO	(putStrLn)
import Data.Text.Encoding				(decodeUtf8, encodeUtf8)
import Network.Mail.Mime hiding 		(simpleMail)
import System.IO.Error                 	(isDoesNotExistError, catchIOError)
import Control.Arrow                   	(second )
import Control.Monad                   	(when )
import System.Exit                     	(exitFailure )
import qualified ComposeWindow as C 	(main)
import GHC.Word 						(Word64)
import Codec.MIME.Parse
import Codec.MIME.Utils
import Codec.MIME.Type
import Data.Maybe
import qualified Data.ByteString as B
import qualified Codec.MIME.Type as M 	(MIMEType( .. ))

-- selectMime
selectMime (m:mimevalues) 
	| (showMIMEType $ mimeType $ mime_val_type m) == (showMIMEType $ (M.Text (pack "plain"))) = Just (mimeParams $ mime_val_type m)
	| otherwise = selectMime mimevalues
selectMime [] = Nothing

-- addMailbox :: String -> String -> String -> IO ()
addMailbox emailList emailBody con (mailIndex, mailHeader) = do
	let paramMime = mime_val_headers $ parseMIMEMessage $ decode mailHeader
	let	subject = fromJust $ getParam "subject" paramMime
	let	sender = fromJust $ getParam "from" paramMime
	let	time = fromJust $ getParam "date" paramMime

	hbox <- hBoxNew True 0
	widgetSetName hbox mailIndex
	
	let clickEvent = do
		mail <- fetch con (read mailIndex :: Word64)
		textBuffer <- textViewGetBuffer emailBody
		let mailBody parsedMail = case mime_val_content $ parsedMail of 
			Single msg -> unpack $ msg
			Multi mimevalue -> mailBody (head mimevalue) -- show $ fromJust $ selectMime mimevalue

		textBufferSetText textBuffer (mailBody (parseMIMEMessage $ decode mail))
		textViewSetEditable emailBody False	
		textViewSetWrapMode emailBody WrapWord
		textViewSetBuffer emailBody textBuffer

	-- sender subject and time
	senderbutton <- buttonNewWithLabel ((unpack sender) ++ "|" ++ (unpack subject) ++ "|" ++ (unpack time))
	onClicked senderbutton clickEvent
	containerAdd hbox senderbutton

	containerAdd emailList hbox

decode :: B.ByteString -> Text
decode = decodeUtf8

unravel :: Show a => (a -> IO b) -> [a] -> IO [(String,b)]
unravel func (x:xs) = do
	t <- func x
	ts <- unravel func xs
	return ((show x, t):ts)
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
	emailBody <- xmlGetWidget xml castToTextView "textview2"
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
		mails <- unravel (fetchHeader con) (take 15 $ reverse msgs)
		
		-- print $ head mails
		mapM_ (addMailbox emailList emailBody con) mails
		widgetShowAll emailList

	widgetShowAll window
	mainGUI
