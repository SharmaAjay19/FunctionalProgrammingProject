module Main where

import qualified Text.Email.Validate as V(isValid)
import qualified Data.ByteString.Char8 as B(pack)
import qualified Data.Text.Lazy as T 	(pack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network.Mail.SMTP
import Control.Exception
import Data.Char 						(isSpace)
import Data.List.Split 					(splitOn)
import Data.Text 						(pack)
import Network.Mail.Mime hiding 		(simpleMail)
import System.IO.Error                 	(isDoesNotExistError, catchIOError)
import Control.Arrow                   	(second )
import Control.Monad                   	(when )
import System.Exit                     	(exitFailure )

trim :: String -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: String -> String -> String
dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

myPacker = T.pack

unravel (x:xs) = do
	t <- x
	tList <- unravel xs
	return (t:tList)
unravel [] = do
	return []

mailSender :: String -> String -> String -> [String] -> [String] -> [String] -> String -> String -> [String] -> IO(Either SomeException ()) 
mailSender hostname username password rcptAddrList ccAddrList bccAddrList subject msg attachList = 
	let 
		sender = (Address (Just (pack username)) (pack username))
		recipients = [(Address (Just (pack x)) (pack x)) | x <- rcptAddrList]
		ccrecipients = [(Address (Just (pack x)) (pack x)) | x <- ccAddrList]
		bccrecipients = [(Address (Just (pack x)) (pack x)) | x <- bccAddrList]
		attachmentsIO = [(filePart (pack "Attachment") attach) | attach <- attachList]
	in 
		do
			attachments <- unravel attachmentsIO
			try (sendMailWithLogin hostname username password (simpleMail sender recipients ccrecipients bccrecipients (pack subject) ([(plainTextPart (myPacker msg))] ++ attachments))) 

main :: IO ()
main = do
	-- Read config file
  let config_file = "config.txt"
      parseConfig = map (second (drop 1) . break (=='=')) . lines
      defaultConfig = unlines 
                        [ "hostname=smtp.cc.iitk.ac.in"
                        , "port=25"
                        , "username=somebody"
                        , "password=something"
                        ]
  opts <- catchIOError (fmap parseConfig $ readFile config_file)$ \e -> do
            when (isDoesNotExistError e) $ do
                writeFile config_file defaultConfig
            exitFailure

  let readConfig name action = maybe (putStrLn$ "error: missing "++name++" option from "++config_file) action$ lookup name opts

  readConfig "hostname"$ \hostname ->
   readConfig "port"$ \port ->
    readConfig "username"$ \username ->
     readConfig "password"$ \password -> do


	initGUI
	Just xml <- xmlNew "glade/compose.glade"
	window   <- xmlGetWidget xml castToWindow "window1"
	onDestroy window mainQuit
	fileLabel <- xmlGetWidget xml castToLabel "label7"
	errorLabel <- xmlGetWidget xml castToLabel "label6"
	sendmailButton <- xmlGetWidget xml castToButton "button1"
	addAttachmentButton <- xmlGetWidget xml castToButton "button2"
	fileChooserButton <- xmlGetWidget xml castToFileChooserButton "filechooserbutton1"
	rcpAddrEntry <- xmlGetWidget xml castToEntry "entry1"
	ccAddrEntry <- xmlGetWidget xml castToEntry "entry2"
	bccAddrEntry <- xmlGetWidget xml castToEntry "entry3"
	subjectAddrEntry <- xmlGetWidget xml castToEntry "entry4"
	bodyAddrEntry <- xmlGetWidget xml castToTextView "textview1"

	onClicked addAttachmentButton $ do
		filepath <- fileChooserGetFilename fileChooserButton
		filenames <- labelGetText fileLabel
		case filepath of
			Just filename -> set fileLabel [labelText := filenames ++ filename ++ ","]
			Nothing -> return ()
		fileChooserUnselectAll fileChooserButton
	
	onClicked sendmailButton $ do
		-- User Input
		rcpAddr <- get rcpAddrEntry entryText
		ccAddr <- get ccAddrEntry entryText
		bccAddr <- get bccAddrEntry entryText
		subjectAddr <- get subjectAddrEntry entryText
		bodyAddr <- get bodyAddrEntry textViewImModule
		fileLabelText <- get fileLabel labelLabel

		-- Validations
		let filenames = [trim x | x <- splitOn "," fileLabelText, length (trim x) > 0]
		let rcptAddrList = [ trim x | x <- splitOn "," rcpAddr, V.isValid $ B.pack $ trim x]
		let	ccAddrList = [ trim x | x <- splitOn "," ccAddr, V.isValid $ B.pack $ trim x]
		let	bccAddrList = [ trim x | x <- splitOn "," bccAddr, V.isValid $ B.pack $ trim x]
		if length (rcptAddrList++ccAddrList++bccAddrList) > 0
			then do
				-- Send mail
				result <- mailSender hostname username password rcptAddrList ccAddrList bccAddrList subjectAddr bodyAddr filenames

				-- Exception Handling
				case result of
					Right _ -> widgetDestroy window
					Left exp -> set errorLabel [labelText := "Error: "++show(exp)]
			else
				set errorLabel [labelText := "Error: No recipient address?"]


	widgetShowAll window
	mainGUI