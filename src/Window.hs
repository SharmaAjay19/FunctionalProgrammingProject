module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
-- import SendMail
import Data.Char
import qualified Data.List.Split as S
import qualified Text.Email.Validate as V
import qualified Data.ByteString.Char8 as B

import Network.Mail.SMTP
import Data.Text hiding (empty, words, dropWhile, null, reverse, length)
import qualified Data.Text.Lazy as T (pack)
import Network.Mail.Mime hiding (simpleMail)
import Data.ByteString.Lazy hiding (pack,putStrLn, dropWhile, null, reverse, length)
import Control.Exception

trim :: String -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: String -> String -> String
dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

myPacker = T.pack
smtpServer = "smtp.cc.iitk.ac.in"
-- smtpServer = "smtp.gmail.com"
smtpPort = 25
useraddr = "vineetp@iitk.ac.in"
username = "vineetp"
password = "riya&68"

unravel (x:xs) = do
	t <- x
	tList <- unravel xs
	return (t:tList)
unravel [] = do
	return []

mailSender :: [String] -> [String] -> [String] -> String -> String -> [String] -> IO(Either SomeException ()) 
mailSender rcptAddrList ccAddrList bccAddrList subject msg attachList = 
	let 
		sender = (Address (Just (pack useraddr)) (pack useraddr))
		recipients = [(Address (Just (pack x)) (pack x)) | x <- rcptAddrList]
		ccrecipients = [(Address (Just (pack x)) (pack x)) | x <- ccAddrList]
		bccrecipients = [(Address (Just (pack x)) (pack x)) | x <- bccAddrList]
		attachmentsIO = [(filePart (pack "Attachment") attach) | attach <- attachList]
	in 
		do
			attachments <- unravel attachmentsIO
			try (sendMailWithLogin smtpServer username password (simpleMail sender recipients ccrecipients bccrecipients (pack subject) ([(plainTextPart (myPacker msg))] ++ attachments))) 

main = do
	initGUI
	Just xml <- xmlNew "compose.glade"
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
		let filenames = [trim x | x <- S.splitOn "," fileLabelText, length (trim x) > 0]
		let rcptAddrList = [ trim x | x <- S.splitOn "," rcpAddr, V.isValid $ B.pack $ trim x]
		let	ccAddrList = [ trim x | x <- S.splitOn "," ccAddr, V.isValid $ B.pack $ trim x]
		let	bccAddrList = [ trim x | x <- S.splitOn "," bccAddr, V.isValid $ B.pack $ trim x]
		if length rcptAddrList == 0
			then 

		-- Send mail
		result <- mailSender rcptAddrList ccAddrList bccAddrList subjectAddr bodyAddr filenames

		-- Exception Handling
		case result of
			Right _ -> widgetDestroy window
			Left exp -> set errorLabel [labelText := "Error: "++show(exp)]

	widgetShowAll window
	mainGUI