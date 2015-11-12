{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Network.HaskellNet.IMAP
import Control.Monad

imapServer = "newmailhost.cc.iitk.ac.in"
-- imapPort = "993"
username = ""
password = ""

main = do
	print "Connecting to IMAP server..."
	con <- connectIMAP imapServer -- (fromIntegral (read imapPort :: Int))
	print "Logging in..."
	login con username password
	print "Fetching messages..."
	mboxes <- list con
	mapM_ print mboxes
	select con "INBOX"
	putStrLn " 1. Search by Sender \n 2. Search by Receiver \n 3. Search by Subject \n 4. Search by Body \n"
	searchType <- getLine
	putStrLn "Enter the search term\n"
	searchTerm <- getLine
	msgs <- case searchType of
		"1" -> search con [(FROMs searchTerm)]
		"2" -> search con [(TOs searchTerm)]
		"3" -> search con [(SUBJECTs searchTerm)]
		"4" -> search con [(BODYs searchTerm)]
	forM_ msgs (fetch con >=> print)
