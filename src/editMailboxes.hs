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
	putStrLn " 1. New Mailbox\n 2. Delete Mailbox\n 3. Rename Mailbox\n"
	opType <- getLine
	if opType == "1"
	then do
		putStrLn "Enter the Name of the New Mailbox"
		newName <- getLine
		create con newName
	else if opType == "2"
	then do
		putStrLn "Enter the Name of the Mailbox to be deleted"
		delName <- getLine
		delete con delName
	else if opType == "3"
	then do
		putStrLn "Enter the old Name"
		oldName <- getLine
		putStrLn "Enter the new Name"
		newName <- getLine
		rename con oldName newName
	else do
		putStrLn "Please enter just 1, 2, or 3. Exiting......"
	close con
