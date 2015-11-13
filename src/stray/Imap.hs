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
	msgs <- search con [ALLs]
	mapM_ print (take 4 msgs)
	forM_ (take 4 msgs) (fetch con >=> print)
