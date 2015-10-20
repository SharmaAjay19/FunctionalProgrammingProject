import Network.Mail.SMTP
import Data.Text hiding (empty, words)
import qualified Data.Text.Lazy as T (pack)
import Network.Mail.Mime hiding (simpleMail)
import Data.ByteString.Lazy hiding (pack,putStrLn)
myPacker = T.pack

makeMail sndrName sndrAddr rcptName rcptAddr subject msg attach = simpleMail (Address (Just (pack sndrName)) (pack sndrAddr)) [(Address (Just (pack rcptName)) (pack rcptAddr))] [] [] (pack subject) [(plainTextPart (myPacker msg)), attach]

main =  do
	putStrLn "Enter Sender's Name:"
	sndrName <- getLine
	putStrLn "Enter Sender's Address:"
	sndrAddr <- getLine
	putStrLn "Enter Recipient Name:"
	rcptName <- getLine
	putStrLn "Enter Recipient Address:"
	rcptAddr <- getLine
	putStrLn "Subject of your mail:"
	subject <- getLine
	putStrLn "Enter your message:"
	msg <- getLine
	putStrLn "Any attachment:"
	path <- getLine
	attach <- (filePart (pack "Attachment") path)
	putStrLn "Your IITK cc id credentials\nName:"
	usr <- getLine
	putStrLn "Password:"
	pwd <- getLine
       	sendMailWithLogin' "smtp.cc.iitk.ac.in" 25 usr pwd (makeMail sndrName sndrAddr rcptName rcptAddr subject msg attach)
                                                                                  
