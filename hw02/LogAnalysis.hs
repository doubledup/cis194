-- CIS 194 Homework 2

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

readFirstWord :: (Read a) => String -> a
readFirstWord = read . head . words

dropFirstWord :: String -> String
dropFirstWord = unwords . tail . words

parseMessage :: String -> LogMessage
parseMessage ('I':' ':s) = LogMessage Info (readFirstWord s)
                                      (dropFirstWord s)
parseMessage ('W':' ':s) = LogMessage Warning (readFirstWord s)
                                      (dropFirstWord s)
parseMessage ('E':' ':s) = LogMessage (Error (readFirstWord s))
                                      (readFirstWord . dropFirstWord $ s)
                                      (dropFirstWord . dropFirstWord $ s)
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage . lines $ s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert _ unknownTree@(Node _ (Unknown _) _) = unknownTree
insert newmsg@(LogMessage _ newstamp _) (Node t1 oldmsg@(LogMessage _ oldstamp _) t2)
  | newstamp < oldstamp = Node (insert newmsg t1) oldmsg t2
  | newstamp >= oldstamp = Node t1 oldmsg (insert newmsg t2)
  | otherwise = Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 msg t2) = (inOrder t1) ++ [msg] ++ (inOrder t2)

relevant :: LogMessage -> Bool
relevant (LogMessage (Error sev) _ _)
  | sev >= 50 = True
  | otherwise = False
relevant _ = False

getLogMessageText :: LogMessage -> String
getLogMessageText (LogMessage _ _ s) = s
getLogMessageText (Unknown s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getLogMessageText . filter relevant . inOrder . build

printTestParse :: Int -> FilePath -> IO ()
printTestParse n path = testParse parse n path >>= (sequence . fmap print) >>= (return . head)

printTestWhatWentWrong :: FilePath -> IO ()
printTestWhatWentWrong path = testWhatWentWrong parse whatWentWrong path >>= (sequence . fmap print) >>= (return . head)

main :: IO()
main = do
  putStrLn ""
  putStrLn ">>> Testing parsing on sample.log..."
  putStrLn ""
  printTestParse 10 "sample.log"
  putStrLn ""
  putStrLn ">>> Testing parsing on error.log..."
  putStrLn ""
  printTestParse 10 "error.log"

  putStrLn ""
  putStrLn ">>> Testing what went wrong in sample.log..."
  putStrLn ""
  testWhatWentWrong parse whatWentWrong "sample.log" >>= (sequence . (fmap print)) >>= (return . head)
  putStrLn ""
  putStrLn ">>> Testing what went wrong in error.log..."
  putStrLn ""
  testWhatWentWrong parse whatWentWrong "error.log" >>= (sequence . (fmap print)) >>= (return . head)

  putStrLn ""
  putStrLn ">>> All done!"
  putStrLn ""
