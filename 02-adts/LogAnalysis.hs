{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ('I':(' ':s)) =

  let
    splitLog = words s
  in
    LogMessage
      Info
      (parseFirstInt $ splitLog)
      (unwords . drop 1 $ splitLog)

parseMessage ('W':(' ':s)) =
  let
    splitLog = words s
  in
    LogMessage
      Warning
      (parseFirstInt $ splitLog)
      (unwords . drop 1 $ splitLog)

parseMessage ('E':(' ':s)) =
  let
    splitLog = words s
  in
    LogMessage
      (Error . parseFirstInt $ splitLog)
      (parseFirstInt . drop 1 $ splitLog)
      (unwords . drop 2 $ splitLog)

parseMessage msg = Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
-- insert ignores Unknown LogMessages
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert _ tree@(Node _ (Unknown _) _) = tree
insert
  insertMsg@(LogMessage _ insertTime _)
  (Node leftChild nodeMsg@(LogMessage _ nodeTime _) rightChild)
  | insertTime >= nodeTime =
    Node leftChild nodeMsg (insert insertMsg rightChild)
  | otherwise =
    Node (insert insertMsg leftChild) nodeMsg rightChild

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftChild msg rightChild) = (inOrder leftChild) ++ [msg] ++ (inOrder rightChild)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = msgs
  |> (filter errorsAtLeast50)
  |> foldl (flip insert) Leaf
  |> inOrder
  |> map extractKnownMessage

main :: IO ()
main = let
    parseMsgs = testParse parse 11 "sample.log"
  in
  do
    parseMsgs
      >>= print
    parseMsgs
      >>= build
        .> inOrder
        .> map extractKnownMessage
        .> foldl (\str msg -> str ++ msg ++ "\n") ""
        .> return
      >>= print
    testWhatWentWrong parse whatWentWrong "sample.log"
      >>= print
    testWhatWentWrong parse whatWentWrong "error.log"
      >>= print

-- helper functions

parseFirstInt :: [String] -> Int
parseFirstInt = (read :: String -> Int) . head

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

extractKnownMessage :: LogMessage -> String
extractKnownMessage (LogMessage _ _ msg) = msg
-- this is redundant because insert already removes Unknown messages
extractKnownMessage (Unknown _) = ""

errorsAtLeast50 :: LogMessage -> Bool
errorsAtLeast50 (LogMessage (Error i) _ _)
  | i >= 50 = True
  | otherwise = False
errorsAtLeast50 _ = False
