module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text, isPrefixOf, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Discord (DiscordHandle, DiscordHandler, RestCallErrorCode, RunDiscordOpts (discordOnEvent, discordToken), def, restCall, runDiscord)
import Discord.Internal.Rest.Channel (ChannelRequest)
import qualified Discord.Requests as R
import Discord.Types (Event (MessageCreate), Message (messageAuthor, messageChannel, messageText), User (userIsBot))

import System.IO (hGetContents)
import System.Process (CreateProcess (std_out), StdStream (CreatePipe), createProcess, proc, terminateProcess)
import System.Random (randomIO)
import Text.Regex (Regex, matchRegex, mkRegex, subRegex)

regexReadAndWrite :: Regex
regexReadAndWrite =
  mkRegex
    "(readFile |writeFile)"

replaceSomeShittyStuff :: String -> String
replaceSomeShittyStuff x =
  case matchRegex regexReadAndWrite x of
    Nothing -> x
    Just _ -> replaceSomeShittyStuff (subRegex regexReadAndWrite x "")

{-
$compile ```hs
main=print "hello world"
```
-}
getCode :: String -> String
getCode x = replaceSomeShittyStuff $drop 14 $take (length x - 3) x

executeCode :: Message -> IO (ChannelRequest Message) --(* -> *) (*)-- ChannelRequest Message
executeCode x = do
  id <- randomIO :: IO Int
  let code = replaceSomeShittyStuff $getCode $unpack $messageText x
  let name = "./src/files/" ++ show id ++ ".hs"
  -- this create a file with the code inside
  writeFile name code
  -- then it run the file with ghci with some flags for avoid some security problems
  (_, Just outHandle, _, ph) <-
    createProcess
      (proc "sh" ["-c", "echo main | ghci " ++ name ++ " -no-global-package-db -no-user-package-db;rm -rf"++name]){ std_out = CreatePipe}

  -- then we get the output
  outIO <- hGetContents outHandle
  -- we wait a second
  threadDelay 1000000
  let out="```" ++ take 500 (drop 180 $ take (length outIO -21) outIO) ++ "```"
  terminateProcess ph
  --removeFile name
  -- remove the file
  -- and we get the output
  return . R.CreateMessage (messageChannel x) $pack out

runBot :: Text -> IO ()
runBot token = do
  userFacingError <- runDiscord $ def {discordToken = token, discordOnEvent = eventHandler}
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = do
  case event of
    MessageCreate m -> unless (fromBot m) $ do
      --withReader(executeCode m )
      messReq <- liftIO (executeCode m)
      _ <- restCall messReq
      pure ()
    _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

main :: IO ()
main = do
  token <- readFile "token.txt"
  runBot $ pack token