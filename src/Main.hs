{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Discord (DiscordHandler, RunDiscordOpts (discordOnEvent, discordToken), def, restCall, runDiscord)
import Discord.Internal.Rest.Channel (ChannelRequest)
import qualified Discord.Requests as R
import Discord.Types
import System.Directory (removeFile)
import System.IO (hGetContents)
import System.Process (CreateProcess (std_err, std_out), StdStream (CreatePipe), cleanupProcess, createProcess, interruptProcessGroupOf, proc, terminateProcess)
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
    
find :: Eq a => [a] -> [a] -> Bool
find x  y= any (\ a -> take (length y) ( drop a x) == y) [0 .. (length x)]
{-
$compile ```hs
main=print "hello world"
```
-}
getCode :: String -> String
getCode x = replaceSomeShittyStuff $drop 14 $take (length x - 3) x

executeCode :: Message -> IO (ChannelRequest Message)
executeCode x = do
  id <- randomIO :: IO Int

  let nameCode = "./src/files/" ++ show (if id < 0 then id * (-1) else id) ++ ".hs"
      nameOut = "src/files/" ++ show (if id < 0 then id * (-1) else id) ++ ".txt"

  writeFile nameCode (replaceSomeShittyStuff $getCode $unpack $messageText x)
  (_, Just outHandler, Just errHandle, ph) <-
    createProcess
      (proc "sh" ["-c", "echo main | ghci " ++ nameCode ++ " -no-global-package-db -no-user-package-db  >" ++ nameOut ++ " & sleep 3;kill $!2>&1 "])
        { std_out = CreatePipe,
          std_err = CreatePipe
        }

  threadDelay $ 2 * 10 ^ 6
  terminateProcess ph
  outIO <- readFile nameOut

  errIO <- hGetContents errHandle
  -- src/files/6027314577073774077.hs
  -- echo main | ghci src/files/6027314577073774077.hs -no-global-package-db -no-user-package-db > src/files/6027314577073774077 & sleep 1;kill $!
  d <- hGetContents outHandler
  print d

  print outIO

  let message =
        R.CreateMessageEmbed
          (messageChannel x)
          "output"
          $def
            { 
              createEmbedAuthorName = "server",
              createEmbedAuthorUrl = "https://discord.gg/e52RFh7Cg2",
              createEmbedTitle = if not$find outIO "Failed" then "Big brain Moment" else "Error 😩",
              createEmbedDescription =
                pack $
                  "```"
                    ++ ( if not$find outIO "Failed"
                           then take 500 (drop 180 $ take (length outIO -21) outIO)
                           else "hs\n" ++ take 500 errIO++"\n"
                       )
                    ++ "```",
              createEmbedImage = if find outIO "Failed" then
                 Just $ CreateEmbedImageUrl "https://media1.tenor.com/images/039d5fa4895c07d58b8c88e69847cf16/tenor.gif?itemid=17634321"
                 else Just $ CreateEmbedImageUrl "https://i0.wp.com/media1.tenor.com/images/a7215e2bf39482df8bb694f132af5c78/tenor.gif?itemid=16327782?resize=91,91"
            }

  removeFile nameCode
  removeFile nameOut
  return message

runBot :: Text -> IO ()
runBot token = do
  userFacingError <- runDiscord $ def {discordToken = token, discordOnEvent = eventHandler}
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event =
  case event of
    MessageCreate m -> when (not (fromBot m) && take 1 (unpack $messageText m) == "$") $ do
      if take 8 (unpack $messageText m) == "$compile"
        then do
          _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
          messReq <- liftIO (executeCode m)
          _ <- restCall messReq
          pure ()
        else
          if take 7 (unpack $messageText m) == "$serve"
            then do
              _ <- restCall (R.CreateMessage (messageChannel m) "https://discord.gg/e52RFh7Cg2")
              pure ()
            else pure ()
      pure ()
    _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

main :: IO ()
main = do
  putStrLn "the bot is running"

  token <- readFile "token.txt"
  runBot $ pack token

{-
executeCode :: Message -> IO (ChannelRequest Message)
executeCode x = do
  id <- randomIO :: IO Int

  let nameCode = "./src/files/" ++ show (if id < 0 then id * (-1) else id) ++ ".hs"
      nameOut = "src/files/" ++ show (if id < 0 then id * (-1) else id) ++ ".txt"

  writeFile nameCode (replaceSomeShittyStuff $getCode $unpack $messageText x)
  (_, Just outHandler, Just errHandle, ph) <-
    createProcess
      (proc "sh" ["-c", "echo main | ghci " ++ nameCode ++ " -no-global-package-db -no-user-package-db > " ++ nameOut ++ " & sleep 3;kill $! "])
        { std_out = CreatePipe,
          std_err = CreatePipe
        }

  threadDelay $ 3 * 10 ^ 6
  terminateProcess ph
  outIO <- readFile nameOut

  errIO <- hGetContents errHandle
  -- src/files/6027314577073774077.hs
  -- echo main | ghci src/files/6027314577073774077.hs -no-global-package-db -no-user-package-db > src/files/6027314577073774077 & sleep 1;kill $!
  d <- hGetContents outHandler

  print errIO
  let message =
        R.CreateMessageEmbed
          (messageChannel x)
          "output"
          $def
            { createEmbedTitle = if null errIO then "big brain :)" else "error 😭",
              createEmbedDescription = pack $ "```" ++ (if null errIO then
                "\n" ++ take 500 (drop 180 $ take (length outIO -21) outIO)
                else "hs\n" ++ take 500 errIO) ++ "```",
              createEmbedImage = if not $null errIO then
                 Just $ CreateEmbedImageUrl "https://media1.tenor.com/images/039d5fa4895c07d58b8c88e69847cf16/tenor.gif?itemid=17634321"
                 else Just $ CreateEmbedImageUrl "https://i0.wp.com/media1.tenor.com/images/a7215e2bf39482df8bb694f132af5c78/tenor.gif?itemid=16327782?resize=91,91",
              createEmbedAuthorName = "server",
              createEmbedAuthorUrl = "https://discord.gg/e52RFh7Cg2"
            }

  removeFile nameCode
  removeFile nameOut
  return message

-}