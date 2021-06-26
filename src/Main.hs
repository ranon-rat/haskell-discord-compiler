{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.Split (splitOn)
import Data.Text (Text, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Discord (DiscordHandler, RunDiscordOpts (discordOnEvent, discordToken), def, restCall, runDiscord)
import Discord.Internal.Rest.Channel (ChannelRequest)
import qualified Discord.Requests as R
import Discord.Types (CreateEmbed (createEmbedAuthorName, createEmbedAuthorUrl, createEmbedDescription, createEmbedImage, createEmbedTitle), CreateEmbedImage (CreateEmbedImageUrl), Event (MessageCreate), Message (messageAuthor, messageChannel, messageId, messageText), User (userIsBot))
import System.Directory (removeFile)
import System.IO (hGetContents)
import System.Process (CreateProcess (std_err, std_out), StdStream (CreatePipe), cleanupProcess, createProcess, interruptProcessGroupOf, proc, terminateProcess)
import System.Random (randomIO)
import Text.Regex (Regex, matchRegex, mkRegex, subRegex)

replaceSomeShittyStuff :: String -> String
replaceSomeShittyStuff x =
  case matchRegex (mkRegex "(readFile |writeFile)") x of
    Nothing -> x
    Just _ -> replaceSomeShittyStuff (subRegex (mkRegex "(readFile |writeFile)") x "")

find :: Eq a => [a] -> [a] -> Bool
find x y = any (\a -> take (length y) (drop a x) == y) [0 .. (length x)]

getCode :: String -> String
getCode x = replaceSomeShittyStuff $drop (if find x "```hs" then 14 else 12) $take (length x - 3) x

executeCode :: Message -> IO (ChannelRequest Message)
executeCode x = do
  id <- randomIO :: IO Int
  let (nameCode, nameOut) =
        ( "./src/files/" ++ show (if id < 0 then id * (-1) else id) ++ ".hs",
          "src/files/" ++ show (if id < 0 then id * (-1) else id) ++ ".txt"
        )
      code = replaceSomeShittyStuff $getCode $unpack $messageText x
  if not $null code
    then do
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
      d <- hGetContents outHandler
      print d

      let message =
            R.CreateMessageEmbed
              (messageChannel x)
              "output"
              $def
                { createEmbedAuthorName = "server",
                  createEmbedAuthorUrl = "https://discord.gg/e52RFh7Cg2",
                  createEmbedTitle = if not $find outIO "Failed" then "Big brain Moment" else "Error 😩",
                  createEmbedDescription =
                    pack $
                      "```"
                        ++ ( if not $find outIO "Failed"
                               then take 500 (drop 179 $ take (length outIO -21) outIO)
                               else "hs\n" ++ take 500 errIO ++ "\n"
                           )
                        ++ "```",
                  createEmbedImage =
                    if find outIO "Failed"
                      then Just $ CreateEmbedImageUrl "https://media1.tenor.com/images/039d5fa4895c07d58b8c88e69847cf16/tenor.gif?itemid=17634321"
                      else Just $ CreateEmbedImageUrl "https://i0.wp.com/media1.tenor.com/images/a7215e2bf39482df8bb694f132af5c78/tenor.gif?itemid=16327782?resize=91,91"
                }
      mapM_ removeFile [nameCode, nameOut]
      return message
    else return . R.CreateMessageEmbed (messageChannel x) "error" $def {createEmbedTitle = "Missing arguments", createEmbedImage = Just $ CreateEmbedImageUrl "https://media.discordapp.net/attachments/820472030474272769/858103586511519744/Screen_Shot_2021-06-25_at_16.57.05.png"}

runBot :: Text -> IO ()
runBot token = do
  userFacingError <- runDiscord $ def {discordToken = token, discordOnEvent = eventHandler}
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event =
  case event of
    MessageCreate m -> when (not (userIsBot (messageAuthor m)) && take 1 (unpack $messageText m) == "$") $ do
      case head $splitOn " " $unpack $messageText m of
        "$compile" -> do
          _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
          messReq <- liftIO (executeCode m)
          _ <- restCall messReq
          pure ()
        "$server" -> do
          _ <- restCall (R.CreateMessage (messageChannel m) "https://discord.gg/e52RFh7Cg2")
          _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "hearts")
          pure ()
        "$help" -> do
          _ <-
            restCall
              ( R.CreateMessageEmbed
                  (messageChannel m)
                  ""
                  $def
                    { createEmbedAuthorName = "server",
                      createEmbedAuthorUrl = "https://discord.gg/e52RFh7Cg2",
                      createEmbedTitle = "list of commands",
                      createEmbedDescription = "```md\n- $help *this command is for receive help lmao*\n- $github *if you want to see the source code*\n- $server *if you want to join us*\n- $compile *this compile the input ant then return the output* ```",
                      createEmbedImage = Just $ CreateEmbedImageUrl "https://media.discordapp.net/attachments/820472030474272769/858103586511519744/Screen_Shot_2021-06-25_at_16.57.05.png"
                    }
              )
          pure ()
        "$github" -> do
          _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "hearts")
          _ <- restCall (R.CreateMessage (messageChannel m) "https://github.com/ranon-rat/haskell-discord-compiler")
          pure ()
        _ -> pure ()
    _ -> pure ()

main :: IO ()
main = do
  putStrLn "the bot is running"

  token <- readFile "token.txt"
  runBot $ pack token