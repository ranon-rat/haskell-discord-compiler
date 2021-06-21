-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Text (Text, isPrefixOf, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Discord
  ( DiscordHandler,
    RestCallErrorCode,
    RunDiscordOpts (discordOnEvent, discordToken),
    def,
    restCall,
    runDiscord,
  )
import qualified Discord.Requests as R
import Discord.Types
  ( Event (MessageCreate),
    Message (messageAuthor, messageChannel, messageText),
    User (userIsBot),
  )
import System.Directory (removeFile)
import System.IO (hGetContents)
import System.Process
  ( CreateProcess (std_out),
    StdStream (CreatePipe),
    createProcess,
    proc,
    terminateProcess,
  )
import System.Random (randomIO)
import Text.Regex (Regex, matchRegex, mkRegex, subRegex)
--import qualified UnliftIO.Concurrent as U

{-
-- this is for replace this kinda stuff
readFile token.txt
writeFile "penis" "cum"
-}
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

executeCode :: Message -> IO (DiscordHandler (Either RestCallErrorCode Message))
executeCode x = do
  id <- randomIO
  -- this create a file with the code inside
  writeFile ("./src/files/" ++ show id ++ ".hs") (replaceSomeShittyStuff $getCode $unpack $messageText x)
  -- then it run the file with ghci with some flags for avoid some security problems
  (_, Just outHandle, _, ph) <-
    createProcess
      (proc "sh" ["-c", "echo main | ghci " ++ "./src/files/" ++ show id ++ ".hs -no-global-package-db -no-user-package-db"])
        { std_out = CreatePipe -- we use a pipe because i dont want to stop the program if someone insert something like this
        -- xd=do print "ass";xd
        }
  -- then we get the output
  out <- hGetContents outHandle
  -- we wait a second
  threadDelay 1000000
  -- i terminate the process if someone insert me something like this 
  -- xd=do print "ass";xd

  terminateProcess ph
  -- remove the file
  removeFile ("./src/files/" ++ show id ++ ".hs")
  -- and we get the output

  return $restCall (R.CreateMessage (messageChannel x) (pack $drop 180 $ take (length out -21) out))

-- maybe for u its a little bit weird that i take a slice of the string but it have a reason,
-- because if i dont take that slice it returns me something like this
{--
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/files/-4887131526902941531.hs, interpreted )
Ok, one module loaded.
*Main> 12
*Main> Leaving GHCi.
--}

pingpongExample :: Text -> IO ()
pingpongExample token = do
  userFacingError <- runDiscord $ def {discordToken = token, discordOnEvent = eventHandler}
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
    xd <- executeCode m
    pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower

main :: IO ()
main = do
  token <- readFile "token.txt"
  pingpongExample $ pack token