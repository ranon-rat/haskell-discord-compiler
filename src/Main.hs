
module Main where

import Calamity (EventType (MessageCreateEvt), Token (BotToken), defaultIntents, react, runBotIO, tell, Message (Message), EmbedFooter (text))
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Types.Model.Channel.Message(content)
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Text.Lazy (Text,pack,unpack )
import qualified Di
import DiPolysemy ( info, runDiToIO )
import Polysemy  ( embedToFinal, runFinal )

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

{-
-- this is for replace this kinda stuff
readFile token.txt
writeFile "penis" "cum"
-}
regexReadAndWrite :: Regex
regexReadAndWrite =
  mkRegex
    "(readFile [\"_'a-zA-Z0-9]+\n|writeFile [\"'a-zA-Z0-9]+[\"'a-zA-Z0-9]+\n)"

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

executeCode :: String -> IO String
executeCode x = do
  id <- randomIO :: IO Int
  -- this create a file with the code inside
  writeFile ("./src/files/" ++ show id ++ ".hs") x
  -- then it run the file with ghci with some flags for avoid some security problems
  (_, Just outHandle, _, ph) <-
    createProcess
      (proc "sh" ["-c", "echo main | ghci " ++ "./src/files/" ++ show id ++ ".hs -no-global-package-db -no-user-package-db"])
        { std_out = CreatePipe -- we use a pipe because i dont want to stop the program if someone insert something like this
        {--
        recusiveShittyStuff=do
          print "penis"
          recusiveShittyStuff

        --}
        }
  -- then we get the output
  out <- hGetContents outHandle
  -- we wait a second
  threadDelay 1000000
  -- i terminate the process if someone insert me something like idk
  {--
        recusiveShittyStuff=do
          print "penis"
          recusiveShittyStuff
  --}
  terminateProcess ph
  -- remove the file
  removeFile ("./src/files/" ++ show id ++ ".hs")
  -- and we get the output
  return $drop 180 $ take (length out -21) out

-- maybe for u its a little bit weird that i take a slice of the string but it have a reason,
-- because if i dont take that slice it returns me something like this
{--
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( src/files/-4887131526902941531.hs, interpreted )
Ok, one module loaded.
*Main> 12
*Main> Leaving GHCi.
--}
--idkIfThisWork::EHType 'MessageCreateEvt->


runBot :: Text -> IO ()
runBot token = Di.new $ \di ->
  void
    . runFinal
    . embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . runBotIO (BotToken token) defaultIntents
    $ do
      info @Text "Connected successfully."
      react @'MessageCreateEvt $ \msg -> do
        let Message {content} = msg 
        let out=unpack content
        case take 14 out of
          "$compile ```hs" -> do
            stt<-executeCode$replaceSomeShittyStuff$getCode out 
            void . tell @Text msg $pack stt
          
       
          

main :: IO ()
main = do
  token <- readFile "token.txt"
  runBot $ pack token