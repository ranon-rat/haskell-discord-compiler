module Main where

import Control.Concurrent (threadDelay)
import System.Directory ( removeFile )
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
-- some calamity shitty stuff


import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Metrics.Noop
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Generics.Labels    ()
import           Data.Maybe
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as T
import qualified Di
import           DiPolysemy
import qualified Polysemy                as P

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
        { std_out = CreatePipe-- we use a pipe because i dont want to stop the program if someone insert something like this
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


main = do
  token<-readFile "token.txt"

  Di.new $ \di ->
    void
  . P.runFinal
  . P.embedToFinal @IO
  . runDiToIO di
  . runCacheInMemory
  . runMetricsNoop
  . useConstantPrefix "!"
  . runBotIO (BotToken t) defaultIntents
  $ do
    info @Text "Connected successfully."
