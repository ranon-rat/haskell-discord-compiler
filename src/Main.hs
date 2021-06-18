module Main where

import Control.Concurrent (threadDelay)
import System.IO (hGetContents)
import System.Process
    ( createProcess,
      proc,
      terminateProcess,
      CreateProcess(std_out),
      StdStream(CreatePipe) )
import Text.Regex (Regex, matchRegex, mkRegex, subRegex)

{-
-- this is for replace this kinda stuff
readFile token.txt
writeFile "penis" "cum"
-}
regexReadAndWrite :: Regex
regexReadAndWrite = mkRegex "(readFile [\"_'a-zA-Z0-9]+\n|writeFile [\"'a-zA-Z0-9]+[\"'a-zA-Z0-9]+\n)"

replaceSomeShittyStuff :: String -> String
replaceSomeShittyStuff x =
  case matchRegex regexReadAndWrite x of
    Nothing -> x
    Just _ -> replaceSomeShittyStuff (subRegex regexReadAndWrite x "")

-- ghc -e "import System" -hide-all-packages -no-global-package-db -no-user-package-dbֲ־־
{-
$compile ```hs
main=print "hello world"
```
-}
getCode :: String -> String
getCode x = replaceSomeShittyStuff $drop 14 $take (length x -3) x

executeCode :: String -> IO String
executeCode x = do
  (_, Just outHandle, _, ph) <- createProcess (proc "ghc"["-e",x,"-no-global-package-db","-no-user-package-db"]) {std_out = CreatePipe}

  out <- hGetContents outHandle
  threadDelay 1000000
  terminateProcess ph

  return out

main :: IO ()
main = do
  out<-executeCode "print 1"
  print out