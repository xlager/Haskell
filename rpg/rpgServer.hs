--meter um telnet localhost 4242
--https://www.text-image.com/convert/pic2ascii.cgi
module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
 
main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4248 iNADDR_ANY)
  listen sock 2
  mainLoop sock
 
type Msg = (Int, String)
 
mainLoop :: Socket  -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  close sock
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock,_)  = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "Welcome to the World of Smash by P&P Productions"
  hPutStrLn hdl "Shall we begin?"
  hPutStrLn hdl "\
  \      -.        -.        \n\
  \    :+Mh++++++++Mh+.      \n\
  \   -smMMMMMMMMMMMMMMhs     \n\
  \   +MMMMNNNNNNNNNMMMMM     \n\
  \   +MMMmy:-----:/mmMMM     \n\
  \   +MMN./y:....os.oMMM     \n\
  \   /MMNso/-.+:.:/sdMMM     \n\
  \   `:hmmmdddmmdddmmm+:     \n\
  \..   ...oMMMMMMMN...`      \n\
  \+s:::` :sMMMMMMMN:-        \n\
  \.-oooooMdomNohMyoMs        \n\
  \`.     -/Ms hN +M:`Ms        \n\
  \  :- `-Ns.hm`+N/-Ns.::     \n\
  \  ohoshso/+oyhosh-      \n\
  \  `-.-++/++/////+/.`      "
  hPutStrLn hdl "Tell us, what is your name?"
  name <- fmap init (hGetLine hdl)
  hPutStrLn hdl ("Welcome, " ++ name ++ "!")
  hPutStrLn hdl "Here in Hakrtolçiae Academy we have some roles,\
  \which one of them do you think you identify yourself?"
  hPutStrLn hdl (show roles)
  role <-return getRole hdl
  --role <- roleDescription hdl role
  hPutStrLn hdl (""++(show role))
  --hPutStrLn hdl ("OK DEAR" ++ name ++" "++ role ++"!")
  hPutStrLn hdl "\nNow that you have chosen your role, lets begin our first dungeon"
  hPutStrLn hdl "Dungeon 1\n You are in a room where you can go north, though, \
  \you see a little goblin in front of you...\nYou will have to fight that little"

--por a seleção e td que envolva o HDL diretamente, no main por hora
getRole::Handle->String
getRole hdl = do 
    role <- fmap init (hGetLine hdl)
    role
    

roles::[String]
roles = ["Assassin","Cleric","Mage","Ranger","Warrior"]

roleDescription::Handle->String->IO ()
roleDescription hdl role
  | role == "Assassin" = 
    do
      hPutStrLn hdl "\
          \\t Assassin\t|\
          \\nAgile and furtive\n\
          \\tStatus\n\
          \HP: ###\n\
          \Attack: *******\n\
          \Skill:Will always dodge 1st enemy atk"
      hPutStrLn hdl (role)
  |otherwise = do 
    hPutStrLn hdl "You chose a bad role, chose again"
    hPutStrLn hdl (show roles)
    role <- fmap init (hGetLine hdl)
    roleDescription hdl role