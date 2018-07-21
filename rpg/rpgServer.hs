--meter um telnet localhost 4242
--https://www.text-image.com/convert/pic2ascii.cgi
--https://pixlr.com/editor/
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
  bind sock (SockAddrInet 4250 iNADDR_ANY)
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
  hPutStrLn hdl "Shall we begin?\n"
  hPutStrLn hdl "Tell us, what is your name?"
  name <- fmap init (hGetLine hdl)
  hPutStrLn hdl ("Welcome, " ++ name ++ "!")
  hPutStrLn hdl "Here in Hakrtolçiae Academy we have some roles,\
  \which one of them do you think you identify yourself?"
  hPutStrLn hdl (show roles)
  role <- setRole hdl
  hPutStrLn hdl ("Greetings to the world of Kalimarbadorna " ++ name ++ ", the " ++ role++"!")
  character <- turnPerIO (Per (name) (role) (getRoleAtk (role)) (getRoleHp (role)) Vazio)
  hPutStrLn hdl (show character)
  hPutStrLn hdl (show (getAtk character))
  hPutStrLn hdl "\nNow that you have chosen your role, lets begin our first dungeon"
  hPutStrLn hdl "Dungeon 1\n You are in a room where you can go north, though, \
  \you see a little goblin in front of you...\nYou will have to fight that little"
  hPutStrLn hdl (show goblin)
  result <- battle hdl character goblin 0
  hPutStrLn hdl (result)
  if result == "Win"
    then do
      hPutStrLn hdl "You got goblin's knife Atk + 1"
      if (getEquipRole goblinKnife) == Atk
        then do 
          character <- turnPerIO (Per(getName character)(getRole character)(getAtk character + (getEquipAtribute goblinKnife)) (getHp character) goblinKnife)
          hPutStrLn hdl "Congratulations"
      else
        do
          character <- turnPerIO (Per(getName character) (getRole character) (getAtk character) (getHp character + (getEquipAtribute goblinKnife)) goblinKnife)
          hPutStrLn hdl "Congratulations"
    else hPutStrLn hdl "Since you ran away, u get no item"
  
  hClose hdl



-------------------------- Tipagem Algébrica ------------------------------
type Nome = String
type Atributo = Int

data Personagem = Per Nome Role Atributo Atributo Equip
 deriving (Show, Eq)
data AorH  = Atk | Hp
 deriving (Show,Eq)
data Equip = Eqp Nome AorH Atributo | Vazio
 deriving (Show,Eq)
type Role = String
-------------------------- Declaração Importantes, de Mobs e Equips --------
-------------------------- Importante --------------------------------------
roles::[String]
roles = ["Assassin","Cleric","Mage","Ranger","Warrior"]
-------------------------- Mobs --------------------------------------------
goblin::Personagem
goblin = Per "Goburin" "Monster" 1 5 Vazio
-------------------------- Equips ------------------------------------------
goblinKnife::Equip
goblinKnife = Eqp "Goblin's Knife" Atk 1

-------------------------- Funções -----------------------------------------
turnPerIO::Personagem->IO Personagem
turnPerIO (Per n r atk hp eqp) = return (Per n r atk hp eqp)

getAtk::Personagem-> Int
getAtk (Per _ _ atk _ _) = atk

getHp::Personagem-> Int
getHp (Per _ _ _ hp _) = hp

getName::Personagem->String
getName (Per n _ _ _ _) = n

getRole::Personagem->String
getRole (Per _ r _ _ _) = r

getRoleAtk::Role->Int
getRoleAtk n
  | n == "Assassin" = 10
  | n == "Cleric"   = 3
  | n == "Mage"     = 7
  | n == "Warrior"  = 5

getRoleHp::Role->Int
getRoleHp n
  | n == "Assassin" = 3
  | n == "Cleric"   = 8
  | n == "Mage"     = 4
  | n == "Warrior"  = 5

getEquipName::Equip->String
getEquipName (Eqp n _ _) = n

getEquipRole::Equip->AorH
getEquipRole (Eqp _ ah _) = ah

getEquipAtribute::Equip->Int
getEquipAtribute (Eqp _ _ a) = a


setRole::Handle->IO String
setRole hdl = do 
   role <- fmap init (hGetLine hdl);
   role <- (roleDescription hdl role)
   return role
    
getHDL::Handle->IO Handle
getHDL hdl = do{
  role <- fmap init (hGetLine hdl);
  return hdl}



roleDescription::Handle->String->IO String
roleDescription hdl role
  | role == "Assassin" = 
    do
      hPutStrLn hdl  "\t       ``       \n\
                     \\t      ````      \n\
                     \\t     `` ```     \n\
                     \\t    ```  ```    \n\
                     \\t   ```    ```   \n\
                     \\t ````     ````  \n\
                     \\t````       ```` \n\
                     \\t ````     ````  \n\n\
          \\t***Assassin***\
          \\nAgile and furtive, a high damage but low hp character\n\
          \\n\tStatus\n\
          \HP: 3\t ###\n\
          \Attack: 8\t ********\n\
          \Skill:\t Shadow: Will always dodge 1st enemy atk\n"
      hPutStrLn hdl "Is this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "Then wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl
  | role == "Cleric" = 
    do
      hPutStrLn hdl  "\t     `````       \n\
                     \\t      ```        \n\
                     \\t``    ```    ``  \n\
                     \\t```````````````  \n\
                     \\t``    ```    ``  \n\
                     \\t      ```        \n\
                     \\t     `````       \n\
          \\t\n ***Cleric***\
          \\nA little weak character but also with great life and kindness\n\
          \\tStatus\n\
          \Health: 8\t ######## \n\
          \Attack: 3\t ***\n\
          \Skill:\t Will of Living: Will be revived once per battle"
      hPutStrLn hdl "Is this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "Then wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl
  | role == "Mage" = 
    do
      hPutStrLn hdl "\t             ```         `` \n\
                    \\t          ``````    ``````  \n\
                    \\t        `````````````````   \n\
                    \\t      `````  ```````````    \n\
                    \\t    `````````````  ```    ` \n\
                    \\t  ``````````````   ```  ``` \n\  
                    \\t `````      ``````  ``````` \n\   
                    \\t ````        `````  ``````  \n\  
                    \\t````          ```` ``````   \n\    
                    \\t`````         ``````````    \n\    
                    \\t ``````     `````````       \n\  
                    \\t  ```````````````           \n\  
                    \\t    ```````````             \n\
          \\n\t***Mage***\
          \\nHigh damage and avarage life          \n\
          \\tStatus                                \n\
          \Health: 4\t ####                       \n\
          \Attack: 7\t *******                     \n\
          \Skill: Fire Ball: Cause severe damage on oponent"
      hPutStrLn hdl "Is this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "Then wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl            
  | role == "Ranger" = 
    do
      hPutStrLn hdl "\
          \\t Assassin\t|\
          \\nAgile and furtive\n\
          \\tStatus\n\
          \HP: ###\n\
          \Attack: *******\n\
          \Skill:Will always dodge 1st enemy atk"
      hPutStrLn hdl "Is this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "Then wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl
  | role == "Warrior" = 
    do
      hPutStrLn hdl "\t                   ``````````` \n\
                    \\t                 ````       `` \n\
                    \\t                ````        `` \n\
                    \\t              `````         `` \n\
                    \\t             ````         ```` \n\
                    \\t    ````   `````       ``````  \n\
                    \\t  `````````````      ``````    \n\
                    \\t  ```` ``````      ``````      \n\
                    \\t   ````` ```    `````          \n\
                    \\t     ````` ```````             \n\
                    \\t    ```````` `````             \n\
                    \\t  ```````````` `````           \n\
                    \\t`````````` `````````           \n\
                    \\t````````     `````             \n\
                    \\t``````                         \n\

          \\t\n***Warrior***\t|\
          \\nA balanced character\n\
          \\tStatus\n\
          \Health: 5\t#####\n\
          \Attack: 5\t*****\n\
          \Skill:Gets +3 health as a armor"
      hPutStrLn hdl "Is this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "Then wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl            
  |otherwise = 
    do
     hPutStrLn hdl "You had written in a foregin language, please english";
     hPutStrLn hdl (show roles);
     setRole hdl


battle::Handle->Personagem->Personagem->Int->IO String
battle hdl principal monstro turno
  |getHp principal <= 0  && turno /= 0 = 
    if (show(getRole principal)) == "Cleric" 
      then battle hdl principal monstro turno
      else do
        hPutStrLn hdl ("===================================================================\
        \\tVocê perdeu e terá de reiniciar o jogo!!!!!!================================\n\n\n")
        hClose hdl
        return "Lost"
  |getHp monstro <= 0 = 
    do
      hPutStrLn hdl "Congratulations you won!"
      return "Win"
  |otherwise =
    do
      hPutStrLn hdl ("\n\n\nTurno: "++(show turno)++" entre: " ++(show (getName principal)) ++ "\
      \e " ++ (show (getName monstro))++"!")
      hPutStrLn hdl ("\n O que deseja fazer? (Attack(a),Skill(s),Run(r))")
      action<- fmap init (hGetLine hdl)
      case action of
        "a" -> 
          do
            principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)-(getAtk monstro)) Vazio)
            monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) - (getAtk principal)) Vazio)
            battle hdl principal monstro (turno+1)
        "s" -> 
          do
            case (getRole principal) of
              "Mage" -> 
                do
                  principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)-(getAtk monstro)) Vazio)
                  monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) - (9)) Vazio)
                  battle hdl principal monstro (turno+1)
              "Warrior" -> 
                do
                  principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)+3) Vazio)
                  monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) (getHp monstro) Vazio)
                  battle hdl principal monstro (turno+1)
              _ ->
                do
                  hPutStrLn hdl "You can't do that"
                  battle hdl principal monstro turno
        "r" -> 
          do
            hPutStrLn hdl "You ran away from battle chicken, also, the monster will still be there"
            return "Run"
    

      {--
      if action == "a"
        then
          do
            principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)-(getAtk monstro)) Vazio)
            monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) - (getAtk principal)) Vazio)
            battle hdl principal monstro (turno+1)
        else
          if action == "s"
            then 
              if (show(getRole principal) == "Mage")
                then 
                  principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)-(getAtk monstro)) Vazio)
                  monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) - (9)) Vazio)
                  battle hdl principal monstro (turno+1)
                else
                  if ((show(getRole principal))=="Warrior")
                    then
                      principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)+3) Vazio)
                      monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) Vazio)
                      battle hdl principal monstro (turno+1)
                    else
                      hPutStrLn hdl "You can't do that"
                      battle hdl principal monstro turno
            else if action == 
--}