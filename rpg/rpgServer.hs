--meter um telnet localhost 4242
--https://www.text-image.com/convert/pic2ascii.cgi
--https://pixlr.com/editor/
--threadDelay 10000000 --Delay de 10 segundos
module Main where

import Network.Socket           --Biblioteca para conexão multi-computadores
import System.IO                --Biblioteca para comandos de entrada e saída  
import Control.Exception
import Control.Concurrent       --Biblioteca para uso de threads
import Control.Monad (when)
import Control.Monad.Fix (fix)

-------------------------- Inicialização do socket de comunicação ---------------------------------------------
main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4243 iNADDR_ANY)
  listen sock 2
  mainLoop sock
 
-------------------------- Socket que fica ouvindo portas e criando threads ----------------------------------- 
mainLoop :: Socket  -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  close sock
  mainLoop sock

-------------------------- Aqui começa o "Main", onde roda o jogo ---------------------------------------------
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock,_)  = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "\tWelcome to the World of Smash by P&P Productions"
  hPutStrLn hdl "\tShall we begin?\n"
  hPutStrLn hdl "\tTell us, what is your name?"
  name <- fmap init (hGetLine hdl)
  hPutStrLn hdl ("\tWelcome, " ++ name ++ "!")
  hPutStrLn hdl "\tHere in Hakrtolçiae Academy we have some roles,\
  \which one of them do you think you identify yourself?"
  hPutStrLn hdl (show roles)
  role <- setRole hdl
  hPutStrLn hdl ("\tGreetings to the world of Kalimarbadorna " ++ name ++ ", the " ++ role ++"!")
  character <- turnPerIO (Per (name) (role) (getRoleAtk (role)) (getRoleHp (role)) empty)
  hPutStrLn hdl (show character)
  hPutStrLn hdl (show (getAtk character))
  hPutStrLn hdl "\n\tNow that you have chosen your role, lets begin our first dungeon"
  printMap hdl 1 
  result <- battle hdl character goblin 0
  hPutStrLn hdl (result)
  if result == "Win"
    then do
      hPutStrLn hdl "\tYou got an item"
      character <- (aquireEquip hdl character goblinKnife);
      printChar hdl character goblin
    else hPutStrLn hdl "\tSince you ran away, u get no item"
  hClose hdl



-------------------------- Tipagem Algébrica ------------------------------------------------------------------
type Nome = String
type Atributo = Int

data Personagem = Per Nome Role Atributo Atributo Equip
 deriving (Show, Eq)
data AorH  = Atk | Hp | NA
 deriving (Show,Eq)
data Equip = Eqp Nome AorH Atributo | Vazio
 deriving (Show,Eq)
type Role = String
-------------------------- Declaração Importantes, de Mobs e Equips -------------------------------------------
-------------------------- Importante -------------------------------------------------------------------------
roles::[String]
roles = ["Assassin","Cleric","Mage","Ranger","Warrior"]
-------------------------- Mobs -------------------------------------------------------------------------------
goblin::Personagem
goblin = Per "Goburin" "Monster" 1 5 Vazio
dragon::Personagem
dragon = Per "Dragon" "Monster" 100 5 Vazio
-------------------------- Equips -----------------------------------------------------------------------------
empty::Equip
empty = Eqp "Bare Hands" Atk 0
goblinKnife::Equip
goblinKnife = Eqp "Goblin's Knife" Atk 1

-------------------------- Funções ----------------------------------------------------------------------------
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

getEquip::Personagem-> Equip
getEquip (Per _ _ _ _ eqp) = eqp

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
getEquipName Vazio = "Vazio"
getEquipName (Eqp n _ _) = n

getEquipRole::Equip->AorH
getEquipRole Vazio = NA
getEquipRole (Eqp _ ah _) = ah

getEquipAtribute::Equip->Int
getEquipAtribute Vazio = 0
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
  | role == "Assassin" || role == "assassin" = 
    do
      hPutStrLn hdl  "\t       ``       \n\
                     \\t      ````      \n\
                     \\t     `` ```     \n\
                     \\t    ```  ```    \n\
                     \\t   ```    ```   \n\
                     \\t ````     ````  \n\
                     \\t````       ```` \n\
                     \\t ````     ````  \n\n\
          \\t\t***Assassin***\
          \\n\tAgile and furtive, a high damage but low hp character\n\
          \\n\tStatus\n\
          \\tHealth: 3\t ###\n\
          \\tAttack: 8\t ********\n\
          \\tSkill:\t Shadow: Will always dodge 1st enemy atk\n"
      hPutStrLn hdl "\tIs this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "\tThen wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl
  | role == "Cleric" || role == "cleric" = 
    do
      hPutStrLn hdl  "\t     `````       \n\
                     \\t      ```        \n\
                     \\t``    ```    ``  \n\
                     \\t```````````````  \n\
                     \\t``    ```    ``  \n\
                     \\t      ```        \n\
                     \\t     `````       \n\
          \\t\t ***Cleric***\
          \\n\tA little weak character but also with great life and kindness\n\
          \\tStatus\n\
          \\tHealth: 8\t ######## \n\
          \\tAttack: 3\t ***\n\
          \\tSkill:\t Will of Living: Will be revived once per battle"
      hPutStrLn hdl "\tIs this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "\tThen wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl
  | role == "Mage" || role == "mage" = 
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
          \\n\tHigh damage and avarage life          \n\
          \\tStatus                                \n\
          \\tHealth: 4\t ####                       \n\
          \\tAttack: 7\t *******                     \n\
          \\tSkill: Fire Ball: Cause severe damage on oponent"
      hPutStrLn hdl "\tIs this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "\tThen wich one is your choice?";
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
      hPutStrLn hdl "\tIs this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "\tThen wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl
  | role == "Warrior" || role == "warrior" = 
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

          \\n\t\t***Warrior***\t|\
          \\nA balanced character\n\
          \\tStatus\n\
          \\tHealth: 5\t#####\n\
          \\tAttack: 5\t*****\n\
          \\tSkill:Gets +3 health as a armor"
      hPutStrLn hdl "\tIs this your choice?(y/n)"
      choice <- fmap init (hGetLine hdl)
      if choice == "y"
        then return role
        else 
          do
            hPutStrLn hdl "\tThen wich one is your choice?";
            hPutStrLn hdl (show roles);
            setRole hdl            
  |otherwise = 
    do
     hPutStrLn hdl "\tYou had written in a foregin language, please english";
     hPutStrLn hdl (show roles);
     setRole hdl


battle::Handle->Personagem->Personagem->Int->IO String
battle hdl principal monstro turno
  |getHp principal <= 0  && turno /= 0 = 
    if getRole principal == "Cleric" 
      then 
        do
          principal <- turnPerIO  (Per(getName principal)("-Cleric")(getRoleAtk (getRole principal))(getRoleHp (getRole principal)) empty)
          printChar hdl principal monstro
          battle hdl principal monstro turno
      else 
        do
          hPutStrLn hdl ("===================================================================\
          \Você perdeu e terá de reiniciar o jogo!!!!!!================================\n\n\n")
          hClose hdl
          return "Lost"
  |getHp monstro <= 0 = 
    do
      hPutStrLn hdl "\tCongratulations you won!"
      return "Win"
  |otherwise =
    do
      printChar hdl principal monstro
      hPutStrLn hdl ("\n\tTurno: "++(show turno))
      hPutStrLn hdl ("\n\t O que deseja fazer? (Attack(a),Skill(s),Run(r))")
      action<- fmap init (hGetLine hdl)
      case action of
        "a" -> 
          do
            if getRole principal == "Assassin" && turno == 0 
              then
                do
                  principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)(getHp principal) Vazio)
                  monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) - (getAtk principal)) Vazio)
                  battle hdl principal monstro (turno+1)
              else
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
                  hPutStrLn hdl "\tYou can't do that"
                  battle hdl principal monstro turno
        "r" -> 
          do
            hPutStrLn hdl "\tYou ran away from battle chicken, also, the monster will still be there"
            return "Run"
    
printChar::Handle->Personagem->Personagem->IO ()
printChar hdl personagem monstro =
  do
    hPutStrLn hdl ("\t| "++ show(getName personagem) ++"\t\t |\t\t\t| "++ show(getName monstro)++ "\t |\n\t| \
    \Health: " ++ show(getHp personagem) ++"\t\t |\t\t\t| Health: "++ show(getHp monstro) ++ "\t |\n\t| \
    \Attack: " ++ show(getAtk personagem) ++"\t\t |\t\t\t| Attack: "++ show(getAtk monstro) ++ "\t \n\t| \
    \Equip: " ++ show(getEquipName (getEquip personagem)) ++"|\t\t\t| Equip: "++ show(getEquipName (getEquip monstro)) ++ "\t |")
    
printMap::Handle->Int->IO ()
printMap hdl num = 
  do
    case num of 
      1 -> 
        do
          hPutStrLn hdl ("\tYou find yourself in a room, where the only thing you can do is north, though there is\
          \ a goblin blocking the door, you will have to fight him")
          hPutStrLn hdl ("\t------| |------\n\
                         \\t|      #      |\n\
                         \\t|             |\n\
                         \\t|      *      |\n\
                         \\t|             |\n\
                         \\t---------------")          
      2 -> 
        do
          hPutStrLn hdl ("\t You entered the second room, at north")
      3 -> 
        do
          hPutStrLn hdl ("bye")
      _ -> 
        do
          hPutStrLn hdl ("bye")

aquireEquip::Handle->Personagem->Equip->IO Personagem
aquireEquip hdl character eqp = 
  do
    hPutStrLn hdl ("\tDo you want the: " ++ show(getEquipName eqp) ++"\t\n"++ show(getEquipRole eqp) ++ ": +" ++ show(getEquipAtribute eqp)++"(y/n)")
    choice <- fmap init (hGetLine hdl)
    if choice == "y" || choice == "yes"
      then
        case (getEquipRole eqp) of
          Atk -> turnPerIO (Per(getName character)(getRole character)(getRoleAtk (getRole character) + (getEquipAtribute eqp)) (getHp character) eqp)
          _ ->  turnPerIO (Per(getName character) (getRole character) (getAtk character) (getRoleHp (getRole character)  + (getEquipAtribute eqp)) eqp)
      else
        turnPerIO (Per(getName character) (getRole character) (getAtk character) (getHp character) (getEquip character))