--Pedro Halmenschlager IPPD
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
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  mainLoop sock
 
-------------------------- Socket que fica ouvindo portas e criando threads ----------------------------------- 
mainLoop :: Socket  -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
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
  hPutStrLn hdl ("\t"++show roles)
  role <- setRole hdl
  hPutStrLn hdl ("\tGreetings to the world of Kalimarbadorna " ++ name ++ ", the " ++ role ++"!")
  character <- turnPerIO (Per (name) (role) (getRoleAtk (role)) (getRoleHp (role)) empty)
  hPutStrLn hdl ("\tYou are a: " ++ role ++ "\n\tHas Atk: " ++show(getAtk character) ++"\n\tHas HP: "++ show(getHp character) ++ "\
  \\n\tEquip: "++ show(getEquip character)++"");
  hPutStrLn hdl "\n\tNow that you have chosen your role, lets begin our first dungeon"
  threadDelay 2000000
  dungeonPlay hdl character 1 0
  hClose hdl

dungeonPlay::Handle->Personagem->Int->Int->IO()
dungeonPlay hdl character room state
  |room == 1 = 
      do
      printMap hdl room state
      hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
      choice<- fmap init (hGetLine hdl)
      if choice == "w" && state == 0 
        then 
          do result<- battle hdl character goblin 0
             if result == "Win"
              then 
                do
                  character<-(aquireEquip hdl character goblinKnife)
                  dungeonPlay hdl character 1 1
              else dungeonPlay hdl character room state
        else 
          if choice == "w" && state == 1
            then dungeonPlay hdl character 2 0
            else
              do
                hPutStrLn hdl ("\tYou can't go that way\n")
                dungeonPlay hdl character room state
        
    |room == 2 = 
      do 
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice <- fmap init (hGetLine hdl)
        if choice == "d" && (state == 0 || state == 1)
          then
            do 
              result <- battle hdl character spider 0
              if result == "Win"
               then
                  do 
                    if state == 0 
                      then
                        do dungeonPlay hdl character 2 2
                      else
                        do dungeonPlay hdl character 2 3
                else dungeonPlay hdl character room state
          else 
            do 
              if choice == "d" && (state == 2 || state == 3)
                then 
                  do
                    dungeonPlay hdl character 3 0
                else
                  do
                    if choice =="a" && (state == 0 || state == 2)
                      then 
                        do 
                          character <- (aquireEquip hdl character bracelet)
                          if state == 0 
                            then 
                              do 
                                dungeonPlay hdl character 2 1
                            else
                              do
                                dungeonPlay hdl character 2 3
                      else
                       do
                        hPutStrLn hdl ("\tYou can't go that way\n")
                        dungeonPlay hdl character room state
        
    |room == 3 = 
      do
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice<- fmap init (hGetLine hdl)
        if choice == "a"
          then 
            do  dungeonPlay hdl character 2 0
          else 
            if choice == "d"
              then
                do
                   dungeonPlay hdl character 4 0
              else
                if choice == "w"
                  then 
                    do
                      dungeonPlay hdl character 5 0
          else 
            hPutStrLn hdl ("\tYou can't go that way\n");
            dungeonPlay hdl character room state
        
    |room == 4 = 
      do
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice<- fmap init (hGetLine hdl)
        if choice == "d" && state == 0 
          then 
            do result<- battle hdl character mummy 0
               if result == "Win"
                then 
                  do
                    character<-(aquireEquip hdl character necklace) 
                    dungeonPlay hdl character 4 1
                else dungeonPlay hdl character room state
          else 
            if choice == "a"
              then dungeonPlay hdl character 3 0
              else
                do
                  hPutStrLn hdl ("\tYou can't go that way\n")
                  dungeonPlay hdl character room state
          
    |room == 5 =
      do 
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice<- fmap init (hGetLine hdl)
        if choice == "a" && (state == 0 || state == 2)
          then 
            do 
              result<- battle hdl character skelleton 0
              if result == "Win"
                then 
                  do
                    if state == 0 
                      then 
                        do
                          character<-(aquireEquip hdl character scmitar) 
                          dungeonPlay hdl character 5 1
                      else  
                        do
                          character<-(aquireEquip hdl character scmitar) 
                          dungeonPlay hdl character 5 3
                else dungeonPlay hdl character room state
           else 
            do
              if choice == "d" && (state == 0 || state == 1)
                then
                  do 
                    result<- battle hdl character ghost 0
                    if result == "Win"
                      then
                        do 
                          if state == 0 
                            then 
                              do dungeonPlay hdl character 5 2
                            else  
                              do dungeonPlay hdl character 5 3
                      else 
                        do dungeonPlay hdl character room state
                else
                  do
                    if choice =="a" && (state == 1 || state == 3 ) 
                      then
                        do dungeonPlay hdl character 7 0
                      else
                        do
                          if choice == "d" && (state == 2 || state == 3)
                            then
                              do dungeonPlay hdl character 6 0
                            else
                              do  
                                if choice == "w"
                                  then dungeonPlay hdl character 9 0
                                  else 
                                    do
                                      hPutStrLn hdl ("\tYou can't go that way\n")
                                      dungeonPlay hdl character room state
    |room == 6 =
      do
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice<- fmap init (hGetLine hdl)
        if choice == "d" && state == 0 
          then 
            do result<- battle hdl character medusa 0
               if result == "Win"
                then 
                  do
                    character <- turnPerIO (Per (getName character)(getRole character)(getRoleAtk (getRole character)+(getEquipAtribute rock))(getRoleHp (getRole character)) rock) 
                    dungeonPlay hdl character 6 1
                else dungeonPlay hdl character room state
          else 
            if choice == "a"
              then dungeonPlay hdl character 5 0
              else
                do
                  hPutStrLn hdl ("\tYou can't go that way\n")
                  dungeonPlay hdl character room state
    |room == 7 = 
      do
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice<- fmap init (hGetLine hdl)
        if choice == "a" && state == 0 
          then 
            do result<- battle hdl character mage 0
               if result == "Win"
                then 
                  do
                    character<-(aquireEquip hdl character wand)
                    dungeonPlay hdl character 7 1
                else dungeonPlay hdl character room state
          else 
            if choice == "d"
              then dungeonPlay hdl character 5 0
              else
                if choice == "w" then dungeonPlay hdl character 8 0
                else 
                  do
                    hPutStrLn hdl ("\tYou can't go that way\n")
                    dungeonPlay hdl character room state
          
    |room == 8 = 
      do
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice<- fmap init (hGetLine hdl)
        if choice == "w" && state == 0 
          then 
            do result<- battle hdl character mimic 0
               if result == "Win"
                then 
                  do
                    character<-(aquireEquip hdl character heart)  
                    dungeonPlay hdl character 8 1
                else dungeonPlay hdl character room state
          else 
            if choice == "s"
              then dungeonPlay hdl character 7 0
              else
                if choice == "d" 
                  then dungeonPlay hdl character 9 0
                else
                  do
                    hPutStrLn hdl ("\tYou can't go that way\n")
                    dungeonPlay hdl character room state
          
    |room == 9 = 
      do
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice<- fmap init (hGetLine hdl)
        if choice == "a"
          then 
            do  dungeonPlay hdl character 8 0
          else 
            if choice == "d"
              then
                do
                   dungeonPlay hdl character 10 0
              else
                if choice == "s"
                  then 
                    do
                      dungeonPlay hdl character 5 0
          else 
            hPutStrLn hdl ("\tYou can't go that way\n");
            dungeonPlay hdl character room state
    |room == 10 = 
      do
        printMap hdl room state
        hPutStrLn hdl ("\t Where to? left(a), up(w), right(d), down(s)")
        choice<- fmap init (hGetLine hdl)
        if choice == "d" && state == 0
          then 
            do result <- (battle hdl character necromancer 0)
               if result == "Win"
                then 
                  do
                    printEnding hdl
                else dungeonPlay hdl character room state
          else 
            if choice == "a"
              then
                do
                   dungeonPlay hdl character 9 0
              else 
                hPutStrLn hdl ("\tYou can't go that way\n");
                dungeonPlay hdl character room state
    |otherwise = do hPutStrLn hdl ("\t É BUG")
  
printEnding::Handle->IO ()
printEnding hdl = 
  do
    hPutStrLn hdl ("\tThank you very much for playing World of Smash, you beat the game and I wish you a very\
    \ happy day, thanks")
    threadDelay 5000000
    hPutStrLn hdl("\n\n\t\t\t A Game by P&P Productions - Pedro Halmenschlager")
    threadDelay 5000000
    hPutStrLn hdl ("\n\n\n\n\tThere's no such thing as credits")
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
roles = ["Assassin","Cleric","Mage","Warrior"]
-------------------------- Mobs -------------------------------------------------------------------------------
goblin::Personagem
goblin = Per "Goburin" "Goblin" 1 5 goblinKnife
spider::Personagem
spider = Per "Spidarak" "Spider" 3 8 Vazio
mummy::Personagem
mummy = Per "Tutankathoth" "Mummy" 5 20 necklace
skelleton::Personagem
skelleton = Per "Don Arantes" "Skelleton" 4 8 scmitar
ghost::Personagem
ghost = Per "Don Arantes(G)" "Ghost" 5 18 Vazio
medusa::Personagem
medusa = Per "Medusa" "Gorgon" 8 35 rock
mage::Personagem
mage = Per "Arantes VI" "Mage" 7 22 wand
mimic::Personagem
mimic = Per "Mimic" "Mimic" 8 40 Vazio
necromancer::Personagem
necromancer = Per "Arantes Thoth I" "Necromancer" 8 60 Vazio
-------------------------- Equips -----------------------------------------------------------------------------
empty::Equip
empty = Eqp "Bare Hands" Atk 0
goblinKnife::Equip
goblinKnife = Eqp "Goblin's Knife" Atk 1
rock::Equip
rock = Eqp "Stoned Equip" Atk 1
bracelet::Equip
bracelet = Eqp "Bracelet of Arantes II" Hp 3
necklace::Equip
necklace = Eqp "Necklace of Thoth" Hp 10
scmitar::Equip
scmitar = Eqp "Scmitar of Don Arantes" Atk 10
wand::Equip
wand = Eqp "Wand of Magi" Atk 15
heart::Equip
heart = Eqp "Mimic's heart" Hp 35
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
        then return "Assassin"
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
        then return "Cleric"
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
        then return "Mage"
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
        then return "Warrior"
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
          hPutStrLn hdl ("You return from dead with all life, but only the role atk and hp")
          principal <- turnPerIO  (Per(getName principal)("-Cleric")(getRoleAtk (getRole principal))(getRoleHp (getRole principal)) empty)
          printChar hdl principal monstro
          battle hdl principal monstro turno
      else 
        do
          hPutStrLn hdl ("===================================================================\
          \You must reborn as a new player!!!!!!================================\n\n\n")
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
                  hPutStrLn hdl ("You deal: "++ show(getAtk principal) ++"and got: -"++show(0)++" hp")
                  principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)(getHp principal) (getEquip principal))
                  monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) - (getAtk principal)) (getEquip monstro))
                  battle hdl principal monstro (turno+1)
              else
                do
                  hPutStrLn hdl ("You deal: "++ show(getAtk principal) ++" damage and got: -"++show(getAtk monstro) ++" hp")
                  principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)-(getAtk monstro)) (getEquip principal))
                  monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) - (getAtk principal)) (getEquip monstro))
                  battle hdl principal monstro (turno+1)
        "s" -> 
          do
            case (getRole principal) of
              "Mage" -> 
                do
                  hPutStrLn hdl ("You deal: "++ show(7+(getEquipAtribute(getEquip principal))) ++" damage and got: -"++show(getAtk monstro) ++" hp")
                  principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)-(getAtk monstro)) (getEquip principal))
                  monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) ((getHp monstro) - (6+(getEquipAtribute (getEquip principal)))) (getEquip monstro))
                  battle hdl principal monstro (turno+1)
              "Warrior" -> 
                do
                  hPutStrLn hdl ("You deal: "++show(3) ++" damage and got: -"++show(0) ++" hp")
                  principal <- turnPerIO  (Per(getName principal)(getRole principal)(getAtk principal)((getHp principal)+3) (getEquip principal))
                  monstro <- turnPerIO    (Per(getName monstro) (getRole monstro) (getAtk monstro) (getHp monstro) (getEquip monstro))
                  battle hdl principal monstro (turno+1)
              _ ->
                do
                  hPutStrLn hdl "\tYou can't do that"
                  battle hdl principal monstro turno
        "r" -> 
          do
            hPutStrLn hdl "\tYou ran away from battle chicken, also, the monster will still be there"
            return "Run"
        _  ->
          do 
            hPutStrLn hdl "\tForeign command, please?"
            battle hdl principal monstro turno

printChar::Handle->Personagem->Personagem->IO ()
printChar hdl personagem monstro =
  do
    hPutStrLn hdl ("\t| "++ show(getName personagem) ++"\t\t |\t\t\t| "++ show(getName monstro)++ "\t |\n\t| \
    \Health: " ++ show(getHp personagem) ++"\t\t |\t\t\t| Health: "++ show(getHp monstro) ++ "\t |\n\t| \
    \Attack: " ++ show(getAtk personagem) ++"\t\t |\t\t\t| Attack: "++ show(getAtk monstro) ++ "\t \n\t| \
    \Equip: " ++ show(getEquipName (getEquip personagem)) ++"|\t\t\t| Equip: "++ show(getEquipName (getEquip monstro)) ++ "\t |")
    
printMap::Handle->Int->Int->IO ()
printMap hdl num state = 
  do
    case num of 
      1 -> 
        do
          case state of 
            0 ->
              do
                hPutStrLn hdl ("\tYou find yourself in a room, where the only thing you can do is go north, though there is\
                \ a goblin blocking the door, you will have to fight him to go through")
                hPutStrLn hdl ("\t------| |------\n\
                               \\t|      #      |\n\
                               \\t|             |\n\
                               \\t|      *      |\n\
                               \\t|             |\n\
                               \\t---------------")     
                threadDelay 2000000 --Delay de 2 segundos
                               
            1 ->
              do
                hPutStrLn hdl ("\tSince you have won the battle, the way isn's blocked anymore, you can freely go through\
                \ the north door")
                hPutStrLn hdl ("\t------| |------\n\
                               \\t|             |\n\
                               \\t|             |\n\
                               \\t|      *      |\n\
                               \\t|             |\n\
                               \\t---------------")
                threadDelay 2000000 --Delay de 2 segundos         
      2 ->
        do
          case state of 
            0 ->
              do
                hPutStrLn hdl ("\tYou entered the second room, at your left is an equipment, at your right, theres a big\
                \ spider, drooling to eat some new flash")
                hPutStrLn hdl ("\t---------------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t|I     *     # \n\
                              \\t|             |\n\
                              \\t------| |------")       
                threadDelay 2000000 --Delay de 2 segundos   
            1 ->
              do
                hPutStrLn hdl ("\tThat was a nice item, although there's still a starving spider to fight")
                hPutStrLn hdl ("\t---------------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t|      *     # \n\
                              \\t|             |\n\
                              \\t------| |------")       
                threadDelay 2000000 --Delay de 2 segundos   
            2 ->
              do
                hPutStrLn hdl ("\tYou beat the spider, not so though now han. Before leaving the room, you look back\
                \and see the item there")
                hPutStrLn hdl ("\t---------------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t|I     *       \n\
                              \\t|             |\n\
                              \\t------| |------")       
                threadDelay 2000000 --Delay de 2 segundos   
            3 ->
              do
                hPutStrLn hdl ("\tAfter beating the spider and looking the item, you start looking to the\
                \ ceiling, watching some webs and dead things caught on the spider's trap, also, the door is unblocked")
                hPutStrLn hdl ("\t---------------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t|      *       \n\
                              \\t|             |\n\
                              \\t------| |------")          
                threadDelay 2000000 --Delay de 2 segundos
      3 ->
            do
              hPutStrLn hdl ("\tYou entered a big and empty room, will there be traps? There are 2 doors unlocked\
              \ up and right")
              hPutStrLn hdl ("\t------| |------\n\
                             \\t|             |\n\
                             \\t|             |\n\
                             \\t       *       \n\
                             \\t|             |\n\
                             \\t---------------") 
              threadDelay 2000000 --Delay de 2 segundos
      4->
        do
          case state of
            0->
              do
                hPutStrLn hdl ("\tThere's a sarcophagus on the end of the room, you see it trembling and \
                \ know that there must have an enemy, though, it might been guarding a rare item")
                hPutStrLn hdl ("\t---------------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t      *     # |\n\
                              \\t|             |\n\
                              \\t---------------") 
                threadDelay 2000000 --Delay de 2 segundos
            1->
              do
                hPutStrLn hdl ("\tSo, by defeating the mummy you see that there was an item inside the sarcophagus\
                \ and look that every room is completely different, yet, you must get back to end that dungeon")
                hPutStrLn hdl ("\t---------------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t      *       |\n\
                              \\t|             |\n\
                              \\t---------------") 
                threadDelay 2000000 --Delay de 2 segundos
      5->
        do
          case state of
            0->
              do
                hPutStrLn hdl ("\tSo, you got on an very dark room, you can sense something on your right, only a presence\
                \ at your left you see some trashy things on the ground and at your front, a door emaning light")
                hPutStrLn hdl ("\t------| |------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t #     *     # \n\
                              \\t|             |\n\
                              \\t------| |------") 
                threadDelay 2000000 --Delay de 2 segundos
            1->
              do
                hPutStrLn hdl ("\tAfter defeating the skelleton, you unlocked the left door, yet the presence is still there\
                \ on the left side")
                hPutStrLn hdl ("\t------| |------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t       *     # \n\
                              \\t|             |\n\
                              \\t------| |------") 
                threadDelay 2000000 --Delay de 2 segundos
            2->
              do
                hPutStrLn hdl ("\tThat enemy was a tough to see and believe in, although the worst has gone\
                \, after thinking that, you realize that there's somenthing about that trash, and the right door unlocked")
                hPutStrLn hdl ("\t------| |------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t #     *       \n\
                              \\t|             |\n\
                              \\t------| |------") 
                threadDelay 2000000 --Delay de 2 segundos
            3->
              do
                hPutStrLn hdl ("\tAfter defeating the ghost and the skelleton, the room got brighter and not so scary\
                \ until you see other lights floating, so, you remember to leave there imediately")
                hPutStrLn hdl ("\t------| |------\n\
                               \\t|             |\n\
                               \\t|             |\n\
                               \\t       *       \n\
                               \\t|             |\n\
                               \\t------| |------")
                threadDelay 2000000 --Delay de 2 segundos
      6->
        do
          case state of
            0->
              do
                hPutStrLn hdl ("\tThe room is full of snakes and lots of stones, at the very end is an hole, it might be some \
                \feracious monster's hideout, even you seeing that everything is a stone, you think that it won't worth\
                \ fighting, also you think at it")
                hPutStrLn hdl ("\t---------------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t       *     #|\n\
                              \\t|             |\n\
                              \\t---------------") 
                threadDelay 2000000 --Delay de 2 segundos
            1->
              do
                hPutStrLn hdl ("\tThis tough fight doesn't pay the price, cause everything is now rocks")
                hPutStrLn hdl ("\t---------------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t       *      |\n\
                              \\t|             |\n\
                              \\t---------------") 
                threadDelay 2000000 --Delay de 2 segundos
      7->
        do
          case state of
            0->
              do
                hPutStrLn hdl ("\tYou see an old man brabing on the side of the room, writing on the walls and\
                \ mumbling to himself.. In the wall is written some magical words, he looks at you, aware but\
                \ not hostile, for now")
                hPutStrLn hdl ("\t------| |------\n\
                              \\t|             |\n\
                              \\t|             |\n\
                              \\t|#     *       \n\
                              \\t|             |\n\
                              \\t---------------") 
                threadDelay 2000000 --Delay de 2 segundos
            1->
              do
                hPutStrLn hdl ("\tA mage and a wand, that was hard to get, you understand now some of the words, but you\
                \ must not stop")
                hPutStrLn hdl ("\t------| |------\n\
                               \\t|             |\n\
                               \\t|             |\n\
                               \\t|      *       \n\
                               \\t|             |\n\
                               \\t---------------")
                threadDelay 2000000 --Delay de 2 segundos
      8->
        do
          case state of
            0->
              do
                hPutStrLn hdl ("\tYou see a treasure box, a little weird, but you notice that it's details are quite\
                \ beautiful and shiny as a diamond, inside might have a really rare and good item")
                hPutStrLn hdl ("\t---------------\n\
                               \\t|      i      |\n\
                               \\t|             |\n\
                               \\t|      *       \n\
                               \\t|             |\n\
                               \\t------| |------") 
                threadDelay 2000000 --Delay de 2 segundos
            1->
              do
                hPutStrLn hdl ("\tWell, you could'nt imagine it was a real mimic afterall, but the efforts are worthy\
                \ that item is quite strong")
                hPutStrLn hdl ("\t---------------\n\
                               \\t|             |\n\
                               \\t|             |\n\
                               \\t|      *       \n\
                               \\t|             |\n\
                               \\t------| |------") 
                threadDelay 2000000 --Delay de 2 segundos
      9->
        do
          hPutStrLn hdl ("\tYou see a big room, with a big door on it's right side, might it be the end of the dungeon?\
                \ better be prepared")
          hPutStrLn hdl ("\t---------------\n\
                         \\t|             |\n\
                         \\t|             |\n\
                         \\t      *        \n\
                         \\t|             |\n\
                         \\t-----| |-------") 
          threadDelay 2000000 --Delay de 2 segundos
      10-> 
        do
          hPutStrLn hdl ("\tBy now you can see and feel the presence of the boss, a full necromancer with high damage and\
                \ life, will you be able to survive?")
          hPutStrLn hdl ("\t---------------\n\
                         \\t|             |\n\
                         \\t|             |\n\
                         \\t      *     B  \n\
                         \\t|             |\n\
                         \\t---------------")
          threadDelay 2000000 --Delay de 2 segundos
      _ -> 
        do
          hPutStrLn hdl ("\tCan't do that")

aquireEquip::Handle->Personagem->Equip->IO Personagem
aquireEquip hdl character eqp = 
  do
    hPutStrLn hdl ("\tDo you want the: " ++ show(getEquipName eqp) ++"\t\n"++ show(getEquipRole eqp) ++ ": +" ++ show(getEquipAtribute eqp)++"(y/n)")
    choice <- fmap init (hGetLine hdl)
    if choice == "y" || choice == "yes"
      then
        case (getEquipRole eqp) of
          Atk -> 
            do
              character<-  turnPerIO(Per(getName character)(getRole character)(getRoleAtk (getRole character) + (getEquipAtribute eqp)) (getRoleHp (getRole character)) eqp)
              printCharSolo hdl character
              turnPerIO (Per(getName character)(getRole character)(getRoleAtk (getRole character) + (getEquipAtribute eqp)) (getRoleHp (getRole character)) eqp)
              
          _ ->
            do
              character<-  turnPerIO(Per(getName character)(getRole character)(getRoleAtk (getRole character) + (getEquipAtribute eqp)) (getRoleHp (getRole character)) eqp)
              printCharSolo hdl character;
              turnPerIO (Per(getName character) (getRole character) (getRoleAtk (getRole character)) (getRoleHp (getRole character)  + (getEquipAtribute eqp)) eqp)
      else
        do
          character<-  turnPerIO(Per(getName character)(getRole character)(getRoleAtk (getRole character) + (getEquipAtribute eqp)) (getRoleHp (getRole character)) eqp)
          printCharSolo hdl character;
          turnPerIO (Per(getName character) (getRole character) (getAtk character) (getHp character) (getEquip character))

printCharSolo::Handle->Personagem->IO ()
printCharSolo hdl personagem =
  do
    hPutStrLn hdl ("\t| "++ show(getName personagem) ++"|\n\t| \
    \Health: " ++ show(getHp personagem) ++"|\n\t| \
    \Attack: " ++ show(getAtk personagem) ++"\n\t| \
    \Equip: " ++ show(getEquipName (getEquip personagem)) ++"\t |")
    