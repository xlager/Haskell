-- :l justificadorHaskell.hs
--justifica texto (nroEspacos  texto) (calculaEspacos texto (listaTamanhos texto) (maiorLista (listaTamanhos texto)))
--Função de Justifcar
justifica:: [String]->[Int]->[Int]-> IO()
justifica [] a z = return()
justifica a [] z = return()
justifica a z [] = return()
justifica (a:xs) (b:xb) (z:zx)
  |xs == [] = putStr (a)
  |z==0 =do {putStr (substituiEspaco a 0 0); justifica xs xb zx}
  |otherwise = do {putStr (substituiEspaco a z (fromIntegral (z)/fromIntegral (b)));justifica xs xb zx}

--Separa string e char e etc - a = linha z = numero de espaços  x = qnts por linha é pra add
substituiEspaco::String->Int->Float->String
substituiEspaco [] a x = []
substituiEspaco (a:xs) z x
  |z == 0 = a:substituiEspaco xs z x
  |a == ' ' && x < 1 =a:adicionaEspaco 1 ' '++substituiEspaco xs (z-1) x
  |a == ' ' && x - fromIntegral (floor (x)) <= 0.5 =a:adicionaEspaco (floor x) ' '++ substituiEspaco xs z (x+0.05)
  |a == ' ' && x - fromIntegral (floor (x)) > 0.5 =a:adicionaEspaco (ceiling x) ' '++ substituiEspaco xs z (x-0.05)
  |otherwise = a:substituiEspaco xs z x

adicionaEspaco :: Int -> Char -> String
adicionaEspaco 0 xs = []
adicionaEspaco a xs
   |a<1 = []
   |otherwise = xs:adicionaEspaco (a-1) xs

-- Calcula o numero de espaços da frase
nroEspacos::[String]->[Int]
nroEspacos [] = []
nroEspacos (a:xs) = length(words(a))-1 : nroEspacos xs

-- Numero de espacos de cada frase
calculaEspacos::[String]->[Int]->Int->[Int]
calculaEspacos [] size bigger = []
calculaEspacos text [] bigger = []
calculaEspacos (te:xt) (si:ze) bigger =fromIntegral bigger-si : calculaEspacos xt ze bigger

--Calcula o tamanho da linha
tamanhoLinha::String->Int
tamanhoLinha [] = 0
tamanhoLinha (a:xs)
   | a == '\n' = 0
   | otherwise = 1 + tamanhoLinha xs

--Através de recursão, Pega o tamanho de cada Linha
listaTamanhos :: [String]->[Int]
listaTamanhos [] = []
listaTamanhos ((a):xs) = tamanhoLinha a : listaTamanhos xs

--Função simples que pega o maior valor de uma lista
maiorLista::[Int]->Int
maiorLista [a] = a
maiorLista (a:xs) = max a (maiorLista xs)

--Separa linhas - atualmente inútil
separaLinha::String->[String]
separaLinha " "= []  --esse a faz ser uma string qlqr
separaLinha a = dropWhile(< '\n') a : []

--Lista com todas as linhas
texto = [linha1,linha2,linha3,linha4,linha5,linha6,linha7,linha8,linha9]

linha1 = "RUBIÃO fitava a enseada eram oito horas da manhã.\n"
linha2 = "Quem o visse com os polegares metidos no cordão do chambre à janela de uma\n"
linha3 = "grande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\n"
linha4 = "quieta mas em verdade vos digo que pensava em outra coisa.\n"
linha5 = "Cotejava o passado com o presente. Que era há um ano?\n"
linha6 = "Professor. Que é agora? Capitalista! Olha para si para as chinelas\n"
linha7 = "(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\n"
linha8 = "para o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\n"
linha9 = "até o céu tudo entra na mesma sensação de propriedade.\n"
