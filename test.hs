-- :l init.hs
import Control.Monad (forM_)
import Control.Monad (mapM_)
--separaPalavras :: String -> [String] a mesma coisa que word String -> [String]
--lenght(words(string)) = n de palavras na linha

--putStr imprime
-- recebe uma string e o tamanhoMaiorLinha, conta qual o tamanho da propria - maior e add espaços entre cada palavra
-- separaLinha, lenght(words(separaLinha))-1, valores pra divisao tamanhoML - tamanhoLinha = n de espaços. n de espacos / lenght-1 = X de espaços
-- reescrever a cada espaço que achar, add o X de espaços

--teste::[String] -> String
--teste [] = " "
--teste ((a):xs) = putStr (a)

--Separa linhas - atualmente inútil
separaLinha::String->[String]
separaLinha " "= []  --esse a faz ser uma string qlqr
separaLinha a = dropWhile(< '\n') a : []

--Calcula o tamanho da linha
tamanhoLinha::String->Int
tamanhoLinha [] = 0
tamanhoLinha (a:xs)
   | a == '\n' = 0
   | otherwise = 1 + tamanhoLinha xs



--Função simples que pega o maior valor de uma lista
maiorLista::[Int]->Int
maiorLista [a] = a
maiorLista (a:xs) = max a (maiorLista xs)

--Através de recursão, Pega o tamanho de cada Linha
listaTamanhos :: [String]->[Int]
listaTamanhos [] = []
listaTamanhos ((a):xs) = tamanhoLinha a : listaTamanhos xs

--Pegar os espaços agora
calculaEspacos::[String]->[Int]->Int->[Int]
calculaEspacos [] size bigger = []
calculaEspacos text [] bigger = []
calculaEspacos (te:xt) (si:ze) bigger =fromIntegral bigger-si : calculaEspacos xt ze bigger

-- função que calcula quantos espaços adicionar de vdd
-- /fromIntegral(length(words (te))-1)
nroEspacos::[String]->[Int]
nroEspacos [] = []
nroEspacos (a:xs) = length(words(a))-1 : nroEspacos xs

--agora é só juntar tudo, função que pega a lista de qnts espaços add e ir add

--Função para virar a que escreve no fim
-- texto calculaEspacos palavras
adicionaEspaco:: [String]->[Int]->[Int]-> IO()
adicionaEspaco [] a z = return()
adicionaEspaco a [] z = return()
adicionaEspaco a z [] = return()
adicionaEspaco (a:xs) (b:xb) (z:zx)
  |xs == [] = putStr (a)
  |z==0 =do {putStr (printaLinha a 0 0); adicionaEspaco xs xb zx}
  |otherwise = do {putStr (printaLinha a z (fromIntegral (z)/fromIntegral (b)));adicionaEspaco xs xb zx}

--Separa string e char e etc - a = linha z = numero de espaços  x = qnts por linha é pra add
printaLinha::String->Int->Float->String
printaLinha [] a x = []
printaLinha (a:xs) z x
  |z == 0 = a:printaLinha xs z x
  |a == ' ' && x < 1 =a:adici 1 ' '++printaLinha xs (z-1) x
  |a == ' ' && x - fromIntegral (floor (x)) <= 0.5 =a:adici (floor x) ' '++ printaLinha xs z (x+0.05)
  |a == ' ' && x - fromIntegral (floor (x)) > 0.5 =a:adici (ceiling x) ' '++ printaLinha xs z (x-0.05)
--  |a == ' ' =a:adici (floor x)' '++printaLinha xs (z-(ceiling x)) x
  |otherwise = a:printaLinha xs z x

adici :: Int -> Char -> String
adici 0 xs = []
adici a xs
   |a<1 = []
   |otherwise = xs:adici (a-1) xs

--adicionaEspaco texto (nroEspacos  texto) (calculaEspacos texto (listaTamanhos texto) (maiorLista (listaTamanhos texto)))
-- calculaEspacos texto (listaTamanhos texto) (maiorLista (listaTamanhos texto)) <- Lista de espaços a serem adicionados
-- palavras = número de espaços



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
