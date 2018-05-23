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

--Função para virar a que escreve no fim
tu:: [String] -> IO()
tu [] = return()
tu ((a):xs) = do {putStr a;adicionaPeca 3 " "; tu xs}


--Adiciona a string o numero de vezes
adicionaPeca :: Int -> String -> IO ()
adicionaPeca n xs = sequence_ [putStr xs| i<- [1..n]]


--Função simples que pega o maior valor de uma lista
maiorLista::[Int]->Int
maiorLista [a] = a
maiorLista (a:xs) = max a (maiorLista xs)

--Através de recursão, Pega o tamanho de cada Linha
listaTamanhos :: [String]->[Int]
listaTamanhos [] = []
listaTamanhos ((a):xs) = tamanhoLinha a : listaTamanhos xs

--Pegar os espaços agora
calculaEspacos::[String]->[Int]-> Int ->[Int]
calculaEspacos [] size bigger = []
calculaEspacos text [] bigger = []
--calculaEspacos te:xt si:ze bigger = []


--Lista com todas as linhas
texto = [linha1,linha2,linha3,linha4,linha5,linha6,linha7,linha8,linha9]

linha1 = "RUBIÃO fitava a enseada eram oito horas da manhã.\n"
linha2 = " Quem o visse com os polegares metidos no cordão do chambre à janela de uma\n"
linha3 = " grande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\n"
linha4 = " quieta mas em verdade vos digo que pensava em outra coisa.\n"
linha5 = " Cotejava o passado com o presente. Que era há um ano?\n"
linha6 = " Professor. Que é agora? Capitalista! Olha para si para as chinelas\n"
linha7 = " (umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\n"
linha8 = " - para o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\n"
linha9 = " até o céu tudo entra na mesma sensação de propriedade."
