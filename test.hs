-- :l init.hs
import Control.Monad (forM_)
import Control.Monad (mapM_)

separaLinha::String->[String]
separaLinha " "= []  --esse a faz ser uma string qlqr
separaLinha a = dropWhile(< '\n') a : []


tamanhoLinha::[String]->Int
tamanhoLinha [] = 0
tamanhoLinha ((a:x):xs)
   | a == '\n' = 0
   | otherwise = 1 + tamanhoLinha (x:xs)

--separaPalavras :: String -> [String] a mesma coisa que word String -> [String]
--lenght(words(string)) = n de palavras na linha

--putStr imprime
-- recebe uma string e o tamanhoMaiorLinha, conta qual o tamanho da propria - maior e add espaços entre cada palavra
-- separaLinha, lenght(words(separaLinha))-1, valores pra divisao tamanhoML - tamanhoLinha = n de espaços. n de espacos / lenght-1 = X de espaços
-- reescrever a cada espaço que achar, add o X de espaços

--teste::[String] -> String
--teste [] = " "
--teste ((a):xs) = putStr (a)

tt:: [String] -> String
tt [] = ""
tt ((a):xs) = foldr (++) [] [a] ++" "++ tt xs

tu:: [String] -> IO()
tu [] = return()
tu ((a):xs) = do {putStr a;adicionaPeca 3 " "; tu xs}

adicionaPeca :: Integer -> String -> IO ()
adicionaPeca n xs = sequence_ [putStr xs| i<- [1..n]]

transformaLinhaLista:: Int->Int->Int->Int->Int->Int->Int->Int->Int->[Int]
transformaLinhaLista a b c d e f g h i = a : b : c : d : e : f : g : h : i :[]

maiorLista::[Int]->Int
maiorLista [a] = a
maiorLista (a:xs) = max a (maiorLista xs)

listaTamanhos = transformaLinhaLista 1 2 3 4 5 6 7 8 9
maior = maiorLista listaTamanhos
