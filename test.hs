-- :l init.hs

separaLinha::String->[String]
separaLinha " "=[]  --esse a faz ser uma string qlqr
separaLinha a = takeWhile(> '\n') a : []


tamanhoLinha::[String]->Int
tamanhoLinha [] = 0
tamanhoLinha ((a:x):xs)
   | a == '\n' = 0
   | otherwise = 1 + tamanhoLinha (x:xs)

--separaPalavras :: String -> [String] a mesma coisa que word String -> [String]
--lenght(words(string)) = n de palavras na linha
teste::String->String
teste a = putStr(words (a))

-- recebe uma string e o tamanhoMaiorLinha, conta qual o tamanho da propria - maior e add espaços entre cada palavra 
-- separaLinha, lenght(words(separaLinha))-1, valores pra divisao tamanhoML - tamanhoLinha = n de espaços. n de espacos / lenght-1 = X de espaços
-- reescrever a cada espaço que achar, add o X de espaços