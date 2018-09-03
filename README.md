# Haskell
Repositório designado para os trabalhos da cadeira de Programação Funcional em linguagem Haskell

## Trabalho 1 - Implementação de um programa que justifica textos
O objetivo do trabalho é implementar a função

justifica: que recebe como entrada uma string contendo um texto em várias linhas e devolve o mesmo texto justificado pela maior linha. 

justifica :: String -> String  
Exemplo de texto:
```
>"RUBIÃO fitava a enseada eram oito horas da manhã.\n   
> Quem o visse com os polegares metidos no cordão do chambre à janela de uma\n  
> grande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\n  
> quieta mas em verdade vos digo que pensava em outra coisa.\n  
> Cotejava o passado com o presente. Que era há um ano?\n  
> Professor. Que é agora? Capitalista! Olha para si para as chinelas\n  
> (umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\n  
> para o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\n  
> até o céu tudo entra na-- mesma sensação de propriedade."  
```
Exemplo de uso:  
Prelude> putStr (justifica texto)
```
>"RUBIÃO	fitava     a     enseada     eram     oito    horas   da      manhã.  
> Quem  o  visse  com  os  polegares  metidos  no cordão do chambre à janela de uma  
> grande  casa  de  Botafogo  cuidaria  que  ele  admirava  aquele  pedaço  de água  
> quieta    mas    em    verdade    vos    digo   que   pensava   em   outra coisa.  
> Cotejava     o    passado     com   o   presente.    Que    era   há   um    ano?  
> Professor.   Que   é   agora?   Capitalista!   Olha  para  si  para  as  chinelas  
> (umas  chinelas  de  Túnis que lhe deu recente amigo Cristiano Palha) para a casa  
> para o jardim para a enseada para os morros e para o céu e tudo desde as chinelas  
> até o céu tudo entra na mesma sensação de propriedade."  
```
## Trabalho Final - RPG de texto interativo
O objetivo do trabalho é a implementação de um código servidor para jogar um RPG de texto
foi desenvolvido utilizando código servidor em haskell, com o network-simple e a conexão
 do cliente via Telnet
