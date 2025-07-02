module Alunos (Aluno(..), showmenu, cadastrar, imprimir) where
import System.IO 
import Dados
import Util
--Insira na classe Dado a definição da função imprimir e implemente uma versão
--para Aluno, Livro e Empréstimo nos respectivos módulos.
--Essa função recebe um valor de um tipo pertencente à classe Dado e 
-- imprime na tela todos os dados desse valor. Repare que a função já faz a impressão na tela 
-- ao invés de retornar um String. A impressão dos atributos e valores dos dados devem estar formatados 
-- com base na função formata.

--Questão 05 [1,0]. Insira na classe Dado a definição da função cadastrar e implemente uma versão
--para Aluno, Livro e Empréstimo nos respectivos módulos. Essa função que solicita do usuário os
--dados do respectivo tipo e grava em um arquivo (cada tipo tem seu próprio arquivo)

-- Questão 09 [1,0]. Insira na classe Dado a definição da função showmenu e implemente uma
-- versão para Aluno, Livro e Empréstimo nos respectivos módulos. Essa função imprime na tela um
-- menu com as opções voltar, visualizar, cadastrar e apagar e solicita que o usuário digite uma
-- opção, retornando o valor digitado.

data Aluno = Aluno Codigo Nome Email deriving (Show, Read)
data Codigo = Codigo Int deriving (Show, Read)
data Nome = Nome String deriving (Show, Read)
data Email = Email String deriving (Show, Read)

instance Dado Aluno where
  imprimir (Aluno (Codigo x) (Nome n) (Email e)) = do 
    putStrLn (formata "Código" (show x))
    putStrLn (formata "Nome" n)
    putStrLn (formata "Email" e)

  cadastrar _ = do 
    putStrLn "Digite o codigo do Aluno: "
    cod <- readLn
    putStrLn "Digite o nome do Aluno: "
    nome <- getLine
    putStrLn "Digite o email do Aluno: "
    email <- getLine

    let aluno = (Aluno (Codigo cod) (Nome nome) (Email email))
    arq <- openFile "Aluno.txt" AppendMode
    hPutStrLn arq (show aluno)
    hClose arq
    putStrLn "Aluno cadastrado com sucesso!"
    return aluno

  showmenu _ = do
    putStrLn "Digite 1-Voltar 2-Visualizar 3-Cadastrar 4-Apagar"
    readLn
