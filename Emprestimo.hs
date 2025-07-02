module Emprestimo where
import System.IO
import Alunos
import Dados
import Util
import Livros
--Insira na classe Dado a definição da função imprimir e implemente uma versão
--para Aluno, Livro e Empréstimo nos respectivos módulos.
--Essa função recebe um valor de um tipo pertencente à classe Dado e 
-- imprime na tela todos os dados desse valor. Repare que a função já faz a impressão na tela 
-- ao invés de retornar um String. A impressão dos atributos e valores dos dados devem estar formatados 
-- com base na função formata.
data Emprestimo = Emprestimo Numero Aluno Data Data [Livro] deriving (Show, Read)
data Numero = Numero Int deriving (Show, Read)

instance Dado Emprestimo where
  imprimir (Emprestimo (Numero x) aluno dataEmp dataDev livros) = do 
    putStrLn (formata "Numero" (show x))
    imprimir aluno
    putStrLn (formata "Data de Emprestimo:" (dataStr dataEmp))
    putStrLn (formata "Data de Devolucao:" (dataStr dataDev))
    mapM_ imprimir livros
  
  cadastrar emprestimo = do 
    arq <- openFile "Emprestimo.txt" AppendMode
    hPutStrLn arq (show emprestimo)
    hClose arq

