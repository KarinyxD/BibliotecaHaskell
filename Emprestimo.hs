module Emprestimo(Emprestimo(..), showmenu, cadastrar, imprimir) where
import System.IO
import Alunos
import Dados
import Util
import Livros

data Emprestimo = Emprestimo Numero Aluno Data Data [Livro] deriving (Show, Read)
data Numero = Numero Int deriving (Show, Read)

instance Dado Emprestimo where
  imprimir (Emprestimo (Numero x) aluno dataEmp dataDev livros) = do 
    putStrLn (formata "Numero" (show x))
    imprimir aluno
    putStrLn (formata "Data de Emprestimo:" (dataStr dataEmp))
    putStrLn (formata "Data de Devolucao:" (dataStr dataDev))
    mapM_ imprimir livros
  
  cadastrar _ = do 
    putStrLn "Digite o numero do Emprestimo: "
    -- num <- readLn
    putStrLn "Digite a data do Emprestimo: "
    dataEmp <- getLine
    putStrLn "Digite a data de devolucao do Emprestimo: "
    dataDev <- getLine

    -- let emprestimo = (Emprestimo (Numero num) (undefined :: Aluno) (Data dataEmp) (Data dataDev) (undefined :: Livro))
    arq <- openFile "Emprestimo.txt" AppendMode
    -- hPutStrLn arq (show emprestimo)
    hClose arq
    putStrLn "Emprestimo cadastrado com sucesso!"
    return (undefined :: Emprestimo)

  showmenu _ = do
    putStrLn "Digite 1-Voltar 2-Visualizar 3-Cadastrar 4-Apagar"
    readLn


