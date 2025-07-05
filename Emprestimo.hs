module Emprestimo(Emprestimo(..), showmenu, cadastrar, imprimir, obter, buscar, apagar) where
import System.IO
import Alunos
import Dados
import Util
import Livros
import Distribution.Compat.Prelude (undefined)

data Emprestimo = Emprestimo Numero Aluno Data Data [Livro] deriving (Show, Read, Eq)
data Numero = Numero Int deriving (Show, Read, Eq)

instance Dado Emprestimo where
  imprimir (Emprestimo (Numero x) aluno dataEmp dataDev livros) = do 
    putStrLn (formata "Numero" (show x))
    imprimir aluno
    putStrLn (formata "Data de Emprestimo:" (dataStr dataEmp))
    putStrLn (formata "Data de Devolucao:" (dataStr dataDev))
    imprimeLivros livros 
      where
        imprimeLivros [] = return ()
        imprimeLivros (l:ls) = do
          imprimir l
          imprimeLivros ls

  cadastrar _ = do 
    putStrLn "Digite o numero do Emprestimo: "
    num <- readLn
    emprestimoExistente <- buscar num (undefined :: Emprestimo)
    case emprestimoExistente of
      Just _ -> putStrLn "Já existe um emprestimo com esse número! Cadastro cancelado."
      Nothing -> do
        putStrLn "Digite o codigo do Aluno para o Emprestimo: "
        --cod <- readLn
        putStrLn "Digite a data do Emprestimo: "
        dataEmp <- getLine
        putStrLn "Digite a data de devolucao do Emprestimo: "
        dataDev <- getLine
        putStrLn "Digite a quantidade de livros para o Emprestimo: "
        --qtd <- readLn
        putStrLn "Digite o registro do livro para o Emprestimo: "
        -- reg <- readLn

        -- let emprestimo = (Emprestimo (Numero num) (undefined :: Aluno) (Data dataEmp) (Data dataDev) (undefined :: Livro))
        arq <- openFile "Emprestimo.txt" AppendMode
        -- hPutStrLn arq (show emprestimo)
        hClose arq
        putStrLn "Emprestimo cadastrado com sucesso!"
        -- imprimir emprestimo

  showmenu _ = do
    putStrLn "Digite 1-Voltar 2-Visualizar 3-Cadastrar 4-Apagar"
    readLn

  obter _ = do
    arq <- openFile "Emprestimo.txt" ReadMode
    let loop list = do
          eof <- hIsEOF arq
          if eof then return list
          else do
            linharq <- hGetLine arq
            let emprestimo = read linharq :: Emprestimo
            loop (inserir emprestimo list)
    listaEmprestimos <- loop (S [])
    hClose arq
    return listaEmprestimos

  buscar numero _ = do 
    S emprestimos <- obter (undefined :: Emprestimo) 
    return (buscarNumero numero emprestimos)
    where
      getNumero (Emprestimo (Numero n) _ _ _ _) = n
      buscarNumero _ [] = Nothing
      buscarNumero numero (e:es)
        | (getNumero e) == numero = Just e
        | otherwise = buscarNumero numero es

  apagar numero _ = do 
    emprestimoEncontrado <- buscar numero (undefined :: Emprestimo)
    case emprestimoEncontrado of
      Nothing -> error "Número de emprestimo não encontrado."
      Just emprestimo -> do
        S emprestimos <- obter (undefined :: Emprestimo)
        let S emprestimoRemovido = remover emprestimo (S emprestimos)
        arq <- openFile "Emprestimo.txt" WriteMode
        salvaEmprestimos arq emprestimoRemovido
        hClose arq
        return emprestimo
        where
          salvaEmprestimos _ [] = return ()
          salvaEmprestimos arq (e:es) = do
            hPutStrLn arq (show e)
            salvaEmprestimos arq es