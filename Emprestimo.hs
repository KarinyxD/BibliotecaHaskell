module Emprestimo(Emprestimo(..), showmenu, cadastrar, imprimir, obter, buscar, apagar) where
import System.IO
import Alunos
import Dados
import Util
import Livros

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
        cod <- readLn
        alunoEncontrado <- Alunos.buscar cod (undefined :: Aluno)
        case alunoEncontrado of
          Nothing -> putStrLn "Aluno não encontrado!"
          Just aluno -> do
            putStrLn "Digite o dia do Emprestimo: " 
            diaEmp <- readLn
            putStrLn "Digite o mes do Emprestimo: " 
            mesEmp <- readLn
            putStrLn "Digite o ano do Emprestimo: " 
            anoEmp <- readLn
            let dataEmp = Data diaEmp mesEmp anoEmp

            putStrLn "Digite o dia da Devolucao do Emprestimo: " 
            diaDev <- readLn
            putStrLn "Digite o mes da Devolucao do Emprestimo: " 
            mesDev <- readLn
            putStrLn "Digite o ano da Devolucao do Emprestimo: " 
            anoDev <- readLn
            let dataDev = Data diaDev mesDev anoDev

            putStrLn "Digite a quantidade de livros para o Emprestimo: "
            qtd <- readLn
            livrosEmprestimo <- loop [] qtd
            let emprestimo = (Emprestimo (Numero num) (aluno) (dataEmp) (dataDev) (livrosEmprestimo))
            arq <- openFile "Emprestimo.txt" AppendMode
            hPutStrLn arq (show emprestimo)
            hClose arq
            putStrLn "Emprestimo cadastrado com sucesso!"
            imprimir emprestimo
            where 
              loop xs 0 = return xs
              loop xs n = do
                putStrLn ("Digite o registro do livro " ++ show n ++ " para o Emprestimo: ")
                reg <- readLn
                livroEncontrado <- Livros.buscar reg (undefined :: Livro)
                case livroEncontrado of
                  Nothing -> do 
                    putStrLn "Livro não encontrado!"
                    loop xs n
                  Just livro -> loop (livro : xs) (n-1)

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