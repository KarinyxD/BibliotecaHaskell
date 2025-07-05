module Alunos (Aluno(..), imprimir, showmenu, cadastrar, obter, buscar, apagar) where
import System.IO 
import Dados
import Util
import Dados 

data Aluno = Aluno Codigo Nome Email deriving (Show, Read, Eq)
data Codigo = Codigo Int deriving (Show, Read, Eq)
data Nome = Nome String deriving (Show, Read, Eq)
data Email = Email String deriving (Show, Read, Eq)

instance Dado Aluno where
  imprimir (Aluno (Codigo x) (Nome n) (Email e)) = do 
    putStrLn (formata "Código" (show x))
    putStrLn (formata "Nome" n)
    putStrLn (formata "Email" e)

  cadastrar _ = do 
    putStrLn "Digite o codigo do Aluno: "
    cod <- readLn
    alunoExistente <- buscar cod (undefined :: Aluno)
    case alunoExistente of
      Just _ -> putStrLn "Já existe um aluno com esse código! Cadastro cancelado."
      Nothing -> do
        putStrLn "Digite o nome do Aluno: "
        nome <- getLine
        putStrLn "Digite o email do Aluno: "
        email <- getLine
        let aluno = (Aluno (Codigo cod) (Nome nome) (Email email))
        arq <- openFile "Aluno.txt" AppendMode
        hPutStrLn arq (show aluno)
        hClose arq
        putStrLn "Aluno cadastrado com sucesso!"
        imprimir aluno 

  showmenu _ = do
    putStrLn "Digite 1-Voltar 2-Visualizar 3-Cadastrar 4-Apagar"
    readLn

  obter _ = do
    arq <- openFile "Aluno.txt" ReadMode
    let loop list = do
          eof <- hIsEOF arq
          if eof then return list
          else do
            linharq <- hGetLine arq
            let aluno = read linharq :: Aluno
            loop (inserir aluno list)
    listaAlunos <- loop (S [])
    hClose arq
    return listaAlunos
-- s <- (obter :: IO (Set Aluno)

  buscar codigo _ = do 
    S alunos <- obter (undefined :: Aluno) 
    return (buscarCodigo codigo alunos)
    where
      getCodigo (Aluno (Codigo x) _ _) = x
      buscarCodigo _ [] = Nothing
      buscarCodigo codigo (a:as)
        | (getCodigo a) == codigo = Just a
        | otherwise = buscarCodigo codigo as

  apagar codigo _ = do 
    alunoEncontrado <- buscar codigo (undefined :: Aluno)
    case alunoEncontrado of
      Nothing -> error "Código de aluno não encontrado."
      Just aluno -> do
        S alunos <- obter (undefined :: Aluno)
        let S alunoRemovido = remover aluno (S alunos)
        arq <- openFile "Aluno.txt" WriteMode
        salvaAlunos arq alunoRemovido
        hClose arq
        return aluno
        where
          salvaAlunos _ [] = return ()
          salvaAlunos arq (a:as) = do
            hPutStrLn arq (show a)
            salvaAlunos arq as
