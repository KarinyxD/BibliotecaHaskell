module Livros (Livro(..), showmenu, cadastrar, imprimir, obter, buscar, apagar) where
import System.IO
import Dados
import Util

data Livro = Livro Registro Titulo Edicao deriving (Show, Read, Eq)
data Registro = Registro Int deriving (Show, Read, Eq)
data Titulo = Titulo String deriving (Show, Read, Eq)
data Edicao = Edicao Int deriving (Show, Read, Eq)

instance Dado Livro where
  imprimir (Livro (Registro x) (Titulo t) (Edicao e)) = do 
    putStrLn (formata "Registro" (show x))
    putStrLn (formata "Titulo" t)
    putStrLn (formata "Edicao" (show e))

  cadastrar _ = do 
    putStrLn "Digite o registro do Livro: "
    reg <- readLn
    livroExistente <- buscar reg (undefined :: Livro)
    case livroExistente of
      Just _ -> putStrLn "Já existe um livro com esse registro! Cadastro cancelado."
      Nothing -> do
        putStrLn "Digite o titulo do Livro: "
        titulo <- getLine
        putStrLn "Digite a edicao do Livro: "
        ed <- readLn
        let livro = (Livro (Registro reg) (Titulo titulo) (Edicao ed))
        arq <- openFile "Livro.txt" AppendMode
        hPutStrLn arq (show livro)
        hClose arq
        putStrLn "Livro cadastrado com sucesso!"
        imprimir livro

  showmenu _ = do
    putStrLn "Digite 1-Voltar 2-Visualizar 3-Cadastrar 4-Apagar"
    readLn

  obter _ = do
    arq <- openFile "Livro.txt" ReadMode
    let loop list = do
          eof <- hIsEOF arq
          if eof then return list
          else do
            linharq <- hGetLine arq
            let livro = read linharq :: Livro
            loop (inserir livro list)
    listaLivros <- loop (S [])
    hClose arq
    return listaLivros

  buscar registro _ = do 
    S livros <- obter (undefined :: Livro)
    return (buscarCodigo registro livros)
    where
      getRegistro (Livro (Registro reg) _ _) = reg
      buscarCodigo _ [] = Nothing
      buscarCodigo registro (l:ls)
        | (getRegistro l) == registro = Just l
        | otherwise = buscarCodigo registro ls

  apagar registro _ = do 
    livroEncontrado <- buscar registro (undefined :: Livro)
    case livroEncontrado of
      Nothing -> error "Registro de livro não encontrado."
      Just livro -> do
        temEmprestimo <- livroEmEmprestimo registro 
        if temEmprestimo 
          then error $ "Nao é possivel remover o livro com registro " ++ show registro ++ ", pois ele está em um emprestimo."
          else do
            S livros <- obter (undefined :: Livro)
            let S livroRemovido = remover livro (S livros)
            arq <- openFile "Livro.txt" WriteMode
            salvaArquivo arq livroRemovido
            hClose arq
            return livro

  