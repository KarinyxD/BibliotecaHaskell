module Livros (Livro(..), showmenu, cadastrar, imprimir) where
import System.IO
import Dados
import Util

data Livro = Livro Registro Titulo Edicao deriving (Show, Read)
data Registro = Registro Int deriving (Show, Read)
data Titulo = Titulo String deriving (Show, Read)
data Edicao = Edicao Int deriving (Show, Read)

instance Dado Livro where
  imprimir (Livro (Registro x) (Titulo t) (Edicao e)) = do 
    putStrLn (formata "Registro" (show x))
    putStrLn (formata "Titulo" t)
    putStrLn (formata "Edicao" (show e))

  cadastrar _ = do 
    putStrLn "Digite o registro do Livro: "
    reg <- readLn
    putStrLn "Digite o titulo do Livro: "
    titulo <- getLine
    putStrLn "Digite a edicao do Livro: "
    ed <- readLn

    let livro = (Livro (Registro reg) (Titulo titulo) (Edicao ed))
    arq <- openFile "Livro.txt" AppendMode
    hPutStrLn arq (show livro)
    hClose arq
    putStrLn "Livro cadastrado com sucesso!"
    return livro

  showmenu _ = do
    putStrLn "Digite 1-Voltar 2-Visualizar 3-Cadastrar 4-Apagar"
    readLn

  