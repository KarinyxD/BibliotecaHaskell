module Livros where
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

  cadastrar livro = do 
    arq <- openFile "Livro.txt" AppendMode
    hPutStrLn arq (show livro)
    hClose arq