module Livros where
import Dados
import Util

data Livro = Livro Registro Titulo Edicao
data Registro = Registro Int
data Titulo = Titulo String
data Edicao = Edicao Int

instance Dado Livro where
  imprimir (Livro (Registro x) (Titulo t) (Edicao e)) = do 
    putStrLn (formata "Registro" (show x))
    putStrLn (formata "Titulo" t)
    putStrLn (formata "Edicao" (show e))