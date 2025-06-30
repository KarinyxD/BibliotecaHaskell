module Livros where

data Livro = Livro Registro Titulo Edicao
data Registro = Registro Int
data Titulo = Titulo String
data Edicao = Edicao Int