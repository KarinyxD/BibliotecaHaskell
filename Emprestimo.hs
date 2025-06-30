module Emprestimo where
import Alunos
import Util
import Livros

data Emprestimo = Emprestimo Numero Aluno Data Data [Livro]
