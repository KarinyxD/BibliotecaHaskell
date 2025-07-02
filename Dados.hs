{-# LANGUAGE AllowAmbiguousTypes #-}
module Dados where
import System.IO
-- Questão 06 [1,0]. Insira na classe Dado a definição da função obter e implemente uma versão
-- para Aluno, Livro e Empréstimo nos respectivos módulos. Essa função retorna um TAD Set
-- (Conjunto) contendo, respectivamente, todos os alunos, livros ou empréstimos do arquivo.

-- Questão 07 [1,0]. Insira na classe Dado a definição da função buscar e implemente uma versão
-- para Aluno, Livro e Empréstimo nos respectivos módulos. Essa função recebe um inteiro, que
-- corresponde ao código, registro ou número, e retorna o dado correspondente a partir da TAD Set
-- carregado do arquivo. Note que a função pode não encontrar o dado com o inteiro informado e
-- retornar Nothing.

-- Questão 08 [1,0]. Insira na classe Dado a definição da função apagar e implemente uma versão
-- para Aluno, Livro e Empréstimo nos respectivos módulos. Essa função recebe um inteiro, que
-- corresponde ao código, registro ou número, remove dado correspondente do TAD Set carregado
-- do arquivo e atualiza esse arquivo. Um aluno ou livro não pode ser apagado se houver um
-- empréstimo cadastrado com ele.

-- Questão 09 [1,0]. Insira na classe Dado a definição da função showmenu e implemente uma
-- versão para Aluno, Livro e Empréstimo nos respectivos módulos. Essa função imprime na tela um
-- menu com as opções voltar, visualizar, cadastrar e apagar e solicita que o usuário digite uma
-- opção, retornando o valor digitado.

class Dado a where
  imprimir :: a -> IO ()
  cadastrar :: a -> IO a
  showmenu :: a -> IO Int

data Set t = S [t] deriving (Eq, Show)

inserir :: (Dado t, Eq t) => t -> Set t -> Set t
remover :: (Dado t, Eq t) => t -> Set t -> Set t
buscar :: (Dado t, Eq t)=> t -> Set t -> Bool
verifica :: Set t -> Bool

inserir x (S xs)  
  | buscar x (S xs) = (S xs)
  | otherwise = (S (x:xs))

remover x (S []) = S []
remover x (S (h:xs)) = if x == h then (S xs) else S (h:ys)
  where 
    S ys = remover x (S xs)

buscar _ (S []) = False
buscar x (S (h:xs)) = if x == h then True else buscar x (S xs)

verifica (S []) = True
verifica (S _) = False
