module Dados where
-- Questão 02 [1,0]. Crie no módulo Dados um Tipo Abstrato de Dados (TAD) Set para armazenar
-- valores de tipos pertencentes a classe Dado. Set (conjunto) é um TAD semelhante a uma lista,
-- porém não permite valores repetidos. Ele deve ter funções que permitem inserir, remover e buscar
-- um dado e verificar se está vazio.

--Insira na classe Dado a definição da função imprimir e implemente uma versão
--para Aluno, Livro e Empréstimo nos respectivos módulos.
--Essa função recebe um valor de um tipo pertencente à classe Dado e 
-- imprime na tela todos os dados desse valor. Repare que a função já faz a impressão na tela 
-- ao invés de retornar um String. A impressão dos atributos e valores dos dados devem estar formatados 
-- com base na função formata.

class Dado a where
  imprimir :: a -> IO ()


data Set t = S [t] deriving (Eq) 

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
