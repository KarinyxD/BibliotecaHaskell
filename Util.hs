-- modulo para implementar funcoes auxiliares de outros modulos
module Util where
-- Questão 06 [1,0]. Insira na classe Dado a definição da função obter e implemente uma versão
-- para Aluno, Livro e Empréstimo nos respectivos módulos. Essa função retorna um TAD Set
-- (Conjunto) contendo, respectivamente, todos os alunos, livros ou empréstimos do arquivo.
data Data = Data {dia :: Int, mes :: Int, ano :: Int} deriving (Show, Read)

dataStr :: Data -> String
dataStr (Data d m a) = print d ++ "/" ++ print m ++ "/" ++ print a
  where 
    print x = if x < 10 then "0" ++ show x else show x

formata :: String -> String -> String
formata chave valor = chave ++ formataChave (30 - (length chave)) ++ ":" ++ formataValor(50 - (length valor)) ++ valor
  where
    formataChave 0 = []
    formataChave x = '.' : formataChave (x - 1)
    formataValor 0 = []
    formataValor x = ' ' : formataValor (x-1)
