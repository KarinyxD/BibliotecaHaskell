module Alunos where
import Dados
import Util
--Insira na classe Dado a definição da função imprimir e implemente uma versão
--para Aluno, Livro e Empréstimo nos respectivos módulos.
--Essa função recebe um valor de um tipo pertencente à classe Dado e 
-- imprime na tela todos os dados desse valor. Repare que a função já faz a impressão na tela 
-- ao invés de retornar um String. A impressão dos atributos e valores dos dados devem estar formatados 
-- com base na função formata.

data Aluno = Aluno Codigo Nome Email
data Codigo = Codigo Int
data Nome = Nome String
data Email = Email String

instance Dado Aluno where
  imprimir (Aluno (Codigo x) (Nome n) (Email e)) = do 
    putStrLn (formata "Código" (show x))
    putStrLn (formata "Nome" n)
    putStrLn (formata "Email" e)