module Main where
import Alunos
import Livros
import Emprestimo
import Util
-- Questão 10 [1,0]. Crie o módulo Main e implemente nele a função main, em que o usuário pode
-- escolher entre sair ou entrar nos menus de aluno, livro e empréstimo. Em cada um desses menus
-- ele pode voltar ao menu principal, visualizar, cadastrar ou apagar os dados de um aluno, livro ou
-- empréstimo.

main = do
  let loop = do
        putStrLn "Digite 1 se deseja Sair ou 2 se deseja entrar nos menus."
        opcao <- readLn
        case opcao of 
          1 -> return ()
          2 -> do     
            putStrLn "Digite 1-Aluno 2-Livro 3-Emprestimo 4-Voltar"
            m <- readLn 
            case m of
              1 -> do 
                x <- Alunos.showmenu (undefined :: Aluno)
                case x of
                  1 -> do 
                    loop
                  3 -> do 
                    Alunos.cadastrar (undefined :: Aluno)
                    loop 
                  _ -> do 
                    putStrLn "Opcao invalida"
                    loop
              2 -> do
                x <- Livros.showmenu (undefined :: Livro)
                case x of
                  1 -> do 
                    loop
                  3 -> do 
                    Livros.cadastrar (undefined :: Livro)
                    loop 
                  _ -> do 
                    putStrLn "Opcao invalida"
                    loop
              3 -> do
                x <- Emprestimo.showmenu (undefined :: Emprestimo)
                case x of
                  1 -> do 
                    loop
                  3 -> do 
                    Emprestimo.cadastrar (undefined :: Emprestimo)
                    loop 
                  _ -> do 
                    putStrLn "Opcao invalida"
                    loop
              4 -> loop
              _ -> do
                putStrLn "Opcao invalida" 
                loop
          _ -> do 
            putStrLn "Opçao invalida"
            loop
  loop