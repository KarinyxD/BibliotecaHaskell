module Main where
import Alunos
import Livros
import Emprestimo
import Dados
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
              -- Aluno
              1 -> do 
                x <- Alunos.showmenu (undefined :: Aluno)
                case x of
                  1 -> do 
                    loop
                  2 -> do
                    S alunos <- Alunos.obter (undefined :: Aluno)
                    imprimirAlunos alunos
                    loop
                      where 
                        imprimirAlunos [] = return ()
                        imprimirAlunos (a:as) = do
                          imprimir a 
                          imprimirAlunos as
                  3 -> do 
                    Alunos.cadastrar (undefined :: Aluno)
                    loop 
                  4 -> do
                    putStrLn "Digite o codigo do aluno que deseja apagar: "
                    codigo <- readLn 
                    alunoRemovido <- Alunos.apagar codigo (undefined :: Aluno)
                    putStrLn "Aluno removido com sucesso: "
                    imprimir alunoRemovido
                    loop
                  _ -> do 
                    putStrLn "Opcao invalida"
                    loop
              -- Livro
              2 -> do
                x <- Livros.showmenu (undefined :: Livro)
                case x of
                  1 -> do 
                    loop
                  2 -> do
                    S livros <- Livros.obter (undefined :: Livro)
                    imprimirLivros livros
                    loop
                      where 
                        imprimirLivros [] = return ()
                        imprimirLivros (l:ls) = do
                          imprimir l
                          imprimirLivros ls
                  3 -> do 
                    Livros.cadastrar (undefined :: Livro)
                    loop                  
                  4 -> do
                    putStrLn "Digite o registro do livro que deseja apagar: "
                    registro <- readLn 
                    livroRemovido <- Livros.apagar registro (undefined :: Livro)
                    putStrLn "Livro removido com sucesso: "
                    imprimir livroRemovido
                    loop 
                  _ -> do 
                    putStrLn "Opcao invalida"
                    loop
              -- Emprestimo
              3 -> do
                x <- Emprestimo.showmenu (undefined :: Emprestimo)
                case x of
                  1 -> do 
                    loop
                  2 -> do
                    S emprestimos <- Emprestimo.obter (undefined :: Emprestimo)
                    imprimirEmprestimos emprestimos
                    loop
                      where 
                        imprimirEmprestimos [] = return ()
                        imprimirEmprestimos (e:es) = do
                          imprimir e
                          imprimirEmprestimos es
                  3 -> do 
                    Emprestimo.cadastrar (undefined :: Emprestimo)
                    loop
                  4 -> do
                    putStrLn "Digite o número do emprestimo que deseja apagar: "
                    numero <- readLn 
                    emprestimoRemovido <- Emprestimo.apagar numero (undefined :: Emprestimo)
                    putStrLn "Emprestimo removido com sucesso: "
                    imprimir emprestimoRemovido
                    loop 
                  _ -> do 
                    putStrLn "Opcao invalida"
                    loop
              -- Voltar
              4 -> loop
              _ -> do
                putStrLn "Opcao invalida" 
                loop
          _ -> do 
            putStrLn "Opçao invalida"
            loop
  loop