module Main where
import Alunos
import Livros
import Emprestimo
import Dados
import Util

main = do
  let loop = do
        putStrLn "Digite 1 se deseja Sair ou 2 se deseja entrar nos menus."
        opcao <- readLn
        case opcao of 
          1 -> return ()
          2 -> do
            menuPrincipal
            loop
          _ -> do
            putStrLn "Opção inválida"
            loop
  loop

menuPrincipal :: IO ()
menuPrincipal = do
  putStrLn "Digite 1-Aluno 2-Livro 3-Emprestimo 4-Voltar"
  m <- readLn
  case m of
    1 -> do
      menuAluno
      menuPrincipal
    2 -> do
      menuLivro
      menuPrincipal
    3 -> do
      menuEmprestimo
      menuPrincipal
    4 -> return ()
    _ -> do
      putStrLn "Opção inválida"
      menuPrincipal

menuAluno :: IO ()
menuAluno = do
  x <- Alunos.showmenu (undefined :: Aluno)
  case x of
    1 -> return ()
    2 -> do
      S alunos <- Alunos.obter (undefined :: Aluno)
      imprimirLista alunos
    3 -> do
      Alunos.cadastrar (undefined :: Aluno)
    4 -> do
      putStrLn "Digite o código do aluno que deseja apagar: "
      codigo <- readLn
      alunoRemovido <- Alunos.apagar codigo (undefined :: Aluno)
      putStrLn "Aluno removido com sucesso:"
      imprimir alunoRemovido
    _ -> putStrLn "Opção inválida"

menuLivro :: IO ()
menuLivro = do
  x <- Livros.showmenu (undefined :: Livro)
  case x of
    1 -> return ()
    2 -> do
      S livros <- Livros.obter (undefined :: Livro)
      imprimirLista livros
    3 -> do
      Livros.cadastrar (undefined :: Livro)
    4 -> do
      putStrLn "Digite o registro do livro que deseja apagar: "
      registro <- readLn
      livroRemovido <- Livros.apagar registro (undefined :: Livro)
      putStrLn "Livro removido com sucesso:"
      imprimir livroRemovido
    _ -> putStrLn "Opção inválida"

menuEmprestimo :: IO ()
menuEmprestimo = do
  x <- Emprestimo.showmenu (undefined :: Emprestimo)
  case x of
    1 -> return ()
    2 -> do
      S emprestimos <- Emprestimo.obter (undefined :: Emprestimo)
      imprimirLista emprestimos
    3 -> do
      Emprestimo.cadastrar (undefined :: Emprestimo)
    4 -> do
      putStrLn "Digite o número do empréstimo que deseja apagar: "
      numero <- readLn
      emprestimoRemovido <- Emprestimo.apagar numero (undefined :: Emprestimo)
      putStrLn "Empréstimo removido com sucesso:"
      imprimir emprestimoRemovido
    _ -> putStrLn "Opção inválida"

