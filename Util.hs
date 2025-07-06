-- modulo para implementar funcoes auxiliares de outros modulos
module Util where
import System.IO
data Data = Data {dia :: Int, mes :: Int, ano :: Int} deriving (Show, Read, Eq)

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

alunoEmEmprestimo :: Int -> IO Bool
alunoEmEmprestimo codigo = do
  arqEmp <- openFile "Emprestimo.txt" ReadMode
  temEmprestimo <- loopEmp arqEmp (show codigo)  
  hClose arqEmp
  return (temEmprestimo)    
    where       
      loopEmp arq codStr = do
        eof <- hIsEOF arq
        if eof then return False
        else do
          linha <- hGetLine arq
          if buscaId ("(Codigo " ++ codStr ++ ")") linha
            then return True
            else loopEmp arq codStr

livroEmEmprestimo :: Int -> IO Bool
livroEmEmprestimo registro = do
  arqEmp <- openFile "Emprestimo.txt" ReadMode
  temEmprestimo <- loopEmp arqEmp (show registro)
  hClose arqEmp
  return temEmprestimo
  where
    loopEmp arq regStr = do
      eof <- hIsEOF arq
      if eof then return False
      else do
        linha <- hGetLine arq
        if buscaId ("(Registro " ++ regStr ++ ")") linha
          then return True
          else loopEmp arq regStr

buscaId :: String -> String -> Bool
buscaId _ [] = False
buscaId sub xs
  | take (length sub) xs == sub = True
  | otherwise = buscaId sub (tail xs)