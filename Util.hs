-- modulo para implementar funcoes auxiliares de outros modulos
module Util where

data Data = Data {dia :: Int, mes :: Int, ano :: Int} deriving (Eq, Show, Read)

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