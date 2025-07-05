module Dados (Dado(..), Set(..), inserir, remover, procurar, verificar)where
import System.IO

class Dado a where
  imprimir :: a -> IO ()
  cadastrar :: a -> IO () 
  showmenu :: a -> IO Int
  obter :: a -> IO (Set a)
  buscar :: Int -> a -> IO (Maybe a)
  apagar :: Int -> a -> IO a


data Set t = S [t] deriving (Eq, Show)

inserir :: (Dado t, Eq t) => t -> Set t -> Set t
remover :: (Dado t, Eq t) => t -> Set t -> Set t
procurar :: (Dado t, Eq t) => t -> Set t -> Bool
verificar :: Set t -> Bool

inserir x (S xs)  
  | procurar x (S xs) = (S xs)
  | otherwise = (S (x:xs))

remover x (S []) = S []
remover x (S (h:xs)) = if x == h then (S xs) else S (h:ys)
  where 
    S ys = remover x (S xs)

procurar _ (S []) = False
procurar x (S (h:xs)) = if x == h then True else procurar x (S xs)

verificar (S []) = True
verificar (S _) = False
