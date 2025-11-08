{-
  Atividade Avaliativa - RA2 (Haskell)
  Estrutura do Projeto - Gerenciador de Inventário
  Autor: Integrante 1 (Arquiteto de Dados)
-}
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Clock (UTCTime)

----------------------------------------------------------------------
-- PARTE 1: ARQUITETO DE DADOS (Tipos de Dados)
----------------------------------------------------------------------

data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read, Eq, Ord)

type Inventario = Map String Item

data AcaoLog = Add | Remove | Update | QueryFail
    deriving (Show, Read, Eq, Ord)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq, Ord)

data LogEntry = LogEntry {
    timestamp :: UTCTime,
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
} deriving (Show, Read, Eq, Ord)

type ResultadoOperacao = (Inventario, LogEntry)

main :: IO ()
main = putStrLn "Estrutura de tipos criada com sucesso."
