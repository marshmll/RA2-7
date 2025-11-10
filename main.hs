{-
  Atividade Avaliativa - RA2 (Haskell)
  Estrutura do Projeto - Gerenciador de Inventário
  Autor: Integrante 2 (Lógica de Negócio)
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

----------------------------------------------------------------------
-- PARTE 2: LÓGICA DE NEGÓCIO PURA (Funções Puras)
----------------------------------------------------------------------

addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem time id nome qtd cat inv =
    if Map.member id inv
    then Left $ "Falha: Item com ID '" ++ id ++ "' já existe."
    else
        let novoItem = Item id nome qtd cat
            novoInv = Map.insert id novoItem inv
            logEntry = LogEntry time Add ("ItemID: " ++ id ++ ", Adicionado: " ++ nome) Sucesso
        in Right (novoInv, logEntry)

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time id qtdRemover inv =
    case Map.lookup id inv of
        Nothing -> Left $ "Falha: Item com ID '" ++ id ++ "' não encontrado."
        Just item ->
            if quantidade item < qtdRemover
            then Left $ "Falha: Estoque insuficiente para '" ++ nome item ++ "'."
            else
                let novaQtd = quantidade item - qtdRemover
                    novoInv = if novaQtd == 0 then Map.delete id inv
                               else Map.adjust (\i -> i { quantidade = novaQtd }) id inv
                    logEntry = LogEntry time Remove ("ItemID: " ++ id ++ ", Removido " ++ show qtdRemover) Sucesso
                in Right (novoInv, logEntry)

listarItens :: Inventario -> String
listarItens inv
    | Map.null inv = "Inventário está vazio."
    | otherwise = unlines $ map (\i -> itemID i ++ " - " ++ nome i ++ ": " ++ show (quantidade i)) (Map.elems inv)

main :: IO ()
main = putStrLn "Funções puras implementadas com sucesso."
