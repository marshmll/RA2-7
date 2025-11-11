{-
  Atividade Avaliativa - RA2 (Haskell)
  Sistema de Gerenciamento de Inventário
  Autor: Integrante 3 (Módulo de I/O e Persistência)
-}
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.IO
import System.IO.Error (isDoesNotExistError, catchIOError)
import Text.Read (readMaybe)

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
-- PARTE 2: LÓGICA DE NEGÓCIO PURA
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
            then Left $ "Falha: Estoque insuficiente."
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

----------------------------------------------------------------------
-- PARTE 3: MÓDULO DE I/O E PERSISTÊNCIA
----------------------------------------------------------------------

arqInventario = "Inventario.dat"
arqAuditoria = "Auditoria.log"

carregarInventario :: IO Inventario
carregarInventario = (do
    conteudo <- readFile arqInventario
    case readMaybe conteudo of
        Nothing -> putStrLn "Aviso: Inventário corrompido" >> return Map.empty
        Just inv -> return inv
    ) `catchIOError` (\_ -> putStrLn "Inventário não encontrado, iniciando vazio." >> return Map.empty)

salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile arqInventario (show inv)

salvarLog :: [LogEntry] -> LogEntry -> IO [LogEntry]
salvarLog logsAntigos novoLog = do
    let novos = logsAntigos ++ [novoLog]
    writeFile arqAuditoria (show novos)
    return novos

main :: IO ()
main = do
    putStrLn "Iniciando sistema de inventário..."
    inv <- carregarInventario
    loop inv []

loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
    putStr "> "
    hFlush stdout
    cmd <- getLine
    time <- getCurrentTime
    case words cmd of
        ["add", id, nome, qtdStr, cat] ->
            case readMaybe qtdStr of
                Nothing -> putStrLn "Quantidade inválida." >> loop inv logs
                Just qtd -> case addItem time id nome qtd cat inv of
                    Left erro -> putStrLn erro >> loop inv logs
                    Right (novo, logE) -> do
                        putStrLn "Item adicionado com sucesso."
                        salvarInventario novo
                        novos <- salvarLog logs logE
                        loop novo novos
        ["listar"] -> putStrLn (listarItens inv) >> loop inv logs
        ["sair"] -> putStrLn "Encerrando."
        _ -> putStrLn "Comando inválido." >> loop inv logs
