# Sistema de Gerenciamento de Inventário - Haskell

## Metadados de Identificação
- **Instituição de Ensino**: Pontifícia Universidade Católica do Paraná (PUCPR)
- **Disciplina**: Programação Lógica e Funcional - Haskell
- **Professor Responsável**: Frank Coelho de Alcantara
- **Atividade Avaliativa**: RA2 - Funcional
- **Data de Entrega**: 14/11/2025
- **Modalidade**: Trabalho em Grupo

## Identificação dos Integrantes

| Nome do Aluno | Usuário GitHub |
|---------------|----------------|
| Pedro Senes Velloso Ribeiro | [@prussianmaster1871](https://github.com/prussianmaster1871)     |
| Renan da Silva Oliveira Andrade     | [@marshmll](https://github.com/marshmll)     |
| Ricardo Lucas Kucek     | [@Ricardo-LK](https://github.com/Ricardo-LK)     |
| Riscala Miguel Fadel Neto     | [@Vareja0](https://github.com/Vareja0)     |

## Link de Execução do Projeto

**Ambiente de Execução Online**: [https://onlinegdb.com/v4oxo63n5K](https://onlinegdb.com/v4oxo63n5K)

## Especificação Técnica do Sistema

### 1. Arquitetura do Sistema

O sistema implementa um gerenciador de inventário seguindo os princípios de programação funcional em Haskell, com rigorosa separação entre lógica pura e operações de I/O.

### 2. Estrutura de Dados Conforme Especificação

#### Tipos de Dados Definidos

```haskell
-- Item do inventário
data Item = Item {
    itemID :: String,      -- Identificador único
    nome :: String,        -- Nome descritivo
    quantidade :: Int,     -- Quantidade em estoque
    categoria :: String    -- Categoria de classificação
} deriving (Show, Read, Eq, Ord)

-- Estrutura principal do inventário
type Inventario = Map String Item

-- Tipos de ação para auditoria
data AcaoLog = Add | Remove | Update | QueryFail
    deriving (Show, Read, Eq, Ord)

-- Status das operações
data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq, Ord)

-- Entrada de log completo
data LogEntry = LogEntry {
    timestamp :: UTCTime,  -- Timestamp da operação
    acao :: AcaoLog,       -- Tipo de ação executada
    detalhes :: String,    -- Detalhes descritivos
    status :: StatusLog    -- Status do resultado
} deriving (Show, Read, Eq, Ord)

-- Resultado das operações
type ResultadoOperacao = (Inventario, LogEntry)
```

### 3. Funções de Lógica Pura Implementadas

#### Operações Principais

- **`addItem`**: Adiciona novo item ao inventário
- **`removeItem`**: Remove quantidade de item existente
- **`updateItem`**: Atualiza quantidade de item
- **`listarItens`**: Lista todos os itens formatados

#### Funções de Análise e Relatório

- **`logsDeErro`**: Filtra logs com status de falha
- **`historicoPorItem`**: Histórico de operações por item
- **`itemMaisMovimentado`**: Identifica item com mais movimentações
- **`formatarLogsDeErro`**: Formata logs para exibição

### 4. Sistema de Persistência

#### Arquivos de Dados

- **`Inventario.dat`**: Estado atual do inventário (sobrescrito)
- **`Auditoria.log`**: Log de auditoria (append-only)

#### Funções de I/O

- **`carregarInventario`**: Carrega estado com tratamento de exceções
- **`carregarLogs`**: Carrega histórico de auditoria
- **`salvarInventario`**: Persiste estado do inventário
- **`salvarLog`**: Adiciona entrada ao log de auditoria

### 5. Comandos do Sistema

| Comando | Sintaxe | Descrição |
|---------|---------|-----------|
| `add` | `add <id> <nome> <qtd> <categoria>` | Adiciona novo item |
| `remove` | `remove <id> <qtd>` | Remove quantidade |
| `update` | `update <id> <nova_qtd>` | Atualiza quantidade |
| `listar` | `listar` | Lista todos os itens |
| `report` | `report` | Gera relatórios |
| `historico` | `historico <id>` | Histórico do item |
| `ajuda` | `ajuda` | Ajuda dos comandos |
| `sair` | `sair` | Encerra sistema |

## Evidências de Conformidade com a Rubrica

### Atendimento aos Requisitos Obrigatórios

#### 1. Separação Lógica Pura/Impura

- **Lógica Pura**: `addItem`, `removeItem`, `updateItem`, `listarItens`
- **Operações I/O**: `main`, `loopPrincipal`, funções de arquivo
- **Sem mistura**: Nenhuma operação de I/O dentro das funções puras

#### 2. Persistência de Estado

- Leitura inicial de `Inventario.dat` e `Auditoria.log`
- Escrita de `Inventario.dat` após operações bem-sucedidas
- Append de `Auditoria.log` para todas as operações
- Tratamento de arquivos inexistentes na primeira execução

#### 3. Sistema de Auditoria

- Registro de todas as operações (sucesso e falha)
- Timestamp em `UTCTime` para cada entrada
- Status detalhado (`Sucesso`/`Falha String`)
- Detalhes descritivos das operações

#### 4. Derivação de Show/Read

```haskell
deriving (Show, Read, Eq, Ord)
```
- Implementada para todos os tipos de dados
- Permite serialização/desserialização completa
- Testado com `read . show` para verificação

### 6. Dados Mínimos para Teste

Conjunto de 10 itens distintos para validação:

```
add 001 Teclado 15 Informatica
add 002 Mouse 20 Informatica
add 003 Monitor 8 Informatica
add 004 Notebook 5 Eletronicos
add 005 Impressora 3 Escritorio
add 006 Cadeira 12 Movel
add 007 Mesa 6 Movel
add 008 Tablet 10 Eletronicos
add 009 Smartphone 25 Eletronicos
add 010 FoneOuvido 30 Acessorios
```

## Documentação dos Cenários de Teste

### Cenário 1: Persistência de Estado

**Objetivo**: Validar persistência entre execuções
**Procedimento**:
1. Executar sistema sem arquivos existentes
2. Adicionar 3 itens: `add 011 ItemA 10 CatA`, `add 012 ItemB 5 CatB`, `add 013 ItemC 8 CatC`
3. Executar `sair` para persistir
4. Reiniciar sistema
5. Executar `listar` para verificar carga

**Resultado Esperado**: Os 3 itens devem aparecer no inventário após reinicialização

### Cenário 2: Validação de Lógica de Negócio

**Objetivo**: Testar tratamento de erro de estoque insuficiente
**Procedimento**:
1. `add 014 Teclado 10 Informatica`
2. `remove 014 15` (tentar remover mais que existe)
3. Verificar mensagem de erro
4. Executar `listar` para confirmar quantidade

**Resultado Esperado**: Mensagem "Falha: Estoque insuficiente", quantidade mantida em 10

### Cenário 3: Geração de Relatórios

**Objetivo**: Validar funções de análise de logs
**Procedimento**:
1. Executar Cenário 2 para gerar erro
2. Executar `report`
3. Verificar saída dos relatórios

**Resultado Esperado**: Relatório de erros deve mostrar a falha de estoque insuficiente

## Análise de Robustez

### Tratamento de Exceções

```haskell
lidarExcecaoLeitura :: a -> IOError -> IO a
lidarExcecaoLeitura valorPadrao err
    | isDoesNotExistError err = return valorPadrao
    | otherwise = return valorPadrao
```
- Tratamento de arquivos não encontrados
- Não crasha na primeira execução
- Retorna estado vazio como fallback

### Validação de Entrada

- Uso de `readMaybe` para conversão segura
- Verificação de IDs duplicados
- Validação de estoque suficiente
- Mensagens de erro descritivas

## Estrutura do Repositório GitHub

```
RA2-7/
├── screenshots             # Prints dos testes
├── main.hs                 # Código fonte principal
├── Inventario.dat          # Arquivo de estado (gerado)
├── Auditoria.log           # Arquivo de logs (gerado)
└── README.md               # Este documento
```

## Comprovante de Autoria

- **Histórico de Commits**: Disponível no GitHub com contribuições individuais
- **Divisão de Tarefas**: Conforme especificação do documento
- **Originalidade**: Código desenvolvido pelo grupo sem plágio

## Instruções de Compilação e Execução

### Online GDB
1. Acessar [LINK DO ONLINE GDB](https://onlinegdb.com/Ob0m4U-UCO)
2. Colar código completo na aba Haskell
3. Clicar em "Run"
4. Interagir via terminal integrado

## Conclusão
Este sistema atende integralmente todos os requisitos especificados na rubrica avaliativa RA2, demonstrando:
- Domínio dos conceitos de programação funcional em Haskell
- Correta separação entre lógica pura e operações de I/O
- Implementação robusta de persistência e auditoria
- Conformidade total com as especificações de nomenclatura e funcionalidade
