# Spec Delta

## Purpose

Define a tela de preparação da partida: como o grupo escolhe o tempo de cada rodada, cadastra e remove jogadores e quando a partida pode começar.

## ADDED Requirements

### Requirement: Escolha do tempo por presets
A tela de preparação SHALL oferecer os tempos de rodada 30s, 1min, 1min30, 2min e 3min como opções de escolha única, com 1min selecionado ao abrir o jogo. O tempo escolhido SHALL valer para todas as rodadas da partida.

#### Scenario: Valor inicial
- **WHEN** o jogador abre a tela de preparação pela primeira vez
- **THEN** a opção 1min está selecionada

#### Scenario: Trocar o tempo
- **WHEN** o jogador toca na opção 2min
- **THEN** 2min fica selecionado e a opção anterior deixa de estar
- **AND** cada rodada da partida começa com o cronômetro em 02:00

### Requirement: Adicionar jogador
O jogador SHALL poder adicionar um participante digitando o nome e confirmando pelo botão de adicionar ou pela tecla Enter/"OK" do teclado. Espaços nas pontas do nome SHALL ser removidos. Nomes vazios ou só com espaços SHALL NOT ser adicionados. Depois de adicionar, o campo SHALL ficar vazio e manter o foco.

#### Scenario: Adicionar pelo teclado
- **WHEN** o jogador digita "  Ana " e confirma pelo teclado
- **THEN** "Ana" aparece no fim da lista de jogadores
- **AND** o campo fica vazio e continua focado para o próximo nome

#### Scenario: Nome vazio
- **WHEN** o campo está vazio ou só tem espaços
- **THEN** o botão de adicionar fica desabilitado
- **AND** confirmar pelo teclado não adiciona ninguém

### Requirement: Remover jogador
Cada jogador da lista SHALL ter um botão de remover, com rótulo acessível que inclua o nome do jogador. Tocar nele SHALL remover o jogador da lista.

#### Scenario: Remover um jogador
- **WHEN** a lista tem "Ana" e "Beto" e o jogador toca em remover ao lado de "Ana"
- **THEN** só "Beto" continua na lista

#### Scenario: Leitor de tela
- **WHEN** um leitor de tela foca o botão de remover de "Ana"
- **THEN** ele anuncia um rótulo como "Remover Ana"

### Requirement: Estado vazio da lista
Quando não há jogadores, a tela SHALL mostrar uma mensagem curta explicando que são necessários pelo menos 2 jogadores, no lugar da lista.

#### Scenario: Sem jogadores
- **WHEN** a tela de preparação é aberta sem jogadores
- **THEN** aparece uma mensagem indicando que é preciso adicionar pelo menos 2 jogadores

### Requirement: Regra para começar
O botão Começar SHALL ficar habilitado apenas quando houver pelo menos 2 jogadores. Enquanto estiver desabilitado, a tela SHALL indicar quantos jogadores faltam.

#### Scenario: Um jogador
- **WHEN** a lista tem 1 jogador
- **THEN** o botão Começar está desabilitado
- **AND** a tela indica que falta 1 jogador

#### Scenario: Dois jogadores
- **WHEN** a lista tem 2 jogadores
- **THEN** o botão Começar está habilitado e, ao ser tocado, leva ao anúncio da primeira dupla
