# Spec Delta

## Purpose

Define o conjunto de palavras que o jogo pode sortear e os critérios que garantem que toda palavra sorteada seja conhecida e possa ser descrita pelos jogadores.

## ADDED Requirements

### Requirement: Lista única de palavras
O jogo SHALL sortear palavras de uma única lista curada. Não SHALL existir opção para o jogador escolher entre palavras verificadas e todas as palavras.

#### Scenario: Tela de preparação sem seleção de lista
- **WHEN** o jogador abre a tela de preparação
- **THEN** não há switch "Palavras verificadas / Todas as palavras"
- **AND** as opções de tempo e jogadores continuam disponíveis

#### Scenario: Sorteio usa a lista curada
- **WHEN** uma nova palavra é sorteada durante a partida
- **THEN** a palavra pertence à lista curada

### Requirement: Palavras conhecidas pelo público geral
Toda palavra da lista SHALL ser de uso comum no português brasileiro, de modo que um adulto médio a reconheça e consiga dar dicas sobre ela sem consultar dicionário. Termos técnicos, arcaicos, regionais raros, eruditos ou de nicho SHALL ser excluídos.

#### Scenario: Palavra comum é mantida
- **WHEN** a palavra é de uso cotidiano (ex.: "Hospital", "Capacete", "Zumbir")
- **THEN** ela está na lista

#### Scenario: Palavra obscura é excluída
- **WHEN** a palavra é técnica, erudita ou rara (ex.: "Androceu", "Háfnio", "Haplologia", "Hamadríade")
- **THEN** ela não está na lista

### Requirement: Palavras bem formadas
Toda palavra da lista SHALL ser uma única palavra válida do português (ou estrangeirismo de uso corrente no Brasil), com grafia correta, sem espaços espúrios nem pontuação, começando com letra maiúscula.

#### Scenario: Entrada corrompida é excluída
- **WHEN** a entrada original tem espaço ou pontuação espúrios, ou é uma não-palavra (ex.: "Phil ips", "Charle sto.", "Araini ng", "Houveu")
- **THEN** ela não está na lista

#### Scenario: Estrangeirismo corrente é aceito
- **WHEN** a palavra é estrangeirismo de uso comum no Brasil (ex.: "Hacker", "Handebol")
- **THEN** ela pode estar na lista se atender aos demais critérios

### Requirement: Sem duplicatas
A lista SHALL conter cada palavra no máximo uma vez.

#### Scenario: Palavra repetida na origem
- **WHEN** a mesma palavra aparecia mais de uma vez nas listas originais (ex.: "Cacto", "Ducha")
- **THEN** ela aparece exatamente uma vez na lista final

### Requirement: Cobertura de todo o alfabeto
A lista SHALL incluir palavras aprovadas de todas as letras iniciais presentes no vocabulário original, não apenas de A a H. Não há limite de quantidade por letra.

#### Scenario: Letras antes não verificadas aparecem
- **WHEN** uma partida sorteia várias palavras
- **THEN** podem sair palavras iniciadas por qualquer letra de A a Z que tenha palavras aprovadas (ex.: I, M, P, S, Z)
