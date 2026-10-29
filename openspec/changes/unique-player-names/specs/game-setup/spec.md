# Spec Delta

## ADDED Requirements

### Requirement: Nome de jogador repetido
A tela de preparação SHALL impedir a adição de um jogador cujo nome já está na lista. Dois nomes SHALL ser considerados iguais quando coincidem depois de remover os espaços das pontas, trocar sequências de espaços internos por um só e ignorar maiúsculas/minúsculas. Acentos SHALL continuar diferenciando nomes. Jogadores sendo removidos SHALL NOT contar. Enquanto o nome repetir, a tela SHALL mostrar um aviso ligado ao campo e o botão de adicionar SHALL ficar desabilitado.

#### Scenario: Mesmo nome com outra caixa e espaços
- **WHEN** a lista tem "Ana" e o jogador digita "  ANA "
- **THEN** aparece abaixo do campo um aviso como "Já tem um jogador chamado Ana."
- **AND** o botão de adicionar fica desabilitado

#### Scenario: Espaços repetidos no meio
- **WHEN** a lista tem "Ana Maria" e o jogador digita "ana   maria"
- **THEN** o aviso de nome repetido aparece e o botão de adicionar fica desabilitado

#### Scenario: Confirmar pelo teclado com nome repetido
- **WHEN** a lista tem "Ana", o campo tem "ana" e o jogador confirma pelo teclado
- **THEN** ninguém é adicionado
- **AND** o campo mantém o texto "ana" e o foco, com o aviso visível

#### Scenario: Acento diferencia nomes
- **WHEN** a lista tem "Ana" e o jogador digita "Aná"
- **THEN** nenhum aviso aparece e o botão de adicionar fica habilitado

#### Scenario: Aviso some ao corrigir o nome
- **WHEN** o aviso de nome repetido está visível e o jogador muda o texto para um nome que não está na lista
- **THEN** o aviso some e o botão de adicionar volta a ficar habilitado

#### Scenario: Remover o jogador de mesmo nome
- **WHEN** a lista tem "Ana", o campo tem "ana" com o aviso visível e o jogador toca em remover ao lado de "Ana"
- **THEN** o aviso some assim que a remoção começa
- **AND** confirmar adiciona "ana" à lista

#### Scenario: Leitor de tela
- **WHEN** um leitor de tela foca o campo com um nome repetido
- **THEN** ele anuncia o campo como inválido e lê o texto do aviso
