# Spec Delta

## ADDED Requirements

### Requirement: Pular custa tempo
Durante a rodada, o botão Pular SHALL estar disponível desde que a palavra aparece, sem espera, e SHALL indicar que pular custa 5 segundos. Tocar em Pular SHALL tirar 5 segundos do tempo restante da rodada e sortear uma nova palavra, sem alterar o contador de acertos. Quando restam 5 segundos ou menos, o botão SHALL ficar desabilitado e um toque nele SHALL NOT ter efeito.

#### Scenario: Pular logo que a palavra aparece
- **WHEN** a palavra acabou de aparecer, restam 00:40 e o jogador toca em Pular
- **THEN** o cronômetro passa a mostrar 00:35 e a barra de progresso diminui na mesma proporção
- **AND** uma nova palavra é exibida e o contador de acertos não muda

#### Scenario: Pulos seguidos
- **WHEN** restam 00:30 e o jogador toca em Pular duas vezes, sem esperar entre os toques
- **THEN** o cronômetro passa a mostrar 00:20

#### Scenario: Custo visível
- **WHEN** a rodada está em andamento
- **THEN** o botão Pular mostra que pular custa 5 segundos ("−5s")
- **AND** um leitor de tela anuncia o botão como pular e perder 5 segundos

#### Scenario: Tempo insuficiente
- **WHEN** restam 5 segundos ou menos
- **THEN** o botão Pular está desabilitado e tocar nele não muda a palavra nem o cronômetro

#### Scenario: Limite exato
- **WHEN** restam 00:06 e o jogador toca em Pular
- **THEN** o cronômetro passa a mostrar 00:01 e o botão Pular fica desabilitado

#### Scenario: Pulo leva aos últimos segundos
- **WHEN** restam 00:14 e o jogador toca em Pular
- **THEN** o cronômetro mostra 00:09 e o cronômetro e a barra mudam para a cor de alerta

## REMOVED Requirements

### Requirement: Pular com bloqueio
**Reason**: O bloqueio de 5 segundos por palavra deu lugar a uma penalidade de tempo. O bloqueio também era contado pelos ticks do cronômetro e durava entre 4 e 5 segundos.
**Migration**: Sem migração de dados. A regra nova está no requisito "Pular custa tempo".
