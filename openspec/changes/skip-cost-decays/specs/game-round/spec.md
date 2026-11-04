# Spec Delta

## MODIFIED Requirements

### Requirement: Pular custa tempo
Durante a rodada, o botão Pular SHALL estar disponível desde que a palavra aparece, sem espera, e SHALL indicar o custo atual de pular, em segundos arredondados para cima. O custo SHALL começar em 5 segundos quando a palavra aparece e cair na mesma medida do tempo de rodada que passa com a palavra na tela, até zerar. Tocar em Pular SHALL tirar o custo atual do tempo restante da rodada e sortear uma nova palavra, sem alterar o contador de acertos. Se a palavra apareceu com 5 segundos ou menos restantes, o botão SHALL ficar desabilitado até o fim da rodada e um toque nele SHALL NOT ter efeito. Um pulo SHALL NOT terminar a rodada.

#### Scenario: Pular logo que a palavra aparece
- **WHEN** a palavra acabou de aparecer, restam 00:40 e o jogador toca em Pular
- **THEN** o cronômetro passa a mostrar 00:35 e a barra de progresso diminui na mesma proporção
- **AND** uma nova palavra é exibida e o contador de acertos não muda

#### Scenario: Custo diminui com o tempo
- **WHEN** a palavra apareceu com 00:40 restantes, o cronômetro chegou a 00:38 e o jogador toca em Pular
- **THEN** antes do toque o botão mostra "−3s"
- **AND** depois do toque o cronômetro mostra 00:35

#### Scenario: Pular de graça
- **WHEN** a palavra apareceu com 00:40 restantes, o cronômetro chegou a 00:33 e o jogador toca em Pular
- **THEN** antes do toque o botão não mostra custo
- **AND** depois do toque o cronômetro continua em 00:33 e uma nova palavra é exibida

#### Scenario: Custo recomeça a cada palavra
- **WHEN** o custo já caiu e uma nova palavra aparece, depois de Acertou ou de Pular
- **THEN** o botão Pular volta a mostrar "−5s"

#### Scenario: Pulos seguidos
- **WHEN** a palavra acabou de aparecer, restam 00:30 e o jogador toca em Pular duas vezes, sem esperar entre os toques
- **THEN** o cronômetro passa a mostrar 00:20

#### Scenario: Pausa não reduz o custo
- **WHEN** a palavra apareceu com 00:40 restantes, a rodada pausou em 00:38 e o jogador retomou depois de 1 minuto
- **THEN** o botão continua mostrando "−3s"
- **AND** tocar em Pular leva o cronômetro a 00:35

#### Scenario: Custo visível
- **WHEN** a palavra acabou de aparecer
- **THEN** o botão Pular mostra que pular custa 5 segundos ("−5s")
- **AND** um leitor de tela anuncia o botão como pular e perder 5 segundos

#### Scenario: Sem custo para o leitor de tela
- **WHEN** o custo de pular zerou
- **THEN** um leitor de tela anuncia o botão como pular sem custo

#### Scenario: Tempo insuficiente
- **WHEN** a palavra apareceu com 00:05 ou menos restantes
- **THEN** o botão Pular fica desabilitado até o fim da rodada e tocar nele não muda a palavra nem o cronômetro

#### Scenario: Limite exato
- **WHEN** a palavra acabou de aparecer, restam 00:06 e o jogador toca em Pular
- **THEN** o cronômetro passa a mostrar 00:01 e o botão Pular fica desabilitado

#### Scenario: Palavra antiga nos últimos segundos
- **WHEN** a palavra apareceu com 00:20 restantes, o cronômetro chegou a 00:03 e o jogador toca em Pular
- **THEN** o cronômetro continua em 00:03 e uma nova palavra é exibida
- **AND** o botão Pular fica desabilitado, porque a nova palavra apareceu com menos de 5 segundos restantes

#### Scenario: Pulo leva aos últimos segundos
- **WHEN** a palavra acabou de aparecer, restam 00:14 e o jogador toca em Pular
- **THEN** o cronômetro mostra 00:09 e o cronômetro e a barra mudam para a cor de alerta
