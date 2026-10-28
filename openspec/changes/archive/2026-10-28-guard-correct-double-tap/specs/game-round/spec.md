# Spec Delta

## MODIFIED Requirements

### Requirement: Acertar
Durante a rodada, tocar em Acertou SHALL somar 1 ao contador de acertos da rodada, exibido na tela, e sortear uma nova palavra ainda não sorteada na partida. Depois de um toque em Acertou ou Pular, o botão Acertou SHALL ignorar toques e teclas até 500 ms depois de a nova palavra aparecer. Um toque ignorado SHALL NOT ter efeito: não soma acerto, não entra no histórico, não troca a palavra e não vibra. A aparência do botão SHALL NOT mudar por causa desse bloqueio.

#### Scenario: Acerto
- **WHEN** o contador está em 2 e o jogador toca em Acertou
- **THEN** o contador mostra 3 com uma animação de incremento
- **AND** uma nova palavra é exibida

#### Scenario: Toque duplo em Acertou
- **WHEN** o contador está em 2 e o jogador toca duas vezes seguidas em Acertou, com menos de 500 ms entre os toques
- **THEN** o contador mostra 3
- **AND** a palavra que apareceu depois do primeiro toque continua na tela e não aparece como acerto no histórico

#### Scenario: Acertou logo depois de Pular
- **WHEN** o jogador toca em Pular e, menos de 500 ms depois de a nova palavra aparecer, toca em Acertou
- **THEN** o contador não muda e a palavra nova continua na tela

#### Scenario: Acertos em sequência
- **WHEN** passaram mais de 500 ms desde que a palavra apareceu e o jogador toca em Acertou
- **THEN** o acerto conta normalmente e uma nova palavra é exibida

#### Scenario: Aparência durante o bloqueio
- **WHEN** uma nova palavra acabou de aparecer
- **THEN** o botão Acertou aparece com o estilo normal, e não com o estilo de desabilitado
