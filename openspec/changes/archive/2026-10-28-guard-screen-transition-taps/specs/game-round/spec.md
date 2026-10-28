# Spec Delta

## ADDED Requirements

### Requirement: Proteção contra toques na troca de tela
Depois de cada troca de tela da partida (preparação → anúncio, anúncio → rodada, rodada → fim, fim → anúncio), os botões do rodapé da nova tela SHALL ignorar toques e teclas nos primeiros 500 ms e responder normalmente depois. A aparência dos botões SHALL NOT mudar por causa desse bloqueio. Uma ação SHALL valer só na tela a que pertence. Se a tela já trocou, a ação SHALL ser ignorada.

#### Scenario: Acertou no fim do tempo
- **WHEN** o tempo acaba enquanto o jogador toca repetidamente em Acertou, e um toque cai em Continuar até 500 ms depois da troca
- **THEN** a tela de fim da rodada continua visível e o placar não muda
- **AND** os pontos e o histórico da rodada não contam acertos depois do fim do tempo

#### Scenario: Toque duplo em Continuar
- **WHEN** o jogador toca duas vezes seguidas em Continuar
- **THEN** a tela de anúncio da próxima dupla aparece e continua visível
- **AND** a rodada não começa e o placar recebe os pontos da rodada uma vez só

#### Scenario: Toque duplo em Começar rodada
- **WHEN** o jogador toca duas vezes seguidas em Começar rodada
- **THEN** a rodada começa com o contador de acertos em 0 e a primeira palavra não aparece como acerto no histórico

#### Scenario: Toque duplo em Começar
- **WHEN** com jogadores suficientes, o jogador toca duas vezes seguidas em Começar na preparação
- **THEN** a tela de anúncio da primeira dupla aparece e continua visível

#### Scenario: Botão liberado depois do bloqueio
- **WHEN** passaram mais de 500 ms desde a troca de tela e o jogador toca no botão do rodapé
- **THEN** a ação do botão acontece normalmente

#### Scenario: Aparência durante o bloqueio
- **WHEN** a tela acabou de trocar
- **THEN** os botões do rodapé aparecem com o estilo normal, e não com o estilo de desabilitado
