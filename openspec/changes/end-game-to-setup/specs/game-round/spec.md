## ADDED Requirements

### Requirement: Encerrar partida
As telas de anúncio da dupla, de rodada pausada e de fim da rodada SHALL oferecer um botão Encerrar partida no rodapé, abaixo da ação principal e com menos destaque visual que ela. A rodada em andamento SHALL NOT oferecer esse botão. Tocar em Encerrar partida SHALL abrir a confirmação para encerrar, sem mudar o placar.

#### Scenario: Anúncio da dupla
- **WHEN** a tela mostra o anúncio da dupla
- **THEN** o rodapé mostra Começar rodada como ação principal e, abaixo, o botão Encerrar partida com menos destaque

#### Scenario: Rodada em andamento
- **WHEN** a rodada está em andamento
- **THEN** o botão Encerrar partida não aparece

#### Scenario: Abrir a confirmação
- **WHEN** o jogador toca em Encerrar partida no fim da rodada
- **THEN** aparece a confirmação para encerrar e o placar ainda não mudou

### Requirement: Confirmação para encerrar
A confirmação SHALL perguntar se o grupo quer encerrar a partida e oferecer Encerrar e Voltar ao jogo, sem usar diálogos do navegador. Voltar ao jogo SHALL levar à tela de onde a confirmação foi aberta, no mesmo estado: mesma dupla, mesmo placar e, na pausa, a rodada pausada com o mesmo tempo, palavra e acertos. Encerrar SHALL levar ao placar final.

#### Scenario: Voltar ao jogo do anúncio
- **WHEN** a confirmação foi aberta no anúncio de Ana dando dicas para Beto e o jogador toca em Voltar ao jogo
- **THEN** a tela volta ao anúncio de Ana dando dicas para Beto, com o mesmo placar

#### Scenario: Voltar ao jogo da pausa
- **WHEN** a confirmação foi aberta com a rodada pausada em 00:25, 3 acertos e a palavra Cacto, e o jogador toca em Voltar ao jogo
- **THEN** a tela de rodada pausada volta com 00:25 restantes
- **AND** ao tocar em Continuar rodada, aparecem a palavra Cacto e o contador em 3

#### Scenario: Voltar ao jogo do fim da rodada
- **WHEN** a confirmação foi aberta no fim de uma rodada com 5 acertos e o jogador toca em Voltar ao jogo
- **THEN** a tela de fim da rodada volta com os mesmos pontos e histórico
- **AND** ao tocar em Continuar, os pontos da rodada entram no placar uma vez só

#### Scenario: Sem diálogo do navegador
- **WHEN** o jogador toca em Encerrar partida
- **THEN** a confirmação aparece como uma tela do jogo, e não como um alerta ou confirmação nativa do navegador

### Requirement: Placar final
Ao confirmar o encerramento, o jogo SHALL mostrar a tela de placar final, com um título de fim de jogo, os vencedores e o placar completo. Encerrando no fim da rodada, os pontos dessa rodada SHALL entrar no placar final. Encerrando com a rodada pausada, a rodada interrompida SHALL NOT dar pontos. São vencedores os jogadores com a maior pontuação, quando ela é maior que zero. Se ninguém pontuou, a tela SHALL dizer que não houve vencedor.

#### Scenario: Encerrar no fim da rodada
- **WHEN** Ana tem 3 pontos e Beto 4, Ana deu dicas para Beto numa rodada que terminou com 5 acertos e o grupo encerra a partida na tela de fim dessa rodada
- **THEN** o placar final mostra Beto com 9 pontos e Ana com 5
- **AND** Beto aparece como vencedor

#### Scenario: Encerrar no anúncio
- **WHEN** Ana tem 5 pontos e Beto 2 e o grupo encerra a partida no anúncio da dupla
- **THEN** o placar final mostra Ana com 5 pontos e Beto com 2, e Ana como vencedora

#### Scenario: Encerrar com a rodada pausada
- **WHEN** Ana tem 2 pontos e Beto 2, a rodada pausou com 4 acertos e o grupo encerra a partida
- **THEN** o placar final mostra Ana e Beto com 2 pontos cada
- **AND** a tela indica empate entre Ana e Beto

#### Scenario: Ninguém pontuou
- **WHEN** todos os jogadores têm 0 pontos e o grupo encerra a partida
- **THEN** a tela de placar final diz que não houve vencedor e nenhum jogador aparece com o troféu

#### Scenario: Sem pausa no placar final
- **WHEN** a tela de placar final está aberta e o jogador troca de aba e volta
- **THEN** a mesma tela continua aberta

## MODIFIED Requirements

### Requirement: Placar
O placar SHALL listar todos os jogadores do maior para o menor número de pontos, destacando quem está na liderança, e SHALL aparecer no anúncio da dupla, no fim da rodada e no placar final. Cada jogador SHALL aparecer com uma posição: 1 mais o número de jogadores com mais pontos que ele, de modo que jogadores com a mesma pontuação mostram a mesma posição. Os jogadores com a maior pontuação, quando ela é maior que zero, SHALL aparecer destacados e com o troféu no lugar do número.

#### Scenario: Ordenação
- **WHEN** Ana tem 3 pontos, Beto 7 e Caio 5
- **THEN** o placar mostra Beto, Caio e Ana, nessa ordem, com Beto destacado

#### Scenario: Empate fora da liderança
- **WHEN** Ana tem 5 pontos, Beto 3 e Caio 3
- **THEN** o placar mostra Ana com o troféu, e Beto e Caio ambos na posição 2

#### Scenario: Posição depois de um empate
- **WHEN** Ana tem 5 pontos, Beto 3, Caio 3 e Davi 1
- **THEN** Beto e Caio aparecem na posição 2 e Davi na posição 4

#### Scenario: Empate na liderança
- **WHEN** Ana tem 5 pontos, Beto 5 e Caio 3
- **THEN** Ana e Beto aparecem destacados e com o troféu
- **AND** Caio aparece na posição 3

#### Scenario: Ninguém pontuou
- **WHEN** todos os jogadores têm 0 pontos
- **THEN** nenhum jogador aparece destacado ou com o troféu
- **AND** todos aparecem na posição 1

#### Scenario: Placar final
- **WHEN** a partida foi encerrada com Ana com 3 pontos, Beto 7 e Caio 5
- **THEN** o placar final mostra Beto, Caio e Ana, nessa ordem, com Beto destacado e com o troféu

### Requirement: Proteção contra toques na troca de tela
Depois de cada troca de tela da partida (preparação → anúncio, anúncio → rodada, rodada → pausa, pausa → rodada, rodada → fim, fim → anúncio, anúncio, pausa ou fim → confirmação para encerrar, confirmação → tela anterior, confirmação → placar final, placar final → preparação), os botões do rodapé da nova tela SHALL ignorar toques e teclas nos primeiros 500 ms e responder normalmente depois. A aparência dos botões SHALL NOT mudar por causa desse bloqueio. Uma ação SHALL valer só na tela a que pertence. Se a tela já trocou, a ação SHALL ser ignorada.

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

#### Scenario: Toque duplo em Continuar rodada
- **WHEN** a rodada está pausada e o jogador toca duas vezes seguidas em Continuar rodada
- **THEN** a rodada volta com a mesma palavra, o mesmo contador de acertos e o mesmo tempo restante

#### Scenario: Toque duplo em Encerrar partida
- **WHEN** o jogador toca duas vezes seguidas em Encerrar partida
- **THEN** a confirmação para encerrar aparece e continua visível, sem ir para o placar final

#### Scenario: Toque duplo em Encerrar
- **WHEN** na confirmação, o jogador toca duas vezes seguidas em Encerrar
- **THEN** o placar final aparece e continua visível, sem voltar à preparação

#### Scenario: Toque duplo em Nova partida
- **WHEN** no placar final, o jogador toca duas vezes seguidas em Nova partida
- **THEN** a tela de preparação aparece e continua visível, sem começar uma nova partida

#### Scenario: Botão liberado depois do bloqueio
- **WHEN** passaram mais de 500 ms desde a troca de tela e o jogador toca no botão do rodapé
- **THEN** a ação do botão acontece normalmente

#### Scenario: Aparência durante o bloqueio
- **WHEN** a tela acabou de trocar
- **THEN** os botões do rodapé aparecem com o estilo normal, e não com o estilo de desabilitado
