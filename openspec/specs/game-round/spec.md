# game-round Specification

## Purpose

Define o ciclo de uma rodada da partida: anúncio da dupla, exibição da palavra, acertar e pular, cronômetro, fim da rodada, pontuação, placar e o feedback no aparelho.

## Requirements

### Requirement: Ordem das duplas
A partida SHALL percorrer todas as duplas ordenadas (quem dá dicas, quem adivinha) formadas pelos jogadores, na ordem em que foram cadastrados: para cada jogador, ele dá dicas para cada um dos outros, na ordem de cadastro. Depois da última dupla, a ordem SHALL recomeçar da primeira.

#### Scenario: Três jogadores
- **WHEN** os jogadores cadastrados são Ana, Beto e Caio, nessa ordem
- **THEN** as duplas seguem a ordem Ana→Beto, Ana→Caio, Beto→Ana, Beto→Caio, Caio→Ana, Caio→Beto
- **AND** depois de Caio→Beto a próxima dupla é Ana→Beto

### Requirement: Anúncio da dupla
Antes de cada rodada, o jogo SHALL mostrar com destaque quem dá as dicas e quem adivinha, junto com o placar atual e um botão para começar a rodada. O cronômetro SHALL ficar parado nessa tela.

#### Scenario: Início da rodada
- **WHEN** a dupla da vez é Ana dando dicas para Beto
- **THEN** a tela mostra Ana como quem dá as dicas e Beto como quem adivinha
- **AND** o cronômetro mostra o tempo escolhido, parado

### Requirement: Exibição da palavra
Durante a rodada, a palavra sorteada SHALL ser o elemento de maior destaque da tela, legível a um braço de distância, e SHALL caber na largura da tela sem cortar letras, quebrando linha ou reduzindo o tamanho se for longa. O placar geral SHALL NOT aparecer nessa tela.

#### Scenario: Palavra longa em tela estreita
- **WHEN** a palavra é longa (ex.: "Abastecedor") numa tela de 320px
- **THEN** a palavra aparece inteira, sem rolagem horizontal

#### Scenario: Placar oculto
- **WHEN** a rodada está em andamento
- **THEN** a lista de pontuação dos jogadores não aparece

### Requirement: Acertar
Durante a rodada, tocar em Acertou SHALL somar 1 ao contador de acertos da rodada, exibido na tela, e sortear uma nova palavra ainda não sorteada na partida.

#### Scenario: Acerto
- **WHEN** o contador está em 2 e o jogador toca em Acertou
- **THEN** o contador mostra 3 com uma animação de incremento
- **AND** uma nova palavra é exibida

### Requirement: Pular com bloqueio
O botão Pular SHALL ficar bloqueado nos primeiros 5 segundos de cada palavra, mostrando quantos segundos faltam e o progresso até liberar. Depois disso, tocar em Pular SHALL sortear uma nova palavra sem alterar o contador de acertos.

#### Scenario: Pular bloqueado
- **WHEN** a palavra acabou de aparecer
- **THEN** o botão Pular está desabilitado e mostra a contagem de 5 até 1

#### Scenario: Pular liberado
- **WHEN** passaram 5 segundos desde que a palavra apareceu e o jogador toca em Pular
- **THEN** uma nova palavra é exibida, o contador de acertos não muda e o bloqueio de 5 segundos recomeça

### Requirement: Cronômetro com progresso
Durante a rodada, o tempo restante SHALL aparecer no formato MM:SS e como uma barra de progresso que diminui a cada segundo. Nos últimos 10 segundos, o cronômetro SHALL ganhar destaque visual de alerta.

#### Scenario: Contagem normal
- **WHEN** a rodada de 1min começa
- **THEN** o cronômetro mostra 01:00, a barra está cheia e ambos diminuem a cada segundo

#### Scenario: Últimos segundos
- **WHEN** faltam 10 segundos ou menos
- **THEN** o cronômetro e a barra mudam para a cor de alerta

### Requirement: Fim da rodada e pontuação
Quando o tempo chega a zero, a rodada SHALL terminar e mostrar os pontos da rodada: quem adivinhou ganha o número de acertos e quem deu as dicas ganha metade, arredondada para baixo. Tocar em Continuar SHALL somar esses pontos ao placar e ir para o anúncio da próxima dupla.

#### Scenario: Cinco acertos
- **WHEN** Ana deu dicas para Beto e a rodada terminou com 5 acertos
- **THEN** a tela mostra Beto com 5 pontos e Ana com 2 pontos
- **AND** depois de Continuar, o placar de Beto aumenta 5 e o de Ana aumenta 2

#### Scenario: Sem ações depois do fim
- **WHEN** o tempo chegou a zero
- **THEN** os botões Acertou e Pular não estão mais disponíveis

### Requirement: Histórico de palavras da rodada
A tela de fim da rodada SHALL mostrar, junto com os pontos, todas as palavras exibidas na rodada, na ordem em que apareceram, cada uma marcada como acerto ou erro. É acerto a palavra em que se tocou Acertou; é erro a palavra pulada ou a que estava na tela quando o tempo acabou. A marcação SHALL usar ícone e cor, não só cor. O histórico SHALL conter apenas as palavras da rodada que terminou.

#### Scenario: Acertos e pulos
- **WHEN** na rodada apareceram Cacto (acertou), Ducha (pulou) e Ferro (acertou), e o tempo acabou com Gaivota na tela
- **THEN** a tela de fim mostra Cacto, Ducha, Ferro e Gaivota, nessa ordem
- **AND** Cacto e Ferro aparecem como acerto, Ducha e Gaivota como erro

#### Scenario: Contagem coerente com os pontos
- **WHEN** a rodada terminou com 5 acertos
- **THEN** o histórico tem exatamente 5 palavras marcadas como acerto

#### Scenario: Rodada seguinte começa limpa
- **WHEN** o jogador toca em Continuar e a próxima rodada termina
- **THEN** o histórico mostra apenas as palavras dessa nova rodada

#### Scenario: Leitor de tela
- **WHEN** um leitor de tela percorre o histórico
- **THEN** cada palavra é anunciada junto com o resultado (acertou ou errou)

### Requirement: Placar
O placar SHALL listar todos os jogadores do maior para o menor número de pontos, destacando quem está na liderança, e SHALL aparecer no anúncio da dupla e no fim da rodada.

#### Scenario: Ordenação
- **WHEN** Ana tem 3 pontos, Beto 7 e Caio 5
- **THEN** o placar mostra Beto, Caio e Ana, nessa ordem, com Beto destacado

### Requirement: Feedback tátil
Em aparelhos que suportam vibração, o jogo SHALL vibrar brevemente ao registrar um acerto e com um padrão distinto quando o tempo acaba. Onde não houver suporte, o jogo SHALL funcionar normalmente, sem erros.

#### Scenario: Acerto num Android
- **WHEN** o jogador toca em Acertou num aparelho com vibração
- **THEN** o aparelho vibra brevemente

#### Scenario: Navegador sem vibração
- **WHEN** o jogo roda num navegador sem suporte a vibração (ex.: Safari do iOS)
- **THEN** acertos e fim do tempo funcionam normalmente, sem erro no console

### Requirement: Tela acesa durante a rodada
Enquanto uma rodada está em andamento, o jogo SHALL pedir ao navegador que mantenha a tela acesa, quando houver suporte, e SHALL liberar esse pedido quando a rodada termina. Se o pedido falhar ou não houver suporte, o jogo SHALL continuar normalmente.

#### Scenario: Rodada longa
- **WHEN** uma rodada de 3min está em andamento num navegador com suporte
- **THEN** a tela não apaga sozinha durante a rodada

#### Scenario: Fim da rodada
- **WHEN** a rodada termina
- **THEN** o jogo libera o pedido e a tela volta a seguir o tempo de bloqueio do aparelho
