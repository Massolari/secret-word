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
Durante a rodada, tocar em Acertou SHALL somar 1 ao contador de acertos da rodada, exibido na tela, e sortear uma nova palavra conforme o requisito "Sorteio de palavras". Depois de um toque em Acertou ou Pular, o botão Acertou SHALL ignorar toques e teclas até 500 ms depois de a nova palavra aparecer. Um toque ignorado SHALL NOT ter efeito: não soma acerto, não entra no histórico, não troca a palavra e não vibra. A aparência do botão SHALL NOT mudar por causa desse bloqueio.

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

### Requirement: Sorteio de palavras
Sempre que a rodada precisa de uma nova palavra (início da rodada, Acertou ou Pular), o jogo SHALL sortear, com chances iguais, uma palavra da lista que ainda não saiu na partida. Toda palavra da lista SHALL poder ser sorteada. O sorteio SHALL terminar sem travar o app, por mais palavras que já tenham saído.

#### Scenario: Sem repetição na partida
- **WHEN** uma palavra já saiu em qualquer rodada da partida e ainda há palavras da lista que não saíram
- **THEN** essa palavra não é sorteada de novo

#### Scenario: Todas as palavras podem sair
- **WHEN** a partida sorteia palavras até passar pela lista inteira
- **THEN** cada palavra da lista, incluindo a primeira e a última, sai exatamente uma vez antes de qualquer repetição

#### Scenario: Poucas palavras restantes
- **WHEN** resta só uma palavra da lista que ainda não saiu e o jogador toca em Acertou
- **THEN** essa palavra aparece na hora, sem o app congelar

### Requirement: Lista esgotada
Quando todas as palavras da lista já saíram na partida, o jogo SHALL começar um novo ciclo de sorteio com a lista inteira, sem aviso e sem interromper a rodada. A palavra que está na tela SHALL ficar de fora do primeiro sorteio do novo ciclo, a menos que ela seja a única palavra da lista.

#### Scenario: Última palavra acertada
- **WHEN** a palavra na tela é a última da lista que ainda não tinha saído e o jogador toca em Acertou
- **THEN** o contador soma 1 e aparece uma nova palavra, diferente da que estava na tela
- **AND** a rodada e o cronômetro continuam normalmente

#### Scenario: Novo ciclo sem repetição
- **WHEN** um novo ciclo começou
- **THEN** nenhuma palavra se repete até que a lista inteira tenha saído de novo

### Requirement: Cronômetro com progresso
Durante a rodada, o tempo restante SHALL aparecer no formato MM:SS e como uma barra de progresso que diminui a cada segundo. O tempo restante SHALL ser calculado pelo tempo real decorrido com a rodada em andamento, de modo que atrasos do navegador não alonguem a rodada. Nos últimos 10 segundos, o cronômetro SHALL ganhar destaque visual de alerta.

#### Scenario: Contagem normal
- **WHEN** a rodada de 1min começa
- **THEN** o cronômetro mostra 01:00, a barra está cheia e ambos diminuem a cada segundo

#### Scenario: Últimos segundos
- **WHEN** faltam 10 segundos ou menos
- **THEN** o cronômetro e a barra mudam para a cor de alerta

#### Scenario: Navegador lento
- **WHEN** uma rodada de 30s fica o tempo todo em primeiro plano, mas o navegador entrega as atualizações com atraso
- **THEN** a rodada termina 30 segundos depois de começar, com margem de até 1 segundo

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
Enquanto uma rodada está em andamento, o jogo SHALL pedir ao navegador que mantenha a tela acesa, quando houver suporte, e SHALL liberar esse pedido quando a rodada termina ou é pausada. Ao retomar a rodada, o jogo SHALL pedir de novo. Se o pedido falhar ou não houver suporte, o jogo SHALL continuar normalmente.

#### Scenario: Rodada longa
- **WHEN** uma rodada de 3min está em andamento num navegador com suporte
- **THEN** a tela não apaga sozinha durante a rodada

#### Scenario: Fim da rodada
- **WHEN** a rodada termina
- **THEN** o jogo libera o pedido e a tela volta a seguir o tempo de bloqueio do aparelho

#### Scenario: Rodada pausada
- **WHEN** a rodada está pausada
- **THEN** a tela segue o tempo de bloqueio do aparelho
- **AND** ao retomar a rodada, a tela volta a ficar acesa

### Requirement: Pausa ao sair da tela
Quando o jogo deixa de ficar visível durante a rodada (aba oculta, outro app em primeiro plano ou tela bloqueada), a rodada SHALL pausar. O tempo em que o jogo ficou fora da tela SHALL NOT ser descontado. Se a página ficar parada sem aviso por mais de alguns segundos, o jogo SHALL tratar isso como uma saída da tela. Fora da rodada (anúncio da dupla e fim da rodada), sair da tela SHALL NOT mudar nada.

#### Scenario: Tela bloqueada
- **WHEN** faltam 40 segundos, o aparelho é bloqueado por 2 minutos e depois desbloqueado
- **THEN** o jogo mostra a rodada pausada com 00:40 restantes

#### Scenario: Troca de aba
- **WHEN** faltam 15 segundos e o jogador abre outra aba por 30 segundos
- **THEN** ao voltar, a rodada está pausada com 00:15 restantes e não terminou

#### Scenario: Página congelada sem aviso
- **WHEN** faltam 20 segundos e o navegador congela a página por 1 minuto sem avisar que ela saiu da tela
- **THEN** ao voltar a rodar, o jogo mostra a rodada pausada com cerca de 00:20 restantes

#### Scenario: Fora da rodada
- **WHEN** o jogo está no anúncio da dupla ou no fim da rodada e o jogador troca de aba e volta
- **THEN** a mesma tela continua aberta, sem pausa

### Requirement: Retomar rodada pausada
A tela de rodada pausada SHALL indicar que a rodada está pausada, mostrar a dupla e o tempo restante parado, esconder a palavra e não oferecer Acertou nem Pular. Tocar em Continuar rodada SHALL retomar a contagem a partir do tempo que restava, com a mesma palavra e o mesmo contador de acertos. A rodada SHALL NOT retomar sozinha quando o jogo volta a ficar visível.

#### Scenario: Volta para o jogo
- **WHEN** a rodada pausou e o jogador volta para o jogo
- **THEN** a tela mostra "Rodada pausada", a dupla e o tempo restante parado
- **AND** a palavra, Acertou e Pular não aparecem

#### Scenario: Retomar
- **WHEN** a rodada pausou com 00:25 restantes, 3 acertos e a palavra Cacto na tela, e o jogador toca em Continuar rodada
- **THEN** a palavra Cacto volta a aparecer, o contador mostra 3 e o cronômetro volta a contar a partir de 00:25

### Requirement: Proteção contra toques na troca de tela
Depois de cada troca de tela da partida (preparação → anúncio, anúncio → rodada, rodada → pausa, pausa → rodada, rodada → fim, fim → anúncio), os botões do rodapé da nova tela SHALL ignorar toques e teclas nos primeiros 500 ms e responder normalmente depois. A aparência dos botões SHALL NOT mudar por causa desse bloqueio. Uma ação SHALL valer só na tela a que pertence. Se a tela já trocou, a ação SHALL ser ignorada.

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

#### Scenario: Botão liberado depois do bloqueio
- **WHEN** passaram mais de 500 ms desde a troca de tela e o jogador toca no botão do rodapé
- **THEN** a ação do botão acontece normalmente

#### Scenario: Aparência durante o bloqueio
- **WHEN** a tela acabou de trocar
- **THEN** os botões do rodapé aparecem com o estilo normal, e não com o estilo de desabilitado
