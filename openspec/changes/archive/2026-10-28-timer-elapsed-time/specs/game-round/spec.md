## MODIFIED Requirements

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

## ADDED Requirements

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

#### Scenario: Bloqueio do Pular na pausa
- **WHEN** a rodada pausou com o botão Pular faltando 3 segundos para liberar
- **THEN** depois de retomar, ainda faltam 3 segundos para o Pular liberar
