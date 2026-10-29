## ADDED Requirements

### Requirement: Voltar à preparação depois da partida
O placar final SHALL oferecer o botão Nova partida, que leva à tela de preparação. A preparação SHALL manter os jogadores da partida, na mesma ordem, e o tempo de rodada escolhido, permitindo adicionar, remover jogadores e trocar o tempo como antes. A partida seguinte SHALL começar do zero: todos com 0 pontos, a ordem das duplas a partir da primeira e o sorteio de palavras sem considerar as que saíram na partida anterior.

#### Scenario: Mesmos jogadores e tempo
- **WHEN** a partida com Ana, Beto e Caio e rodadas de 2min foi encerrada e o jogador toca em Nova partida
- **THEN** a tela de preparação mostra Ana, Beto e Caio, nessa ordem, com 2min selecionado

#### Scenario: Mudar o grupo
- **WHEN** na preparação depois de uma partida o jogador remove Caio, adiciona Davi e toca em Começar
- **THEN** a nova partida tem Ana, Beto e Davi, e a primeira dupla é Ana dando dicas para Beto

#### Scenario: Placar zerado
- **WHEN** a partida anterior terminou com Beto com 9 pontos e o grupo começa uma nova partida
- **THEN** o placar do primeiro anúncio mostra todos os jogadores com 0 pontos

#### Scenario: Ordem das duplas recomeça
- **WHEN** a partida anterior foi encerrada no anúncio de Beto dando dicas para Caio e o grupo começa uma nova partida com os mesmos jogadores
- **THEN** a primeira dupla anunciada é Ana dando dicas para Beto

#### Scenario: Sorteio recomeça
- **WHEN** uma palavra saiu na partida anterior e o grupo começa uma nova partida
- **THEN** essa palavra pode ser sorteada na nova partida
