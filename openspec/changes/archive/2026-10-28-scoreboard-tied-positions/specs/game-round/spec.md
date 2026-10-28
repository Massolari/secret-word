# Spec Delta

## MODIFIED Requirements

### Requirement: Placar
O placar SHALL listar todos os jogadores do maior para o menor número de pontos, destacando quem está na liderança, e SHALL aparecer no anúncio da dupla e no fim da rodada. Cada jogador SHALL aparecer com uma posição: 1 mais o número de jogadores com mais pontos que ele, de modo que jogadores com a mesma pontuação mostram a mesma posição. Os jogadores com a maior pontuação, quando ela é maior que zero, SHALL aparecer destacados e com o troféu no lugar do número.

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
