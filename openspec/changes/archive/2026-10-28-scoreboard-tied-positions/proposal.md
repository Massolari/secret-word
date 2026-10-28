# Proposal

## Why

No placar, jogadores com a mesma pontuação fora da liderança aparecem com posições diferentes: com `[5, 3, 3]` o placar mostra 🏆, 2 e 3, como se o terceiro estivesse atrás do segundo. A posição vem da ordem na lista, não da pontuação, e isso passa uma classificação falsa aos jogadores.

## What Changes

- A posição de cada jogador no placar passa a depender só da pontuação: jogadores com a mesma pontuação mostram a mesma posição.
- A numeração segue a classificação de competição ("1, 2, 2, 4"): a posição de um jogador é 1 mais o número de jogadores com mais pontos que ele. Com `[5, 3, 3, 1]` o placar mostra 🏆, 2, 2 e 4.
- A liderança continua como hoje: todos os que têm a maior pontuação (maior que zero) aparecem com o troféu e destacados. Quando ninguém pontuou, ninguém é destacado e todos aparecem na posição 1.
- A ordem das linhas não muda: do maior para o menor número de pontos, com os empatados na ordem atual.

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

- `game-round`: o requisito "Placar" passa a definir a posição exibida de cada jogador e o tratamento de empates, incluindo empate na liderança e placar zerado.

## Impact

- `src/Main.elm`: `viewScoreboard` passa a calcular a posição pela pontuação em vez de usar o índice da lista.
- Sem mudança em `src/index.css`, `src/Icons.elm`, na pontuação ou no modelo. Sem dependências novas.
