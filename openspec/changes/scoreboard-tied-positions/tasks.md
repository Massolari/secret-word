# Tasks

## 1. Posição pela pontuação

- [ ] 1.1 Em `viewScoreboard` (`src/Main.elm`), extrair a lista de pontuações no `let`, reaproveitá-la em `topScore` e calcular a posição de cada jogador como `1 + List.length (List.filter (\s -> s > player.score) scores)` (Decisão 2 do design). Trocar `List.indexedMap` por `List.map` e exibir essa posição no lugar de `index + 1`, mantendo o troféu para `leader`. Verificar com `npx elm make src/Main.elm --output=/dev/null`.

## 2. Verificação integrada

- [ ] 2.1 Rodar `yarn start`, jogar com quatro jogadores até o placar ficar `[5, 3, 3, 1]` e verificar os cenários "Empate fora da liderança" e "Posição depois de um empate" do spec: 🏆, 2, 2 e 4, no anúncio da dupla e no fim da rodada.
- [ ] 2.2 Verificar os cenários "Empate na liderança" (dois jogadores com a maior pontuação aparecem com troféu e destaque, o seguinte na posição 3) e "Ninguém pontuou" (no primeiro anúncio, todos na posição 1, sem troféu nem destaque), e que o cenário "Ordenação" continua valendo.
