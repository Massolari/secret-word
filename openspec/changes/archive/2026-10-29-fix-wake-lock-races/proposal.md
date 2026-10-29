# Proposal

## Why

O pedido para manter a tela acesa (wake lock) tem duas condições de corrida em `src/index.js`. Na primeira, um segundo pedido feito enquanto o primeiro ainda está pendente cria um segundo lock, e o primeiro nunca é liberado. Na segunda, o evento de liberação de um lock antigo apaga a referência ao lock atual, que também fica sem ser liberado. Nos dois casos a tela pode ficar acesa depois do fim da rodada, gastando bateria sem que o jogo consiga desfazer o pedido.

## What Changes

- No máximo um pedido de tela acesa fica em andamento por vez. Pedidos feitos enquanto um está pendente (rodada recomeçada logo depois do fim, volta ao app logo depois de começar a rodada) não criam um segundo lock.
- Quando um pedido pendente termina, o jogo confere se a rodada ainda quer a tela acesa. Se não quiser mais, libera o lock na hora.
- O evento de liberação de um lock só limpa a referência se ele ainda for o lock atual. Um lock antigo liberado com atraso deixa de afetar o atual.
- Continua tudo silencioso: falha ou falta de suporte não gera erro e não interrompe o jogo.
- Sem mudança na interface nem nos momentos em que a rodada pede ou libera a tela acesa (`Play` e fim do tempo em `Main.elm`).

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

- `game-round`: o requisito "Tela acesa durante a rodada" passa a garantir que a tela volta a apagar sozinha depois do fim da rodada mesmo quando rodadas começam e terminam em sequência rápida ou o app sai e volta da tela durante um pedido, e que nunca há mais de um pedido ativo.

## Impact

- `src/index.js`: `requestWakeLock`, `releaseWakeLock`, o listener de `release` e o listener de `visibilitychange`.
- `src/Main.elm`: sem mudança (a porta `keepAwake` continua igual).
- Sem dependências novas. O projeto não tem testes de JavaScript, então a verificação é manual, no navegador, com um `navigator.wakeLock` simulado para forçar os atrasos.
