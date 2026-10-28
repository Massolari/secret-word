# Proposal

## Why

O botão principal do rodapé de cada tela fica no mesmo lugar do botão da tela anterior. Quando a tela troca, um toque que era para a tela anterior acaba acionando a seguinte. Toques rápidos em Acertou no fim do tempo caem em Continuar e pulam a tela de resultado. Um toque duplo em Continuar inicia a rodada sem o anúncio da dupla. Um toque duplo em Começar rodada pode marcar um acerto numa palavra que ninguém viu. Esses erros mudam o placar ou escondem informação, e o jogador não tem como desfazer.

## What Changes

- Logo depois de cada troca de tela da partida, os botões do rodapé da nova tela ignoram toques por um curto período (500 ms). As trocas cobertas são:
  - preparação → anúncio da dupla (Começar → Começar rodada);
  - anúncio → rodada (Começar rodada → Acertou / Pular);
  - rodada → fim da rodada, quando o tempo acaba (Acertou / Pular → Continuar);
  - fim da rodada → anúncio (Continuar → Começar rodada).
- Depois desse período os botões voltam a responder normalmente. Durante o bloqueio a aparência dos botões não muda, para não piscar a cada troca de tela.
- Cada ação passa a valer só na tela a que pertence. Um Acertou ou Pular que chegue depois do fim da rodada é ignorado, assim como um segundo Continuar ou Começar rodada depois da troca.
- A troca preparação → anúncio não está na lista do roadmap. Ela foi incluída porque tem o mesmo problema: um toque duplo em Começar pularia o anúncio da primeira dupla.

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

- `game-round`: novo requisito "Proteção contra toques na troca de tela", que cobre o bloqueio curto dos botões do rodapé e o descarte de ações que pertencem a uma tela que já saiu. Os requisitos existentes não mudam. O bloqueio de 5 s do Pular continua igual e é independente deste.

## Impact

- `src/Main.elm`: estado novo no `Model` para o bloqueio, uma mensagem que libera o bloqueio, e o bloqueio ativado em `Start`, `Play`, `Restart` e no `Tick` que leva a `GameOver`. `Play`, `Restart`, `Correct` e `Jump` passam a checar o status do jogo e o bloqueio antes de agir.
- Sem mudanças em CSS, `index.js` ou dependências.
- Fora do escopo: o toque duplo em Acertou durante a rodada, que é a change `guard-correct-double-tap`, e o alinhamento do bloqueio do Pular, que é a change `skip-lock-per-word`.
