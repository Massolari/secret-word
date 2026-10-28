# Proposal

## Why

O visual atual é o Material Design padrão sem personalização: campos e botões soltos numa coluna centralizada, sem hierarquia, sem movimento e pensado para desktop. O jogo é jogado em grupo, com um celular passando de mão em mão, então a tela de jogo precisa ser legível de longe, ter botões grandes ao alcance do polegar e dar feedback claro a cada ação. Hoje nada disso acontece.

## What Changes

- **BREAKING (dependências):** remover o `aforemny/material-components-web-elm` (Elm e npm), o `mdgriffith/elm-ui` (não usado) e as fontes Roboto/Material Icons. As views passam a usar `Html` puro com componentes próprios e CSS próprio, organizado em tokens (cor, espaço, raio, sombra, duração).
- Nova identidade visual minimalista e moderna: uma fonte de interface (Google Fonts), paleta curta com uma cor de destaque, superfícies em cartão, ícones SVG inline.
- Tema claro e escuro automáticos, seguindo `prefers-color-scheme`.
- Layout mobile-first: coluna única com largura máxima, ação principal fixa na parte de baixo da tela (zona do polegar), alvos de toque de pelo menos 48px, respeito às safe areas do iPhone, sem zoom acidental em inputs e duplo toque.
- Animações com CSS: entrada de cada tela, troca de palavra, contagem de pontos, entrada e saída de jogadores na lista, botões com resposta ao toque. Tudo desativado ou reduzido com `prefers-reduced-motion`.
- Melhorias de UX sem mudar as regras do jogo:
  - Escolha do tempo da rodada por presets (30s, 1min, 1min30, 2min, 3min), com 1min já selecionado, no lugar do `input type="time"`.
  - Barra de progresso do tempo e destaque visual nos últimos 10 segundos.
  - Botão "Pular" com indicador de contagem regressiva até liberar.
  - Vibração curta ao acertar e ao terminar o tempo, quando o aparelho suportar.
  - Tela mantida acesa durante a rodada (Wake Lock), quando o navegador suportar.
  - Placar oculto enquanto a palavra está na tela, para não poluir; visível no anúncio da dupla e no fim da rodada.
- Preservado: o fluxo de telas (preparação → anúncio da dupla → palavra → fim da rodada → próxima dupla), a regra para começar (pelo menos 2 jogadores), a ordem das duplas, a pontuação (adivinhador ganha os acertos, quem dá as dicas ganha metade arredondada para baixo), o bloqueio de 5s do "Pular" e a lista de palavras.

## Capabilities

### New Capabilities
- `ui-theme`: identidade visual, tema claro/escuro, layout mobile-first, acessibilidade de toque e política de animação (incluindo movimento reduzido).
- `game-setup`: tela de preparação, com escolha do tempo da rodada, cadastro e remoção de jogadores e a regra para começar a partida.
- `game-round`: o ciclo de uma rodada, com anúncio da dupla, exibição da palavra, acertar e pular, cronômetro e seus avisos, fim da rodada, pontuação e placar, e o feedback no aparelho (vibração, tela acesa).

### Modified Capabilities

(nenhuma; `word-list` não muda)

## Impact

- `src/Main.elm`: todas as views reescritas em `Html`; imports de `Material.*` removidos; o tempo da preparação passa de `String` para segundos (`Int`); `port module` com portas para vibração e wake lock; uso de `Html.Keyed` para disparar animações.
- Novo módulo `src/Icons.elm` com os ícones SVG (o `elm/svg` passa de dependência indireta para direta).
- `src/index.css`: reescrito por completo.
- `src/index.html`: novas fontes, `viewport-fit=cover`, `theme-color` para claro e escuro.
- `src/index.js`: remove os imports do MDC; trata as portas de vibração e wake lock.
- `elm.json`, `package.json`, `yarn.lock`: remoção de `material-components-web-elm` e `mdgriffith/elm-ui`; `elm/svg` vira direta.
- Fora do escopo: mudar regras ou pontuação, persistir jogadores entre sessões, botão para encerrar a partida, PWA/instalação, correção do sorteio de palavras.
