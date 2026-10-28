# Proposal

## Why

O cronômetro da rodada não mede tempo: cada `Tick` subtrai 1000 ms fixos, sem olhar para o `Time.Posix` que recebe. Quando a aba fica em segundo plano ou a tela bloqueia, o navegador atrasa ou suspende os ticks, e a rodada fica parada ou lenta sem que ninguém perceba. Na volta, ela continua de onde estava, como se nada tivesse acontecido. Mesmo em primeiro plano, ticks atrasados alongam a rodada.

## What Changes

- O tempo restante da rodada passa a ser calculado pelo tempo real decorrido entre os ticks, e não pela contagem de ticks. Atrasos do navegador deixam de alongar a rodada.
- Quando o app vai para segundo plano durante a rodada (aba oculta, app trocado, tela bloqueada), a rodada **pausa de forma explícita**. O tempo fora da tela não é descontado.
- Nova tela de rodada pausada: mostra o tempo restante parado, esconde a palavra, não oferece Acertou nem Pular e tem um botão para retomar. A rodada só volta a correr com esse toque.
- Se o navegador congelar a página sem avisar que ela saiu da tela, um intervalo longo entre dois ticks é tratado como interrupção: a rodada pausa e esse intervalo não é descontado.
- Durante a pausa, o pedido de tela acesa é liberado; ao retomar, ele é feito de novo.
- O bloqueio do botão Pular continua contando pelos segundos do cronômetro, como hoje, e fica parado durante a pausa. A mudança dele para contar a partir da palavra fica para a change `skip-lock-per-word`.

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

- `game-round`: o requisito "Cronômetro com progresso" passa a exigir que a contagem siga o tempo real; o requisito "Tela acesa durante a rodada" passa a liberar o pedido durante a pausa; novos requisitos "Pausa ao sair da tela" e "Retomar rodada pausada".

## Impact

- `src/Main.elm`: novo estado `GamePaused` em `GameStatus`; novo campo no `GameModel` com o instante do último tick; `Play` e o novo `Resume` capturam a hora atual; `Tick` passa a usar o tempo decorrido; nova mensagem para a mudança de visibilidade; `subscriptions` passa a ouvir `Browser.Events.onVisibilityChange` e a tickar com mais frequência; nova `viewPaused`; o texto do tempo arredonda os segundos para cima.
- `src/index.css`: estilo da tela de pausa, reaproveitando `.card` e `.round`.
- `src/index.js`: sem mudança. O port `keepAwake` já cobre liberar e pedir de novo a tela acesa.
- Sem dependências novas (`Browser.Events` faz parte de `elm/browser`, que já é dependência). Sem mudança na pontuação.
