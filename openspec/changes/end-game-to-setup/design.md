# Design

## Context

Todo o jogo está em `src/Main.elm`. O `Model` tem `status : Status` (`Preparing | Playing`), e dentro de `Playing` o `gameModel.status : GameStatus` escolhe a tela (`GameShowingPlayers | GamePlaying | GamePaused | GameOver`). Cada tela é um `Screen` com `key`, e a troca de `key` dispara a animação de entrada.

Três mecanismos que esta change precisa respeitar:

- **Proteção de toques**: `acceptsGameMsg` associa cada mensagem de botão a *uma* tela (`Maybe GameStatus`) e recusa a mensagem se a tela não bate ou se `tapGuard.locked`. `lockTaps` trava os botões por 500 ms depois de cada troca de tela.
- **Pontuação**: os pontos da rodada só entram em `model.players` dentro de `Restart`, junto com o avanço para a próxima dupla.
- **Estado que vaza entre partidas**: `Start` usa `gameModel.currentPlayersIndex` e herda `drawnIndex`/`currentIndex` do `gameModel` existente. Hoje isso não importa porque só existe uma partida por carregamento de página.

`subscriptions` só assina tick e visibilidade em `GamePlaying`, e o pedido de tela acesa já é liberado na pausa e no fim da rodada. Nenhuma das telas novas precisa mexer nisso.

## Goals / Non-Goals

**Goals:**
- Encerrar a partida a partir de anúncio, pausa e fim da rodada, com confirmação e volta exata à tela de origem se o grupo desistir.
- Reaproveitar `viewScoreboard` e a proteção de toques existente, sem um mecanismo novo de bloqueio.
- Garantir que a partida seguinte comece sem nenhum estado da anterior.

**Non-Goals:**
- Modal sobreposto à tela atual, com foco preso e fundo esmaecido. A confirmação é uma tela inteira do fluxo.
- Encerrar durante a rodada em andamento.
- Guardar histórico de partidas ou o placar final depois de voltar à preparação.

## Decisions

### 1. Duas novas variantes de `GameStatus`

```elm
type GameStatus
    = GameShowingPlayers
    | GamePlaying
    | GamePaused
    | GameOver
    | GameConfirmingEnd GameStatus -- tela de onde a confirmação foi aberta
    | GameFinished
```

`GameConfirmingEnd origin` guarda a tela de origem, e Voltar ao jogo só restaura `status = origin`. Como confirmar não toca em mais nada do `gameModel`, a pausa volta com o mesmo `currentTime`, `word`, `score` e `history`, e o fim da rodada volta com os pontos ainda não somados.

*Alternativas:* um campo `confirmingEnd : Bool` no `GameModel`, desenhado como sobreposição em cima da tela atual. Foi descartado porque os botões da tela de baixo continuariam recebendo mensagens, e `acceptsGameMsg` teria que consultar o campo à parte. Também perderia a troca de `key` que já anima a transição. A outra alternativa era um `Status` de topo `Finished` para o placar final. Não foi escolhido porque o placar final ainda usa `gameModel` e `players` da partida, e ficar em `Playing` mantém `view`/`viewGame` como estão.

`GameStatus` continua comparável com `==`, o que `acceptsGameMsg` e `VisibilityChanged` já usam.

### 2. Mensagens novas como `GameMsg`

`AskEnd`, `CancelEnd`, `ConfirmEnd` e `NewGame` entram em `GameMsg`, e assim passam pela mesma porta de `acceptsGameMsg` que já cuida da trava de 500 ms e da tela certa. `NewGame` muda `model.status` para `Preparing`, o que `updateGame` já pode fazer porque recebe o `Model` inteiro.

`acceptsGameMsg` passa de `Maybe GameStatus` para `Maybe (GameStatus -> Bool)`, porque `AskEnd` vale em três telas e `CancelEnd`/`ConfirmEnd` valem em qualquer `GameConfirmingEnd _`:

| Mensagem | Aceita em |
|---|---|
| `Play` | `GameShowingPlayers` |
| `Correct`, `Jump` | `GamePlaying` |
| `Resume` | `GamePaused` |
| `Restart` | `GameOver` |
| `AskEnd` | `GameShowingPlayers`, `GamePaused`, `GameOver` |
| `CancelEnd`, `ConfirmEnd` | `GameConfirmingEnd _` |
| `NewGame` | `GameFinished` |

As mensagens que não vêm de botões (`Tick`, `NewWordIndex`, `VisibilityChanged`) continuam sem filtro.

### 3. Transições

Todas terminam em `lockTaps`, como as demais trocas de tela.

- `AskEnd`: `status = GameConfirmingEnd gameModel.status`.
- `CancelEnd`: com `GameConfirmingEnd origin`, `status = origin`. Não reenvia `keepAwake` nem tick. A pausa continua pausada e só `Resume` religa a rodada.
- `ConfirmEnd`: com origem `GameOver`, soma os pontos da rodada a `players` (Decisão 4). Com qualquer outra origem, `players` fica igual. Depois, `status = GameFinished`.
- `NewGame`: `status = Preparing`, `gameModel = initGame`, cada jogador com `score = 0`. `preparingModel` fica como está, então `roundSeconds`, `currentId` (ids continuam únicos) e a ordem do `Dict` se mantêm.

Zerar `gameModel` para `initGame` resolve de uma vez a ordem das duplas (`currentPlayersIndex = 0`), o sorteio (`drawnIndex = Set.empty`, `currentIndex = Nothing`) e o histórico. `Start` recalcula `playersOrder`, `time` e a dupla a partir disso.

### 4. Helper de pontuação compartilhado

A soma que hoje está dentro de `Restart` vira `addRoundPoints : GameModel -> Dict Int Player -> Dict Int Player`: quem adivinha ganha `score` e quem dá as dicas ganha `score // 2`. `Restart` e `ConfirmEnd` (origem `GameOver`) passam a usá-la, e a regra fica num lugar só. A tela de fim da rodada continua calculando o que mostra como hoje.

### 5. Telas

- **Botão Encerrar partida**: um `viewEndButton` com `btn btn--ghost btn--block` e o texto "Encerrar partida", acrescentado ao `footer` de `viewGameShowingPlayers`, `viewPaused` e `viewGameOver`, depois da ação principal. A ação principal continua sendo o botão mais destacado e o mais perto da área do polegar, como pede o `ui-theme`.
- **Confirmação** (`key = "confirm-end"`): header com o `topbar` "Palavra Secreta". No main, um `.card` com o título "Encerrar a partida?" e um texto de apoio conforme a origem:
  - fim da rodada: "Os pontos desta rodada entram no placar final."
  - pausa: "A rodada pausada não vai contar pontos."
  - anúncio: "O placar atual será o placar final."

  No footer, **Encerrar** (`btn--danger btn--lg btn--block`) e **Voltar ao jogo** (`btn--secondary btn--lg btn--block`).
- **Placar final** (`key = "final"`): header com o título "Fim de jogo" e um subtítulo com o resultado. Os vencedores são os jogadores com a maior pontuação, quando ela é maior que zero, na ordem do placar:
  - um vencedor: "Ana venceu!"
  - empate: "Empate entre Ana e Beto" (com "e" antes do último nome e vírgulas entre os anteriores)
  - ninguém pontuou: "Ninguém pontuou"

  No main, `viewScoreboard model`. No footer, **Nova partida** (`btn--primary btn--lg btn--block`).

A regra do vencedor é a mesma usada para o troféu em `viewScoreboard`. Um helper `leaders : Dict Int Player -> List Player` pode servir aos dois.

### 6. CSS

- `.btn--danger` com `background: var(--danger)` e texto claro, seguindo o padrão de `.btn--success`. `--danger` já existe nos dois temas. Conferir o contraste do texto nos dois.
- `.app__footer` já empilha os filhos em coluna com `gap: var(--space-2)`, então o botão Encerrar partida entra abaixo da ação principal sem CSS novo de layout.
- Confirmação e placar final reaproveitam `.card`, `.section`, `.scoreboard`, `.over__title` e `.over__subtitle`. Só é preciso um estilo pequeno para o texto do cartão de confirmação.

## Risks / Trade-offs

- [Toque em Encerrar partida logo depois de um Continuar duplo] → O botão está no rodapé e já cai na trava de 500 ms da troca de tela. Mesmo assim, abre só a confirmação, nunca encerra direto.
- [Rodapé mais alto em telas de 320 × 568] → O botão ghost tem 48 px e reduz a área do main nas três telas. O main já rola, e o placar e o histórico continuam acessíveis. Conferir no modo dispositivo do DevTools.
- [Mensagens atrasadas depois de voltar à preparação] → Fora de `GamePlaying` não há tick nem sorteio pendente, e `ReleaseTaps`/`ReleaseCorrect` só destravam pelo token. Não há nada em voo que possa trazer estado da partida antiga.
- [A confirmação perde a ação principal da origem] → É intencional: a tela de confirmação só tem Encerrar e Voltar ao jogo. Na pausa, isso também impede de retomar a rodada sem querer.
- [`GameConfirmingEnd GameStatus` permite aninhar confirmações em tese] → `AskEnd` só é aceito nas três telas de origem, então `GameConfirmingEnd (GameConfirmingEnd _)` nunca é construído.

## Open Questions

- Textos finais da confirmação e do subtítulo do placar final. Podem ser ajustados na revisão visual sem mudar os requisitos.
