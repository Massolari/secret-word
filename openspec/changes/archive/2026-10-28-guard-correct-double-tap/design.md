# Design

## Context

Hoje, em `src/Main.elm`:

- `Correct` soma 1 em `score`, registra `( word, Guessed )` em `history`, vibra e pede um sorteio com `newWordIndex`. A palavra só troca quando chega `NewWordIndex`, que é assíncrono e pode se repetir até achar um índice ainda não sorteado.
- `Jump` também pede um sorteio e tira 5 s do tempo.
- O `tapGuard` (`TapGuard { locked, token }`, `tapGuardMillis = 500`, `lockTaps`, `ReleaseTaps`) bloqueia todos os botões do rodapé depois das trocas de tela. `acceptsGameMsg` checa esse bloqueio e o status da tela antes de deixar `Correct`, `Jump`, `Play` e `Restart` agirem.

O requisito "Pular custa tempo" exige que pulos seguidos valham ("Pulos seguidos"). Por isso o bloqueio novo não pode travar o Pular.

## Goals / Non-Goals

**Goals:**

- Um toque em Acertou só conta depois que a palavra atual está na tela há pelo menos 500 ms.
- Reaproveitar o padrão de bloqueio com token que já existe, sem um mecanismo novo.

**Non-Goals:**

- Medir o tempo de reação do jogador ou adaptar a duração do bloqueio.
- Mudar o bloqueio da troca de tela ou o comportamento do Pular.

## Decisions

### 1. Um bloqueio separado só para o Acertou

Adicionar `correctGuard : TapGuard` ao `Model`, com uma mensagem de topo `ReleaseCorrect Int` que só desbloqueia quando o token é o atual, igual a `ReleaseTaps`. A duração é a mesma constante `tapGuardMillis` (500 ms).

- Alternativa: reaproveitar o `tapGuard` a cada nova palavra. Foi descartada porque travaria o Pular junto e quebraria o cenário "Pulos seguidos".
- Alternativa: guardar a hora em que a palavra apareceu e comparar com a hora do toque. Foi descartada porque em Elm a hora vem por `Task`, e cada toque viraria duas mensagens. O token com `Process.sleep` já está no código e resolve.

### 2. Travar no toque, liberar 500 ms depois de a palavra aparecer

- Em `Correct` e `Jump`, marcar `correctGuard.locked = True` e incrementar o token, sem agendar liberação. Isso cobre o intervalo entre o toque e a chegada de `NewWordIndex`, em que a palavra antiga ainda está na tela. O token novo também invalida uma liberação pendente da palavra anterior.
- Em `NewWordIndex`, no ramo em que a palavra é definida, travar com um token novo e agendar `ReleaseCorrect token` em `tapGuardMillis`. Os ramos que sorteiam de novo não mexem no bloqueio.
- Concentrar isso numa função `lockCorrect`, no formato de `lockTaps`, que recebe e devolve `( Model, Cmd Msg )`. A trava sem liberação de `Correct`/`Jump` pode ser feita direto no registro ou com um parâmetro. Quem implementar escolhe, desde que o token seja sempre incrementado.

A primeira palavra da rodada (depois de `Play`) também passa por `NewWordIndex` e ganha o bloqueio. Isso combina com a regra "logo depois que uma nova palavra aparece" e fica sobreposto ao bloqueio da troca de tela, que tem a mesma duração.

- Alternativa: começar a contar os 500 ms no toque. Foi descartada porque o sorteio é assíncrono, e a janela útil depois de a palavra aparecer ficaria menor e variável.

### 3. Checar em `acceptsGameMsg`

No caso `Correct`, `acceptsGameMsg` também exige `not model.correctGuard.locked`. Toques bloqueados caem no `( model, Cmd.none )` que já existe, então não somam, não registram, não sorteiam e não vibram. `Jump` não muda.

### 4. Sem mudança visual

O botão não recebe `disabled` nem classe nova, igual à Decisão 4 da proteção na troca de tela: um estilo de desabilitado piscando a cada acerto atrapalharia mais do que ajudaria.

## Risks / Trade-offs

- [Um jogador muito rápido tem um acerto legítimo ignorado em menos de 500 ms] → Ler a palavra, dar a dica e ouvir o palpite leva bem mais que meio segundo. Se for preciso, dá para ajustar só a constante.
- [O toque ignorado não dá retorno, e o jogador pode achar que o botão falhou] → A palavra não troca, o que já sinaliza que o toque não contou. A janela é curta, e um novo toque depois dela funciona.
- [Se `NewWordIndex` nunca definir a palavra, o Acertou fica travado] → Isso só acontece no loop infinito do sorteio quando as palavras acabam (change `word-draw-exhaustion`), em que a aba já congela de qualquer jeito.
- [Uma liberação de uma rodada anterior chega na rodada seguinte] → O token incrementado em cada trava invalida essa liberação.
