# Design

## Context

Em `src/Main.elm`, o `GameModel` guarda a duração da rodada em `time` e o tempo restante em `currentTime`, ambos como `Time.Posix` usado como duração em ms. `subscriptions` assina `Time.every 1000 (GameMsg << Tick)` só em `GamePlaying`. Hoje `Tick _` faz:

- `currentTime - 1000`, e `GameOver` quando chega a zero (registrando a última palavra no histórico, vibrando e chamando `keepAwake False`);
- `toShowJump - 1` enquanto for maior que zero.

`Play` vai para `GamePlaying`, sorteia a palavra e chama `keepAwake True`. Em `src/index.js`, `keepAwake` liga ou desliga `wantsWakeLock`, e o listener de `visibilitychange` só pede o lock de novo se `wantsWakeLock` for verdadeiro.

`getTimeString` arredonda os segundos para baixo; com a contagem por ticks, `currentTime` sempre caía em múltiplos de 1000, então isso nunca apareceu.

## Goals / Non-Goals

**Goals:**
- Descontar do tempo restante só o tempo real decorrido com a rodada em andamento e visível.
- Pausar ao sair da tela e retomar só com um toque.
- Manter o contrato atual do bloqueio do Pular (5 segundos do cronômetro), agora parado durante a pausa.

**Non-Goals:**
- Botão de pausa manual durante a rodada.
- Fazer o bloqueio do Pular contar a partir do momento em que a palavra apareceu (change `skip-lock-per-word`).
- Corrigir as condições de corrida do wake lock em `index.js` (change `fix-wake-lock-races`).
- Bloquear toques que vazam entre telas (change `guard-screen-transition-taps`). O botão de retomar fica no rodapé, mas só aparece depois de uma saída da tela, então não recebe toques destinados a outra tela.
- Salvar a partida entre recarregamentos.

## Decisions

### 1. Tempo decorrido entre ticks, não um prazo final

Adicionar ao `GameModel`:

```elm
, lastTick : Maybe Time.Posix
```

`Tick now`:
- `lastTick = Nothing` → só grava `lastTick = Just now`, sem descontar nada. Esse é o tick de partida.
- `lastTick = Just prev` → `elapsed = now - prev`. Se `elapsed` passar do limite de interrupção (Decisão 3), pausa. Senão, `currentTime - elapsed`, com `GameOver` quando chega a ≤ 0, e grava `lastTick = Just now`.

`Play` e `Resume` gravam `lastTick = Nothing` e disparam `Task.perform (GameMsg << Tick) Time.now`, para que a contagem comece no instante do toque, sem esperar o primeiro intervalo.

Alternativa considerada: guardar um prazo final (`deadline = início + duração`) e calcular `deadline - now`. É a forma mais simples de seguir o relógio, mas com pausa exige recalcular o prazo ao retomar e não ajuda a detectar congelamentos. Descontar deltas resolve os dois com o mesmo código, e a pausa é só zerar `lastTick`.

### 2. Ticks a cada 200 ms e exibição arredondada para cima

Com o tempo real, o tick deixa de definir a granularidade. `Time.every 200` faz o MM:SS virar perto da hora certa e a rodada terminar no máximo 200 ms depois do prazo. `getTimeString` passa a arredondar para cima (`(ms + 999) // 1000`), para que a rodada de 1min comece mostrando 01:00 e mostre 00:01 até o fim, como hoje. A barra usa os ms e anda de forma contínua, então o "a cada segundo" do spec continua valendo para o MM:SS.

`getTimeString` também é usado no chip do anúncio da dupla com `currentTime` cheio (múltiplo de 1000), em que arredondar para cima não muda nada.

### 3. Pausa por visibilidade e por intervalo longo

- `subscriptions` em `GamePlaying`: `Sub.batch [ Time.every 200 (GameMsg << Tick), Browser.Events.onVisibilityChange (GameMsg << VisibilityChanged) ]`. Fora de `GamePlaying`, nada muda; em `GamePaused` não há assinatura.
- `VisibilityChanged Hidden` em `GamePlaying` → `pause`. `Visible` não faz nada: retomar exige o toque.
- Limite de interrupção: um `elapsed` acima de 2000 ms (10 vezes o intervalo) só acontece se a página foi congelada ou suspensa. Isso cobre navegadores que suspendem a página sem disparar `visibilitychange`. Nesse caso, `pause` também, sem descontar o intervalo.
- `pause`: `status = GamePaused`, `lastTick = Nothing`, `keepAwake False`.

Os até 200 ms entre o último tick e a pausa não são descontados. Fica dentro da margem do spec e evita uma ida extra a `Time.now`.

Alternativa considerada: tratar a pausa em `index.js` e mandá-la por um port. Foi descartada porque `Browser.Events.onVisibilityChange` já existe em `elm/browser` e mantém a lógica da rodada toda no Elm.

### 4. Bloqueio do Pular por segundos cruzados

Com ticks a cada 200 ms, `toShowJump - 1` por tick liberaria o Pular em 1 s. Para manter o comportamento atual, o bloqueio passa a diminuir pelo número de segundos inteiros que o MM:SS cruzou no tick: `secs antes - secs depois`, com `secs = ceil(currentTime / 1000)` e mínimo 0. É exatamente o que acontecia com um tick por segundo, e para durante a pausa. A change `skip-lock-per-word` substitui isso depois.

### 5. Tela de pausa

Novo `GameStatus`: `GamePaused`, com `viewPaused` no `case` da view, `key = "paused"` para a transição entre telas:

```
header: viewTimer gameModel        (parado, com a cor de alerta se for o caso)
main:   div.round
          p.round__pair   "Ana dá dicas para Beto"
          div.card.paused
            Icons.clock
            h2  "Rodada pausada"
            p   "O tempo para quando o jogo sai da tela."
footer: button.btn.btn--primary.btn--lg.btn--block  [Icons.play] "Continuar rodada"  → GameMsg Resume
```

A palavra fica escondida para ninguém vê-la durante a interrupção. O contador de acertos também não aparece; ele volta igual ao retomar.

`Resume` (só em `GamePaused`): `status = GamePlaying`, `lastTick = Nothing`, `Cmd.batch [ Task.perform (GameMsg << Tick) Time.now, keepAwake True ]`. Não sorteia palavra nova.

### 6. Mensagens fora de estado

`Tick` e `VisibilityChanged` só agem em `GamePlaying`; em outros estados devolvem o modelo sem mudança. Isso cobre o `Tick` de `Time.now` que chega depois de uma pausa ou um `Tick` atrasado depois do `GameOver`. Hoje um `Tick` em `GameOver` reprocessa a transição sem efeito; com o guarda, isso deixa de acontecer.

## Risks / Trade-offs

- [O limite de 2 s pausa a rodada num travamento longo em primeiro plano, num aparelho muito lento] → É raro, a pausa é visível e retomar é um toque. É melhor que descontar segundos que ninguém viu passar.
- [iOS pode não disparar `visibilitychange` ao bloquear a tela em algumas versões] → O limite de intervalo cobre: no desbloqueio, o primeiro tick vê o intervalo longo e pausa.
- [Ticks a cada 200 ms renderizam a view 5 vezes por segundo] → A view da rodada é pequena e as listas são `Keyed`; o custo é desprezível. A animação da barra já usa CSS.
- [`keepAwake False` na pausa e o listener de `visibilitychange` em `index.js`] → Ao voltar para a aba, o listener vê `wantsWakeLock = false` e não pede o lock. Ele só é pedido de novo em `Resume`, que é o comportamento do spec.
