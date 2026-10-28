# Tasks

## 1. Contagem pelo tempo real

- [x] 1.1 Adicionar `lastTick : Maybe Time.Posix` ao `GameModel`, com `Nothing` em `initGame`. Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [x] 1.2 Reescrever `Tick now` conforme a Decisão 1 do design: sem `lastTick`, só grava o instante; com `lastTick`, desconta `now - prev` de `currentTime`, mantém a transição para `GameOver` (histórico, vibração, `keepAwake False`) quando chega a ≤ 0, e ignora o `Tick` fora de `GamePlaying` (Decisão 6). Verificar que compila.
- [x] 1.3 Fazer o bloqueio do Pular diminuir pelos segundos inteiros cruzados no tick (Decisão 4), com mínimo 0. Verificar no navegador que o Pular continua liberando cerca de 5 segundos depois da palavra aparecer.
- [x] 1.4 Em `Play`, gravar `lastTick = Nothing` e incluir `Task.perform (GameMsg << Tick) Time.now` no `Cmd.batch`. Trocar `Time.every 1000` por `Time.every 200` em `subscriptions`. Verificar que compila.
- [x] 1.5 Fazer `getTimeString` arredondar os segundos para cima. Verificar no navegador que uma rodada de 1min começa em 01:00, mostra 00:01 no último segundo e que o anúncio da dupla continua mostrando o tempo cheio.

## 2. Pausa e retomada

- [x] 2.1 Adicionar `GamePaused` a `GameStatus` e as mensagens `Resume` e `VisibilityChanged Browser.Events.Visibility` a `GameMsg`, com um helper `pause` (`status = GamePaused`, `lastTick = Nothing`, `keepAwake False`). Verificar que compila, ajustando os `case` que passarem a não ser exaustivos.
- [x] 2.2 Assinar `Browser.Events.onVisibilityChange` em `GamePlaying`, junto com o tick, e pausar em `Hidden` (ignorando `Visible` e qualquer estado que não seja `GamePlaying`). No `Tick`, pausar sem descontar quando o intervalo passar de 2000 ms (Decisão 3). Verificar que compila.
- [x] 2.3 Implementar `Resume` (só em `GamePaused`): voltar para `GamePlaying` com `lastTick = Nothing`, `Task.perform (GameMsg << Tick) Time.now` e `keepAwake True`, sem sortear palavra. Verificar que compila.
- [x] 2.4 Criar `viewPaused` conforme a Decisão 5 (timer parado no header, dupla, cartão "Rodada pausada" com o ícone de relógio e texto de apoio, botão "Continuar rodada" no rodapé, sem palavra, contador, Acertou ou Pular) e ligá-la na view para `GamePaused` com `key = "paused"`. Adicionar em `src/index.css` o estilo `.paused` a partir de `.card`, conferindo nos temas claro e escuro. Verificar no navegador que a tela aparece ao trocar de aba durante a rodada.

## 3. Verificação integrada

- [x] 3.1 Rodar `yarn start`, começar uma rodada de 30s, trocar de aba com cerca de 15 s restantes, esperar 30 s e voltar. Verificar os cenários "Troca de aba" e "Volta para o jogo": a rodada está pausada, mostra o mesmo tempo e não terminou.
- [x] 3.2 Tocar em Continuar rodada e verificar o cenário "Retomar": mesma palavra, mesmo contador, a contagem segue do tempo pausado, e um bloqueio do Pular que estava pela metade continua de onde parou.
- [x] 3.3 Numa rodada de 30s sem sair da tela, cronometrar do toque em Começar rodada até a tela de fim e verificar que dá 30 s com margem de até 1 s. Repetir com a CPU limitada a 6x no DevTools (Performance) para cobrir o cenário "Navegador lento".
- [x] 3.4 Num celular (ou no modo dispositivo do DevTools), bloquear a tela durante a rodada e desbloquear depois de 1 min. Verificar o cenário "Tela bloqueada": a rodada está pausada com o tempo de antes, e a tela segue o bloqueio do aparelho enquanto está pausada.
- [x] 3.5 Trocar de aba no anúncio da dupla e no fim da rodada e verificar o cenário "Fora da rodada": nada muda ao voltar.
- [x] 3.6 Rodar `yarn build` e verificar que o build termina sem erros.
