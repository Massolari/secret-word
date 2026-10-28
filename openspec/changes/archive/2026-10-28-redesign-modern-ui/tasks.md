# Tasks

## 1. Fundação visual

- [x] 1.1 Antes de mexer no código, conferir `git status --short` e confirmar com o usuário o que fazer com qualquer alteração sem commit fora de `openspec/` e `.claude/`. Verificar que a árvore está limpa ou que o usuário decidiu.
- [x] 1.2 Atualizar `src/index.html` conforme a decisão 4 da design: `lang="pt-BR"`, `viewport` com `viewport-fit=cover`, duas `meta theme-color` (claro e escuro, com `media`) e o link do Google Fonts para Plus Jakarta Sans 400/600/800 com `display=swap`, no lugar de Roboto e Material Icons. Verificar com `grep -n "Material+Icons\|Roboto" src/index.html`, que não deve retornar nada.
- [x] 1.3 Reescrever o início de `src/index.css` com os tokens da decisão 2 (cores claro e escuro, espaços, raios, sombras, durações, easing), `color-scheme: light dark`, reset base (box-sizing, margens, `body` com `--bg`/`--text` e a fonte), os ajustes de toque (`touch-action: manipulation`, `-webkit-tap-highlight-color`, inputs com 16px) e os keyframes da decisão 8 (`screen-in`, `word-in`, `pop`, `item-in`, `item-out`, `pulse`), mais o bloco de `prefers-reduced-motion`. Remover as regras `mdc-*` só depois, na tarefa 4.2. Verificar abrindo `yarn start` e alternando o modo escuro do sistema (ou a emulação do DevTools): o fundo da página muda.
- [x] 1.4 Criar `src/Icons.elm` com `play`, `plus`, `trash`, `check`, `skip`, `clock`, `users` e `trophy` (SVG 24×24, `stroke="currentColor"`, `aria-hidden="true"`) e mover `elm/svg` para as dependências diretas do `elm.json`. Verificar com `npx elm make src/Icons.elm --output=/dev/null`.
- [x] 1.5 Adicionar ao `src/index.css` os componentes base `.app` / `.app__header` / `.app__main` / `.app__footer` (decisão 4, com safe areas e rodapé sticky), `.btn` com as variantes `--primary`, `--success`, `--secondary` e `--icon` (altura mínima de 48px e de 56px, `:active`, `:focus-visible`, `:disabled`), `.card`, `.chip`/controle segmentado e `.field`. Verificar com `yarn build` sem erros.

## 2. Tela de preparação (`specs/game-setup`)

- [x] 2.1 Trocar `PreparingModel.time : String` por `roundSeconds : Int` (inicial 60) e `InputTime` por `SelectTime Int`, e fazer `Start` usar `roundSeconds * 1000` (decisão 6). Reescrever `getTimeString` sem `Time.Zone`, removendo `zone`, `AdjustTimeZone` e o `Task.perform Time.here`. Verificar com `npx elm make src/Main.elm --output=/dev/null` depois da tarefa 2.3, e manualmente: com 1min30 escolhido, o cronômetro da rodada começa em 01:30.
- [x] 2.2 Em `AddUser`, aplicar `String.trim`, ignorar nome vazio e devolver `Browser.Dom.focus "player-name"` (com `NoOp`). Adicionar `removing : Set Int`, `DeletePlayer` agendando `RemovePlayer` após 200ms, e a regra de começar contando `players` menos `removing` (decisões 7 e 8). Verificar manualmente: Enter com o campo vazio ou só com espaços não adiciona ninguém; "  Ana " vira "Ana"; o campo continua focado; o item removido some com animação.
- [x] 2.3 Reescrever `viewPreparing` em `Html` puro, conforme a decisão 5: cabeçalho, controle segmentado de tempo (`fieldset` + radios ocultos + labels), campo com botão de adicionar (`disabled` quando vazio; `enterkeyhint`, `autocapitalize`, `autocomplete` e `maxlength`), lista em `Html.Keyed` com avatar de inicial e botão remover com `aria-label "Remover <nome>"`, estado vazio, e rodapé com "Começar" mais a dica de quantos jogadores faltam. Verificar no DevTools em 375×667 e 320×568, nos temas claro e escuro, cada cenário de `specs/game-setup/spec.md` (valor inicial 1min, troca de tempo, adicionar, nome vazio, remover, estado vazio, 1 e 2 jogadores).

## 3. Telas da partida (`specs/game-round`)

- [x] 3.1 Criar a view do placar (ordenado por pontos, com posição e destaque com `trophy` para o líder ou os líderes empatados) e reescrever `viewGame`/`viewGameShowingPlayers` para o anúncio da dupla (cartão "X dá as dicas → Y adivinha", tempo parado, placar e rodapé "Começar rodada"), com `Html.Keyed` por tela para `screen-in`. Verificar manualmente com Ana, Beto e Caio a ordem das duplas do spec e a ordenação do placar.
- [x] 3.2 Reescrever `viewPlaying`: cabeçalho com cronômetro `tabular-nums` e barra via `--progress` (`.timer--alert` com 10s ou menos), palavra keyed por `List.length drawnIndex` com tipografia fluida, contador de acertos keyed pelo valor (`pop`), e rodapé com "Pular" (anel `conic-gradient` com o número durante o bloqueio, ícone `skip` depois) e "Acertou" (`--success`). Sem placar. Verificar em 320px com uma palavra longa (sem rolagem horizontal), o bloqueio de 5 a 1 do Pular, o alerta nos últimos 10s e a animação de nova palavra a cada toque.
- [x] 3.3 Reescrever `viewGameOver`: "Tempo esgotado!", os pontos do adivinhador e de quem deu as dicas (metade arredondada para baixo), o placar e o rodapé "Continuar". Verificar manualmente: com 5 acertos, a tela mostra 5 e 2, e depois de Continuar o placar soma esses valores e mostra a próxima dupla.

## 4. Remoção do Material

- [x] 4.1 Remover de `src/Main.elm` todos os imports `Material.*` e o helper `viewList`, remover de `src/index.js` os dois imports do MDC, tirar `aforemny/material-components-web-elm` e `mdgriffith/elm-ui` do `elm.json` e rodar `yarn remove material-components-web-elm`. Verificar com `grep -rn "Material\|mdc" src/`, que não deve retornar nada, e com `npx elm make src/Main.elm --output=/dev/null` terminando em `Success!`.
- [x] 4.2 Apagar de `src/index.css` as regras antigas (`.center`, `.list-inline-block`, `.action-button*`, `.fab--disabled`, `.mdc-*`, `--mdc-theme-*`). Verificar com `grep -n "mdc\|action-button\|list-inline" src/index.css`, que não deve retornar nada, e com `yarn build` sem erros.

## 5. Vibração e tela acesa

- [x] 5.1 Transformar `Main` em `port module` com `vibrate : List Int -> Cmd msg` e `keepAwake : Bool -> Cmd msg`, e emitir os comandos em `Correct`, `Play` e no `Tick` que leva a `GameOver` (decisão 9). Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [x] 5.2 Em `src/index.js`, guardar a instância do app e inscrever as portas com guarda (`app.ports?.vibrate`): `navigator.vibrate?.()`, Wake Lock com `request`/`release`, novo pedido em `visibilitychange` e todas as falhas silenciadas. Verificar em `yarn start`, no Chrome, que durante a rodada `navigator.wakeLock` mantém um sentinel ativo (log temporário ou inspeção no console) e que, depois do fim, ele foi liberado. No Safari ou Firefox, verificar que não há erro no console ao acertar e ao terminar a rodada.

## 6. Verificação integrada

- [x] 6.1 Rodar `yarn build` com Node 24 e confirmar que termina sem erros, que o `dist/` não contém `mdc-` (`grep -l "mdc-" dist/*` vazio) e que o bundle de JS ficou menor que o atual.
- [x] 6.2 Com `yarn start`, percorrer a partida completa no DevTools em modo dispositivo (iPhone SE e um Android grande), nos temas claro e escuro, sem erros no console: preparar com 3 jogadores e 30s, jogar uma rodada com acertos e pulos, terminar, continuar por pelo menos 2 duplas. Conferir o contraste do texto principal e dos botões com a ferramenta de contraste do DevTools (≥ 4,5:1).
- [x] 6.3 Repetir a rodada com `prefers-reduced-motion: reduce` emulado e confirmar que palavra, telas e alerta de tempo aparecem sem deslocamento, escala ou pulsação (`specs/ui-theme`, "Movimento reduzido").
- [x] 6.4 Testar num celular real pelo IP da máquina na rede local (se preciso, `yarn start --host 0.0.0.0`): duplo toque rápido em Acertou não dá zoom, focar o campo de nome não dá zoom, os botões ficam acima da barra de gestos e, se o aparelho tiver suporte, a vibração funciona. O Wake Lock não funciona por HTTP fora de `localhost`, então ele fica de fora deste teste.

## Workflow follow-up

- Fazer commit da implementação depois de revisar o resultado.
- Arquivar a change com `/opsx:archive` para criar os specs `ui-theme`, `game-setup` e `game-round` em `openspec/specs/`.
