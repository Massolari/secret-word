# Proposal

## Why

O projeto não tem nenhum teste automatizado. Hoje cada change é verificada à mão no navegador, cenário por cenário, e as últimas changes (toques duplos, pulo que custa tempo, sorteio sem repetição, placar com empates, nomes repetidos) mexeram em regras sutis que podem quebrar sem ninguém notar. Uma bateria de testes com `elm-test` e `elm-program-test` deixa essas regras verificáveis em segundos e protege as próximas changes do roadmap (`end-game-to-setup`, `protect-game-from-reload`).

## What Changes

- Adicionar `elm-test` (runner `elm-test@0.19.3-0`, compatível com o Elm 0.19.3 do projeto) e um script `yarn test` no `package.json`.
- Adicionar ao `elm.json` as dependências de teste `elm-explorations/test` 2.2.x e `avh4/elm-program-test` 4.0.1, mais as indiretas que elas pedem.
- Refatorar `src/Main.elm` para o padrão de efeitos do `elm-program-test`: `update` e `init` passam a devolver um tipo `Effect` (dados), e uma função `perform : Effect -> Cmd Msg` o converte em `Cmd` real no `main`. É a única forma de o `elm-program-test` simular portas (`vibrate`, `keepAwake`), `Process.sleep` (bloqueios de 500 ms e remoção animada), `Time.now`, o foco do campo e o sorteio aleatório. A refatoração SHALL NOT mudar o comportamento do app.
- Criar uma bateria de testes em `tests/` que cubra o máximo possível dos cenários dos specs `game-setup`, `game-round` e `word-list`:
  - testes de unidade das funções puras (`getTimeString`, `secondsLeft`, `normalizeName`, `duplicatePlayer`, `wordPool`, `canSkip`, `getCurrentPlayers`, ordem das duplas, pontuação, posições do placar);
  - testes de `update` mensagem a mensagem para o que o `elm-program-test` não simula (ticks do cronômetro, visibilidade da página, respostas de sorteio atrasadas, ciclo da lista esgotada);
  - testes de fluxo com `elm-program-test`, dirigindo a interface como o jogador (preencher nome, tocar em botões, avançar o tempo) e conferindo a tela e as portas;
  - testes de dados sobre `Words.words` (sem duplicatas, bem formadas, cobertura do alfabeto, palavras obscuras ausentes).

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

(nenhuma; os testes verificam requisitos que já existem e a refatoração de efeitos não muda comportamento, por isso a change usa `skip_specs: true`)

## Impact

- `elm.json`: novas `test-dependencies` (`elm-explorations/test`, `avh4/elm-program-test` e indiretas como `elm/bytes`, `elm/http`, `elm/parser`, `hecrj/html-parser`, `elm-community/list-extra`, `avh4/elm-fifo`, `mgold/elm-nonempty-list`). As dependências do app não mudam.
- `package.json` e `yarn.lock`: devDependency `elm-test` e script `test`.
- `src/Main.elm`: `init` e `update` passam a devolver `( Model, Effect )`; novo tipo `Effect` e função `perform`; `lockTaps`, `lockCorrect`, `tickNow` e `newWordIndex` passam a produzir `Effect`. As views, o `Model` e as mensagens não mudam.
- Novos arquivos em `tests/` (um módulo por área, mais um módulo auxiliar com a simulação dos efeitos).
- Fora do escopo: o JS de `src/index.js` (wake lock e vibração no navegador) e o que depende de CSS ou do aparelho (tema, layout em 320px, alvos de 48px, animações, movimento reduzido, zoom). O `elm-test` não roda esse código; esses cenários continuam verificados à mão.
