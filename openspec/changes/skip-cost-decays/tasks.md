# Tasks

## 1. Regra do pulo

- [x] 1.1 Em `src/Main.elm`, adicionar `wordStartTime : Time.Posix` ao `GameModel` e a `initGame`, e defini-lo como `gameModel.currentTime` em `NewWordIndex` ao aceitar a palavra (Decisão 1). Em `tests/Harness.elm`, iniciar o campo em 60000 no `playingModel`. Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [x] 1.2 Renomear a constante `skipCost` para `maxSkipCost`, criar `skipCost : GameModel -> Int` (Decisão 1), mudar `canSkip` para `wordStartTime > maxSkipCost` (Decisão 3) e fazer `Jump` descontar `skipCost gameModel`. Em `tests/HelpersTest.elm`, trocar os testes de `canSkip` por: `wordStartTime` em 5001 pode pular, em 5000 não pode, e o resultado não depende de `currentTime`. Acrescentar testes de `skipCost` com a palavra recém-aparecida (5000), depois de 2400 ms (2600), depois de 5000 ms (0) e depois de 7000 ms (0, sem ficar negativo). Verificar com `yarn test tests/HelpersTest.elm`.

## 2. Botão Pular

- [x] 2.1 Fazer `viewSkipButton` receber o custo e mostrar texto e `aria-label` conforme a tabela da Decisão 4, com `secondsLeft` para arredondar para cima e com `span.skip-cost` invisível (`.skip-cost--hidden`, `visibility: hidden` em `src/index.css`), mas ocupando espaço, quando o custo é zero. Verificar que `npx elm make src/Main.elm --output=/dev/null` compila.
- [x] 2.2 Em `tests/Harness.elm`, fazer `skip` usar `ProgramTest.clickButton "Pular"`. Em `tests/RoundTest.elm`, fazer `skipButton` localizar o botão pelo texto "Pular" (Decisão 5). Criar o helper que leva a rodada a `N` segundos e toca em Acertou, respeitando o bloqueio de 500 ms, para ter uma palavra recém-sorteada. Verificar com `grep -n "perde 5 segundos" tests/` que o texto só aparece nas checagens do `aria-label`.
- [x] 2.3 Reescrever `skipTests` em `tests/RoundTest.elm` com os cenários do delta spec: "Pular logo que a palavra aparece", "Custo diminui com o tempo", "Pular de graça", "Custo recomeça a cada palavra", "Pulos seguidos", "Custo visível", "Sem custo para o leitor de tela", "Tempo insuficiente", "Limite exato", "Palavra antiga nos últimos segundos" e "Pulo leva aos últimos segundos". Verificar com `yarn test tests/RoundTest.elm`.
- [x] 2.4 Em `tests/UpdateTest.elm`, cobrir o cenário "Pausa não reduz o custo": palavra aparece, 2 s de ticks, pausa, retomada, tick com salto grande, e custo e pulo iguais aos de antes da pausa. Verificar com `yarn test tests/UpdateTest.elm`.

## 3. Documentação

- [x] 3.1 Atualizar a regra do Pular em `README.md` (linha 13): o custo começa em 5 segundos e cai 1 por segundo enquanto a palavra está na tela. Verificar relendo o trecho.

## 4. Verificação integrada

- [x] 4.1 Rodar `yarn test` e verificar que toda a suíte passa, incluindo os testes fora de `skipTests` que usam `skip` e `skipButton`.
- [x] 4.2 Rodar `yarn start` e jogar uma rodada de 1min, numa largura de 320px e numa maior. Verificar que o rótulo cai de "−5s" até sumir, que o botão não muda de largura, que o cronômetro cai conforme o custo e que uma palavra que aparece com 5 s ou menos deixa o botão desabilitado até o fim.
- [ ] 4.3 Com um leitor de tela (VoiceOver), verificar os anúncios "Pular (perde 5 segundos)", "Pular (perde 1 segundo)" e "Pular (sem custo)", e o estado indisponível quando o botão está desabilitado.

## Workflow follow-up

- Arquivar a change depois de revisada e testada.
