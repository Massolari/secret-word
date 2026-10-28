# Tasks

## 1. Registro do histórico

- [x] 1.1 Adicionar o tipo `WordResult` (`Guessed | Missed`) e o campo `history : List ( String, WordResult )` no `GameModel`, com `history = []` em `initGame`. Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [x] 1.2 Registrar a palavra atual em `Correct` (como `Guessed`) e em `Jump` (como `Missed`), prependendo com `::`. Verificar que compila e que o contador de acertos continua igual.
- [x] 1.3 No `Tick` que leva a `GameOver`, registrar a palavra na tela como `Missed`, a não ser que ela já seja o primeiro item do histórico (Decisão 2 do design). Verificar que compila e que o registro só acontece na transição para `GameOver`, não a cada tick depois dela.
- [x] 1.4 Zerar `history` em `Restart`. Verificar que compila.

## 2. Tela de fim da rodada

- [x] 2.1 Adicionar `Icons.close` (um "x" no mesmo estilo de `Icons.check`) e exportá-lo em `src/Icons.elm`. Verificar que compila.
- [x] 2.2 Criar `viewHistory` em `src/Main.elm`: `section.section` com título "Palavras" e `section__count` com acertos/total, e `ol.card.history` com uma linha por palavra na ordem da rodada (`List.reverse`), contendo ícone, palavra e um `span.visually-hidden` com "acertou" ou "errou". Incluir a seção em `viewGameOver` entre `.card.result` e o placar. Verificar que compila.
- [x] 2.3 Adicionar em `src/index.css` as regras `.history`, `.history__row`, `.history__row + .history__row`, `.history__icon`, `.history__word` (com `overflow-wrap: anywhere`) e os modificadores `--guessed` (ícone em `--success`) e `--missed` (ícone e palavra em `--text-muted`), seguindo as medidas de `.scoreboard`. Verificar no navegador, nos temas claro e escuro, que acerto e erro se distinguem pelo ícone.

## 3. Verificação integrada

- [x] 3.1 Rodar `yarn start`, jogar uma rodada de 30s com acertos, pulos e o tempo acabando com uma palavra na tela, e verificar o cenário "Acertos e pulos" do spec: ordem correta, a última palavra como erro e o número de acertos no histórico igual aos pontos de quem adivinhou.
- [x] 3.2 Tocar em Continuar, jogar outra rodada e verificar que o histórico mostra só as palavras novas.
- [x] 3.3 Com a largura em 320px, verificar que uma palavra longa no histórico quebra linha sem rolagem horizontal e que o botão Continuar continua acessível.
