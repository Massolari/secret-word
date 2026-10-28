# Tasks

## 1. Modelo do sorteio

- [x] 1.1 Em `GameModel`, trocar `drawnIndex : List Int` por `drawnIndex : Set Int`, remover o campo `generator` e adicionar `currentIndex : Maybe Int` e `drawCount : Int`, ajustando `initGame` (`Set.empty`, `Nothing`, `0`) e o `Play` (que hoje redefine `generator`). Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [x] 1.2 Criar a função pura `wordPool : Int -> Set Int -> Maybe Int -> List Int` (tamanho da lista, sorteados, índice atual). Ela devolve os índices de `0` a `tamanho - 1` que não foram sorteados, ou, se não restar nenhum, todos menos o atual (ou todos, se isso esvaziar o pool), conforme as Decisões 1 e 2 do design. Verificar que compila. Os casos `wordPool 3 (Set.fromList [0,2]) (Just 2)` == `[1]`, `wordPool 3 (Set.fromList [0,1,2]) (Just 1)` == `[0,2]` e `wordPool 1 (Set.fromList [0]) (Just 0)` == `[0]` são exercitados na tarefa 3.2 (o `elm repl` não carrega `Main`, que é um `port module`).

## 2. Sorteio e handler

- [x] 2.1 Substituir `randomWord`/`newWordIndex` por um `newWordIndex : GameModel -> Cmd Msg` que monta o pool com `wordPool (Array.length Words.words) …` e sorteia uma posição com `Random.int 0 (List.length pool - 1)`, mapeando para o índice da palavra. Atualizar as chamadas em `StartRound`, `Correct` e `Jump`. Verificar que compila e que `Random.int 0 (Array.length …)` não aparece mais em `src/Main.elm`.
- [x] 2.2 Reescrever `NewWordIndex index` seguindo as Decisões 2 e 3: se `drawnIndex` já tem todas as palavras, começar o ciclo com `Set.singleton index`. Senão, se o índice já saiu, emitir `newWordIndex` a partir do estado atual. Senão, inserir o índice. Ao aceitar, atualizar `word`, `currentIndex`, `toShowJump = 5` e incrementar `drawCount`. Se `Array.get` der `Nothing`, manter o estado sem re-sortear. Verificar que compila e que nenhum caminho re-sorteia sem que o pool tenha mudado.
- [x] 2.3 Em `viewPlaying`, trocar a chave do `Keyed.node` da palavra de `List.length gameModel.drawnIndex` para `String.fromInt gameModel.drawCount`. Verificar no navegador que a animação de entrada da palavra continua acontecendo a cada Acertou e Pular.

## 3. Verificação integrada

- [x] 3.1 Rodar `npm start`, jogar duas rodadas com acertos e pulos e verificar o cenário "Sem repetição na partida": nenhuma palavra se repete entre as rodadas (conferir pelo histórico do fim da rodada).
- [x] 3.2 Trocar temporariamente `Words.words` por uma lista de 3 palavras (sem commitar), jogar até passar da terceira palavra e verificar os cenários "Última palavra acertada" e "Novo ciclo sem repetição": o app não congela, a palavra seguinte é diferente da que estava na tela, as 3 voltam a sair sem repetição dentro do ciclo, e o contador e o cronômetro seguem normais. Repetir com uma lista de 1 palavra e verificar que a mesma palavra reaparece sem travar. Restaurar `src/Words.elm` e confirmar com `git diff --stat` que ele não mudou.
- [x] 3.3 Rodar `npm run build` e verificar que o build termina sem erros.
