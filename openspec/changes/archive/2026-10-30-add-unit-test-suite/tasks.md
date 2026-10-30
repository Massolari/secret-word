# Tasks

## 1. Ferramentas de teste

- [x] 1.1 Adicionar `elm-test@0.19.3-0` como devDependency e o script `"test": "elm-test"` no `package.json` (`yarn add -D elm-test@0.19.3-0`). Verificar que `yarn elm-test --version` imprime `0.19.3-0`.
- [x] 1.2 Instalar `elm-explorations/test` (2.2.x) e `avh4/elm-program-test` (4.0.1) nas `test-dependencies` do `elm.json` com `npx elm-test install …`, ou à mão se o `install` falhar com Elm 0.19.3 (Decisão 5). Criar `tests/SmokeTest.elm` com um teste trivial que importa `Main` e `ProgramTest`. Verificar que `yarn test` passa e que `npx elm make src/Main.elm --output=/dev/null` continua compilando. Se o runner não funcionar, trocar por `elm-test-rs` e registrar a troca no design.

## 2. Refatoração para `Effect`

- [x] 2.1 Em `src/Main.elm`, criar `type Effect` e `perform : Effect -> Cmd Msg` (Decisão 1). Mudar `init`, `update`, `updatePreparing`, `updateGame`, `advanceRound`, `pauseRound`, `lockTaps`, `lockCorrect`, `tickNow` (vira `GetTime`) e `newWordIndex` (vira `DrawWord (wordPool …)`) para devolver `Effect`, mantendo a mesma ordem dos `Batch` de hoje. Ajustar `main` para aplicar `perform`. Verificar que `npx elm make src/Main.elm --output=/dev/null` compila e que `git diff` não toca em `view*`, `Model`, `Msg` nem `subscriptions`.
- [x] 2.2 Rodar `yarn start` e verificar no navegador que o comportamento não mudou: adicionar e remover jogadores (remoção animada), começar a partida, toque duplo em Começar rodada ignorado, Acertou vibrando e trocando a palavra, Pular tirando 5 s, aba oculta pausando a rodada, Continuar rodada, fim do tempo e Continuar somando o placar.

## 3. Harness de testes

- [x] 3.1 Criar `tests/Harness.elm` com `simulate : Effect -> SimulatedEffect Msg` (tabela da Decisão 2), `simulateWith : (List Int -> Maybe Int) -> Effect -> SimulatedEffect Msg`, `start : ProgramTest Model Msg Effect` (sobre `ProgramTest.createElement` com `withSimulatedEffects`) e os helpers `addPlayer name`, `addPlayers names`, `startGame` (Começar + `waitGuard`), `startRound` (Começar rodada + `waitGuard`), `waitGuard` (`advanceTime 500`), `tick t` e `ticks from to` (um tick por segundo). Criar também os construtores de `Model` para a camada 2: `withPlayers`, `playingModel`, `withRemaining`. Verificar com um teste em `tests/HarnessTest.elm` que `start |> addPlayers [ "Ana", "Beto" ] |> startGame |> startRound` mostra "Abacate" e "Ana dá dicas para Beto", e que `ticks 1000 60000` numa rodada de 1min leva a "Tempo esgotado!".

## 4. Funções puras

- [x] 4.1 Criar `tests/HelpersTest.elm` com testes de `getTimeString` (00:00, 00:01 para 1 ms, 01:00, 03:00, arredondamento para cima) e `secondsLeft`, com fuzz para "sempre `MM:SS`" e "nunca mostra 00:00 com tempo restante > 0". Verificar com `yarn test`.
- [x] 4.2 Testar `normalizeName` (pontas, espaços internos, caixa, acento preservado, idempotência por fuzz) e `duplicatePlayer` (encontra "Ana" para "  ANA ", "Ana Maria" para "ana   maria", ignora jogadores em `removing`, `Nothing` para "Aná" e para texto vazio). Verificar com `yarn test`.
- [x] 4.3 Testar `wordPool` (restantes enquanto houver, novo ciclo sem a palavra atual, lista de uma palavra só, fuzz: nunca devolve índice já sorteado com restantes), `canSkip` (5001 ms sim, 5000 ms não) e `getCurrentPlayers` (índice fora do array). Verificar com `yarn test`.

## 5. Preparação (`game-setup`)

- [x] 5.1 Criar `tests/SetupTest.elm` com `describe` por requisito, cobrindo os cenários "Valor inicial", "Trocar o tempo" (2min selecionado e rodada começando em 02:00), "Adicionar pelo teclado" (`submit` com "  Ana " → "Ana" no fim da lista e campo vazio; conferir `FocusPlayerName` no efeito via teste de `update`) e "Nome vazio" (botão desabilitado e `submit` sem efeito). Verificar com `yarn test`.
- [x] 5.2 Cobrir "Nome de jogador repetido": "Mesmo nome com outra caixa e espaços", "Espaços repetidos no meio", "Confirmar pelo teclado com nome repetido" (texto mantido), "Acento diferencia nomes", "Aviso some ao corrigir o nome", "Remover o jogador de mesmo nome" (aviso some antes do `advanceTime 200`) e "Leitor de tela" (`aria-invalid "true"` e `aria-describedby "player-name-error"`). Verificar com `yarn test`.
- [x] 5.3 Cobrir "Remover jogador" ("Remover um jogador" só depois de `advanceTime 200`, e "Leitor de tela" pelo `aria-label "Remover Ana"`), "Estado vazio da lista" e "Regra para começar" ("Um jogador" com "Falta 1 jogador" e botão desabilitado; "Dois jogadores" levando ao anúncio). Listar no comentário do módulo os cenários de `game-setup` que ficam manuais (foco real do campo no navegador). Verificar com `yarn test`.

## 6. Rodada (`game-round`) com `elm-program-test`

- [x] 6.1 Criar `tests/RoundTest.elm` cobrindo "Ordem das duplas" (três jogadores: as seis duplas e a volta para Ana→Beto, passando por Continuar seis vezes), "Anúncio da dupla" (nomes e cronômetro parado em 01:00 depois de `advanceTime`) e "Placar oculto" durante a rodada. Verificar com `yarn test`.
- [x] 6.2 Cobrir "Acertar": "Acerto" (contador 2→3 e nova palavra), "Toque duplo em Acertou" (segundo toque sem `advanceTime` ignorado e fora do histórico), "Acertou logo depois de Pular", "Acertos em sequência" e "Aparência durante o bloqueio" (botão sem `disabled`). Conferir `vibrate [30]` com `expectOutgoingPortValues`. Verificar com `yarn test`.
- [x] 6.3 Cobrir "Pular custa tempo": "Pular logo que a palavra aparece" (00:40 → 00:35), "Pulos seguidos" (00:30 → 00:20), "Custo visível" (texto "−5s" e `aria-label`), "Tempo insuficiente" (botão `disabled`), "Limite exato" (00:06 → 00:01 e desabilitado) e "Pulo leva aos últimos segundos" (`timer--alert`). Verificar com `yarn test`.
- [x] 6.4 Cobrir "Cronômetro com progresso" ("Contagem normal" e "Últimos segundos" via `timer--alert`), "Fim da rodada e pontuação" ("Cinco acertos" com +5 e +2 na tela e no placar depois de Continuar; "Sem ações depois do fim") e "Histórico de palavras da rodada" ("Acertos e pulos" com a ordem e as classes `history__row--guessed`/`--missed`, "Contagem coerente com os pontos", "Rodada seguinte começa limpa" e "Leitor de tela" com ", acertou"/", errou"). Conferir `vibrate [120,80,120,80,240]` e `keepAwake` `[True, False]`. Verificar com `yarn test`.
- [x] 6.5 Cobrir "Placar": "Ordenação", "Empate fora da liderança", "Posição depois de um empate", "Empate na liderança" e "Ninguém pontuou", montando os pontos com rodadas reais ou com `ProgramTest.createElement` a partir de um `Model` pronto. Conferir a ordem das linhas, `scoreboard__row--leader` e o texto da posição. Verificar com `yarn test`.
- [x] 6.6 Cobrir "Retomar rodada pausada" ("Volta para o jogo": "Rodada pausada", sem palavra, sem Acertou e Pular; "Retomar": mesma palavra, contador e tempo, entrando na pausa com `ProgramTest.update (GameMsg (VisibilityChanged Hidden))`) e "Proteção contra toques na troca de tela" (os seis cenários de toque duplo e o "Botão liberado depois do bloqueio"). Listar no comentário do módulo os cenários de `game-round` que ficam manuais (palavra longa em 320px, vibração real, wake lock real, animações) com o motivo. Verificar com `yarn test`.

## 7. Rodada (`game-round`) em `update`

- [x] 7.1 Criar `tests/UpdateTest.elm` com o efeito exato de cada mensagem: `Start`, `Play`, `Correct`, `Jump`, `Restart`, `Resume`, `AddUser` e `DeletePlayer`, incluindo os tokens de `After 500 (ReleaseTaps n)` e `After 500 (ReleaseCorrect n)`, e `ReleaseTaps`/`ReleaseCorrect` com token velho sem efeito. Verificar com `yarn test`.
- [x] 7.2 Cobrir os cenários de relógio e visibilidade: "Navegador lento" (ticks atrasados de até 2000 ms somando 30 s terminam a rodada de 30 s), "Tela bloqueada" e "Troca de aba" (`VisibilityChanged Hidden` pausa com o tempo intacto e `KeepAwake False`), "Página congelada sem aviso" (tick com intervalo > 2000 ms pausa sem descontar), "Fora da rodada" (`Hidden` no anúncio e no fim não muda o modelo) e o primeiro tick depois de `Resume` não descontando tempo. Verificar com `yarn test`.
- [x] 7.3 Cobrir "Sorteio de palavras" e "Lista esgotada": "Sem repetição na partida" (`DrawWord` sem índices sorteados), "Todas as palavras podem sair" (índice 0 e o último aceitos), "Poucas palavras restantes" (pool com um único índice), "Última palavra acertada" (com `drawnIndex` cheio menos um, `Correct` pede um pool sem a palavra atual e o `NewWordIndex` reinicia o ciclo), "Novo ciclo sem repetição" e o `NewWordIndex` atrasado (stale) pedindo novo sorteio. Verificar com `yarn test`.

## 8. Lista de palavras (`word-list`)

- [x] 8.1 Criar `tests/WordsTest.elm` cobrindo "Sem duplicatas", "Palavras bem formadas" (maiúscula inicial, só letras e hífen interno, sem espaços nem pontuação; "Phil ips", "Charle sto.", "Araini ng" e "Houveu" ausentes), "Palavras conhecidas pelo público geral" (exemplos presentes e ausentes do spec), "Estrangeirismo corrente" ("Hacker", "Handebol") e "Cobertura de todo o alfabeto" (iniciais I, M, P, S e Z presentes). Verificar com `yarn test`.

## 9. Verificação integrada

- [x] 9.1 Remover `tests/SmokeTest.elm` se já estiver coberto, rodar `yarn test` do zero (`rm -rf elm-stuff` antes) e conferir que todos os testes passam. Comparar a saída com os specs: cada cenário de `game-setup`, `game-round` e `word-list` tem um teste com o mesmo nome ou aparece na lista de manuais de um comentário de módulo.
- [x] 9.2 Confirmar que `yarn build` continua gerando o app e que, no navegador, uma partida completa com 3 jogadores funciona como antes da refatoração.

## Workflow follow-up

- Arquivar a change depois de revisada e com `yarn test` passando.
