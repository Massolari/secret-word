# Design

## Context

Todo o app está em `src/Main.elm` (`port module Main exposing (..)`), com a lista em `src/Words.elm` (5456 palavras, de "Abacate" a "Zumbir") e ícones SVG em `src/Icons.elm`. O projeto usa Elm 0.19.3 e Parcel 2, e ainda não tem `tests/` nem `test-dependencies`.

Hoje `init` e `update` devolvem `Cmd Msg` direto, e esses `Cmd` são opacos para testes:

- portas `vibrate : List Int` e `keepAwake : Bool`;
- `Process.sleep` em `lockTaps`/`lockCorrect` (500 ms, `ReleaseTaps`/`ReleaseCorrect`) e em `DeletePlayer` (200 ms, `RemovePlayer`);
- `tickNow` (`Time.now` → `GameMsg (Tick _)`);
- `newWordIndex` (`Random.generate` sobre `wordPool` → `GameMsg (NewWordIndex _)`);
- `Browser.Dom.focus playerNameInputId` em `AddUser`.

As assinaturas `Time.every 200` e `Browser.Events.onVisibilityChange` só existem com `GamePlaying`.

Versões verificadas no registro: `avh4/elm-program-test` 4.0.1 exige `elm-explorations/test` `2.2.0 <= v < 3.0.0` (a 3.x exigia test 1.x). O runner `elm-test` no npm tem `latest` = `0.19.3-0`, para Elm 0.19.3. Em `SimulatedEffect.Time` só existe `now`, e `SimulatedEffect.Sub` só simula portas. Por isso nem `Time.every` nem `onVisibilityChange` podem ser simulados.

Ver proposal.md para a motivação. Os requisitos testados são os de `openspec/specs/game-setup`, `game-round` e `word-list`.

## Goals / Non-Goals

**Goals:**
- Cada cenário dos specs `game-setup`, `game-round` e `word-list` que dependa só de código Elm tem pelo menos um teste com o nome do cenário.
- Os efeitos (portas, sleeps, foco, sorteio, relógio) podem ser conferidos como dados nos testes de `update` e simulados no `elm-program-test`.
- `yarn test` roda tudo em poucos segundos, sem navegador.

**Non-Goals:**
- Testar `src/index.js` (wake lock, vibração real) ou CSS (tema, layout, animações, movimento reduzido, zoom, alvos de 48px). Esses cenários continuam manuais.
- Medir cobertura de linhas com ferramenta (`elm-coverage` está sem manutenção e seu suporte ao Elm 0.19.3 não está garantido). A cobertura é medida pelo mapeamento cenário → teste.
- Configurar CI. O repositório não tem `.github/`; fica para uma change própria.
- Separar `Main.elm` em vários módulos. Só entra o necessário para os testes.

## Decisions

### 1. Tipo `Effect` dentro de `Main.elm`

`init` e `update` passam a devolver `( Model, Effect )`:

```elm
type Effect
    = NoEffect
    | Batch (List Effect)
    | Vibrate (List Int)
    | KeepAwake Bool
    | FocusPlayerName
    | GetTime
    | DrawWord (List Int)
    | After Float Msg
```

- `perform : Effect -> Cmd Msg` traduz para os `Cmd` de hoje: `GetTime` vira `Task.perform (GameMsg << Tick) Time.now`, `DrawWord pool` vira o `Random.generate` atual sobre o pool, e `After ms msg` vira `Process.sleep ms |> Task.perform (\_ -> msg)`.
- `main` usa `init = \_ -> Tuple.mapSecond perform init` e `update = \msg model -> update msg model |> Tuple.mapSecond perform`. `view`, `subscriptions`, `Model` e `Msg` não mudam.
- `lockTaps`, `lockCorrect`, `tickNow` e `newWordIndex` passam a produzir `Effect`. `newWordIndex` devolve `DrawWord (wordPool …)`, então o pool calculado fica visível nos testes.
- `Effect` só contém `Msg`, `Int`, `Float`, `Bool` e listas, então dá para comparar com `==`. Os testes de `update` usam `Expect.equal` direto no efeito.
- Alternativa descartada: um módulo `Effect.elm` separado. `perform` precisa das portas e de `Msg`, que estão em `Main`. Separar exigiria mover `Msg` e as portas e abriria a refatoração em módulos que está fora do escopo.
- Alternativa descartada: só testar `update` puro, sem `elm-program-test`. Não daria para conferir a tela nem os fluxos com toques, que são o centro de vários cenários (toque duplo, bloqueio de 500 ms, campo que mantém o texto).

### 2. Simulação dos efeitos nos testes

Um módulo `tests/Harness.elm` define `simulate : Effect -> SimulatedEffect Msg`:

| Effect | Simulação |
|---|---|
| `Vibrate p` | `SimulatedEffect.Ports.send "vibrate" (Encode.list Encode.int p)` |
| `KeepAwake b` | `SimulatedEffect.Ports.send "keepAwake" (Encode.bool b)` |
| `After ms msg` | `SimulatedEffect.Process.sleep ms |> SimulatedEffect.Task.perform (\_ -> msg)` |
| `GetTime` | `Task.succeed (Time.millisToPosix 0)` → `GameMsg (Tick _)` |
| `DrawWord pool` | `Task.succeed (List.head pool)` → `GameMsg (NewWordIndex _)` |
| `FocusPlayerName` | `SimulatedEffect.Cmd.none` |

- **Sorteio determinístico:** o primeiro índice do pool. As palavras saem na ordem da lista ("Abacate", "Abacateiro", "Abacaxi", …), o que deixa as asserções legíveis. `Harness` também expõe uma variante que recebe a função de escolha (`List Int -> Maybe Int`), para testes que precisam de outra palavra, como a última do pool.
- **Relógio da rodada:** `GetTime` sempre responde 0. Como os ticks vêm de uma assinatura que não dá para simular, os testes mandam `ProgramTest.update (GameMsg (Tick t))` com `t` absoluto. Começar e retomar a rodada põem `lastTick` em 0, e cada teste conta a partir daí. `Harness.ticks from to` manda um tick por segundo entre os dois instantes, sempre abaixo do limite de 2000 ms que pausa a rodada. A alternativa era usar `SimulatedEffect.Time.now`, mas aí o teste teria que somar todos os `advanceTime` anteriores para saber o instante de partida.
- **Bloqueio de toques:** depois de cada troca de tela, `Harness.waitGuard` faz `advanceTime 500`. Os testes de "toque ignorado" simplesmente não avançam o tempo.
- **Portas:** `expectOutgoingPortValues "keepAwake" Decode.bool` e `"vibrate" (Decode.list Decode.int)` conferem a sequência enviada.

### 3. Três camadas de teste

1. **Funções puras** (`tests/HelpersTest.elm`): `getTimeString`, `secondsLeft`, `normalizeName`, `duplicatePlayer`, `wordPool`, `canSkip`, `getCurrentPlayers`, além da ordem das duplas e das posições do placar. Usam `Test.fuzz` onde há invariantes, por exemplo: `getTimeString` sempre tem 5 caracteres para menos de 100 minutos, `secondsLeft` arredonda para cima, `wordPool` nunca devolve um índice já sorteado enquanto houver restantes, e `normalizeName` é idempotente.
2. **`update` mensagem a mensagem** (`tests/UpdateTest.elm`): o que o `elm-program-test` não alcança, como ticks com atraso, intervalo acima de 2000 ms (pausa), `VisibilityChanged Hidden` dentro e fora da rodada, `NewWordIndex` atrasado (stale), ciclo esgotado com `drawnIndex` pré-preenchido, tokens velhos de `ReleaseTaps`/`ReleaseCorrect` e o efeito exato de cada mensagem (por exemplo, `Play` → `Batch [ DrawWord …, GetTime, KeepAwake True ]` mais o `After 500 (ReleaseTaps n)`). O ponto de partida é um `Model` montado por helpers (`playingModel`, `withPlayers`, `withTime`), sem passar pela interface.
3. **Fluxos com `elm-program-test`** (`tests/SetupTest.elm`, `tests/RoundTest.elm`): dirigem a tela com `fillIn`, `clickButton`, `simulateDomEvent` (`submit` do form para o Enter), `advanceTime` e ticks, e conferem a view com `Test.Html.Selector` (texto, `disabled`, `attribute "aria-invalid"`, `attribute "aria-label"`, `class "timer--alert"`, `class "scoreboard__row--leader"`).

Mais `tests/WordsTest.elm` para a lista. Ele confere tamanho igual ao do `Set` (sem duplicatas), cada palavra começando com maiúscula e só com letras e hífen interno (por causa de "Porta-voz"), os exemplos do spec presentes ("Hospital", "Capacete", "Zumbir", "Hacker", "Handebol", "Cacto", "Ducha") e ausentes ("Androceu", "Háfnio", "Haplologia", "Hamadríade", "Phil ips", "Houveu"), e a cobertura das iniciais I, M, P, S e Z.

### 4. Nomes dos testes seguem os specs

Cada `describe` tem o nome do requisito e cada `test` o nome do cenário, como em `describe "Pular custa tempo" [ test "Limite exato" … ]`. Assim, a saída do `elm-test` funciona como checklist. Cenários impossíveis de testar em Elm ficam listados num comentário de módulo em `tests/RoundTest.elm` e `tests/SetupTest.elm`, com o motivo.

### 5. Ferramentas

- `package.json`: devDependency `elm-test@0.19.3-0` e script `"test": "elm-test"`.
- `elm.json`: as dependências de teste entram por `npx elm-test install elm-explorations/test` e `npx elm-test install avh4/elm-program-test`. Se o `install` falhar com Elm 0.19.3, adicionar à mão com as versões resolvidas (test 2.2.1, program-test 4.0.1 e as indiretas do `elm.json` deles).
- Os testes importam `Main` direto. Como é `port module`, o `elm-test` compila as portas sem JS, e elas são só ignoradas.

## Risks / Trade-offs

- [A refatoração para `Effect` pode mudar o comportamento sem querer, por exemplo trocando a ordem de um `Batch` ou esquecendo um `lockTaps`.] → A refatoração vem primeiro, numa tarefa só dela, e é verificada no navegador com os mesmos cenários de antes. Os testes de `update` conferem depois o efeito exato de cada mensagem.
- [`GetTime` respondendo 0 e o sorteio pegando o primeiro do pool diferem do navegador.] → É o preço do determinismo. A tradução real (`perform`) fica com o código de hoje e é coberta pela verificação manual. A lógica que depende do relógio e do sorteio (tempo decorrido, pool, stale) é testada nas camadas 1 e 2 com valores arbitrários.
- [O `elm-program-test` pode não achar botões pelo texto quando há ícone SVG ou `span` extra, como no "Pular −5s".] → Usar o `aria-label` ("Pular (perde 5 segundos)", "Adicionar jogador", "Remover Ana") ou `ProgramTest.within` com seletor de classe.
- [`elm-test@0.19.3-0` é recente e pode ter arestas com o Elm 0.19.3.] → A primeira tarefa só instala e roda um teste trivial. Se falhar, a alternativa é o runner `elm-test-rs`, registrada como decisão na própria tarefa.
- [A lista tem 5456 palavras, e esgotá-la pela interface seria lento.] → O ciclo esgotado é testado na camada 2, com `drawnIndex` pré-preenchido com todos os índices menos um.

## Open Questions

- Se o `elm-program-test` simula `submit` disparado pelo botão `type="submit"` dentro do `form`, ou se o teste precisa mandar o `submit` direto no form. Os dois caminhos estão previstos no harness, e não muda o plano.
