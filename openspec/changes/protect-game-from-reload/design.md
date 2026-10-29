# Design

## Context

`src/Main.elm` é um `Browser.element` sem rotas, então a página tem uma só entrada no histórico e nada do estado é salvo. `Model.status` é `Preparing` ou `Playing`. `Start` passa para `Playing`, e hoje nada volta para `Preparing` (a change `end-game-to-setup` vai adicionar isso). Dentro de `Playing`, `GameModel.status` alterna entre `GameShowingPlayers`, `GamePlaying`, `GamePaused` e `GameOver`.

Já existe infraestrutura que esta change reaproveita:

- `pauseRound` leva a rodada para `GamePaused`, zera `lastTick` e libera a tela acesa, com `lockTaps`.
- `acceptsGameMsg` filtra as ações de cada tela (`Play`, `Correct`, `Jump`, `Restart`, `Resume`) por tela atual e pelo `tapGuard`.
- `src/index.js` já fala com o Elm por ports (`vibrate`, `keepAwake`), sempre com `app.ports?.x?.subscribe`.

Os comportamentos exigidos estão em `specs/game-session/spec.md`.

## Goals / Non-Goals

**Goals:**
- Um único ponto no Elm decide se a proteção está ligada; o JS só executa.
- O voltar nunca sai da página sem passar pelo diálogo enquanto a proteção estiver ligada, inclusive com o diálogo já aberto.
- Funcionar sem mudar a URL (nada de hash ou rotas).

**Non-Goals:**
- Salvar a partida e retomar depois de recarregar (descartado na decisão do roadmap).
- Proteger os jogadores cadastrados na preparação.
- Texto próprio na confirmação de recarregar: os navegadores mostram só a mensagem padrão.
- Encerrar a partida e voltar à preparação (change `end-game-to-setup`). O design só deixa o port `protectGame False` pronto para ela.

## Decisions

### 1. Ports: o Elm manda, o JS executa

```elm
port protectGame : Bool -> Cmd msg
port leaveGame : () -> Cmd msg
port backPressed : (() -> msg) -> Sub msg
```

- `Start` envia `protectGame True` junto com o que já faz.
- `backPressed` é assinado em `subscriptions` sempre que `model.status == Playing`, em `Sub.batch` com as assinaturas de `GamePlaying` que já existem (tick e visibilidade).
- Sair envia `leaveGame ()`.

Alternativa considerada: o JS ligar a proteção sozinho ao detectar a partida (por exemplo, observando o DOM). Ficaria acoplado à view e duplicaria a regra de quando há partida. Com o port, `end-game-to-setup` só precisa enviar `protectGame False` ao voltar para a preparação.

### 2. `beforeunload` só enquanto protegido

Em `index.js`, um handler `onBeforeUnload(e) { e.preventDefault(); e.returnValue = "" }` é adicionado com `addEventListener` quando `protectGame(true)` e removido quando `protectGame(false)` ou `leaveGame`. Deixar o listener registrado só quando necessário evita que o navegador desative o bfcache da página na preparação.

Enquanto a confirmação nativa está na tela, a página fica congelada. Se isso acontecer com a rodada correndo, o próximo tick vê um intervalo maior que `interruptionMillis` e pausa a rodada, como no requisito "Pausa ao sair da tela". Não precisa de código novo para isso.

### 3. Voltar interceptado com uma entrada de guarda no histórico

- `protectGame(true)`: se ainda não está protegido, `history.pushState({ secretWordGuard: true }, "")` (mesma URL) e `guarded = true`. O controle é por uma variável em memória, e não por `history.state`, porque o estado do histórico sobrevive ao recarregamento e enganaria a próxima partida.
- `popstate` com `guarded`: o voltar acabou de tirar a entrada de guarda. O JS empilha uma nova guarda na hora (`pushState` de novo) e envia `app.ports.backPressed.send(null)`. Assim há sempre uma guarda no topo enquanto a proteção está ligada, e um segundo voltar com o diálogo aberto também cai no Elm em vez de sair da página.
- `popstate` sem `guarded`: ignora.
- `leaveGame()`: `guarded = false`, remove o `beforeunload` e chama `history.go(-2)`: o −1 tira a guarda e o −2 é o voltar que o jogador pediu.
- `protectGame(false)` (uso futuro): `guarded = false`, remove o `beforeunload` e, se a guarda estiver no topo, `history.back()` para não deixar uma entrada morta.

Alternativa considerada: só empilhar a guarda de novo quando o jogador escolhe Continuar partida. É mais simples, mas entre o voltar e o toque não há guarda, e um segundo voltar sairia da página com o diálogo aberto, o que contraria o spec.

### 4. Estado do diálogo no `Model`

Adicionar `exitPrompt : Bool` ao `Model`, com `False` em `init`. Novas mensagens no `Msg` de topo (não são ações de uma tela específica):

```elm
| BackPressed
| StayInGame
| LeaveGame
```

- `BackPressed`:
  - fora de `Playing`: ignora;
  - diálogo aberto: igual a `StayInGame`;
  - diálogo fechado: `exitPrompt = True`. Se `gameModel.status == GamePlaying`, aplica antes `pauseRound`, que já trata `lastTick`, `keepAwake False` e o `lockTaps`. Foca o botão Continuar partida com `Browser.Dom.focus`.
- `StayInGame`: `exitPrompt = False`, com `lockTaps`, para que um toque que fechou o diálogo não caia no botão do rodapé que ficou embaixo.
- `LeaveGame`: envia `leaveGame ()`. O modelo não muda: a página vai sair.

`acceptsGameMsg` ganha a condição `not model.exitPrompt` para as ações de tela, o que cobre o requisito de bloquear a tela de trás mesmo se um evento passar pelo fundo. `Tick`, `NewWordIndex` e `VisibilityChanged` continuam passando: com a rodada pausada, o tick não é assinado.

### 5. View do diálogo

`view` acrescenta, quando `exitPrompt`, um nó depois dos três filhos do `Keyed.node` (chave `"exit-prompt"`), para não remontar a tela de trás:

- fundo `.dialog-backdrop` cobrindo a tela inteira, que recebe os toques sem fazer nada;
- `.dialog` (reaproveitando `.card`) com `role="dialog"`, `aria-modal="true"` e `aria-labelledby` para o título "Sair da partida?", o texto "O placar e a vez de cada dupla serão perdidos." e os botões **Continuar partida** (`btn--primary btn--lg btn--block`, com o id usado no foco) e **Sair** (`btn--ghost btn--block`), com a ação principal embaixo, ao alcance do polegar, como no rodapé das telas;
- Esc → `StayInGame`, ouvido em `Browser.Events.onKeyDown` (assinado em `subscriptions` só com o diálogo aberto) e não no elemento do diálogo: um toque no texto do diálogo tira o foco para o `body`, e um `keydown` no diálogo deixaria de receber a tecla.

Em `index.css`: `.dialog-backdrop` com `position: fixed; inset: 0`, fundo escurecido semitransparente e o diálogo ancorado embaixo dentro da área segura, com `fade-in` no fundo e `screen-in` no diálogo. No bloco `prefers-reduced-motion`, o diálogo passa a usar só `fade-in`, como as outras telas. As cores vêm dos tokens existentes, então o tema escuro sai junto.

O Esc precisa de um decoder de evento, então `elm/json` passa de dependência indireta para direta em `elm.json`, na mesma versão 1.1.4. Nenhum pacote novo é baixado.

## Risks / Trade-offs

- [iOS Safari ignora `beforeunload` na maioria dos casos] → Aceito e documentado no proposal. No iOS o voltar continua protegido pela guarda, que é o caso mais comum (gesto de voltar sem querer).
- [Jogo aberto numa aba nova, sem página anterior] → Sair chama `history.go(-2)` e não há para onde ir. O navegador fica na página, com a partida na tela e a proteção desligada. Mitigação: nenhuma por enquanto. Um novo voltar segue o comportamento normal do navegador (no Android, fecha a aba). Se isso incomodar, `end-game-to-setup` pode fazer Sair voltar para a preparação nesse caso.
- [Guarda que sobra depois de recarregar] → Depois de recarregar, a entrada de guarda da partida anterior continua no histórico. O primeiro voltar na preparação só tira essa entrada e não sai da página. É um toque a mais, sem perda de dados. Não dá para apagar entradas do histórico pelo JS.
- [O Chrome pula, no voltar, entradas criadas por páginas com que o usuário ainda não interagiu] → A guarda só é criada depois de o jogador cadastrar pelo menos dois nomes e tocar em Começar, então a página já teve interação e as guardas valem. Isso é conferido na tarefa de verificação no Chrome Android.
- [Chrome pede interação com a página antes de mostrar a confirmação de `beforeunload`] → Na partida o jogador já tocou em Começar, então a condição sempre está satisfeita.
