# Design

## Context

Em `src/Main.elm`, `view` monta cada tela como um `Screen` com `header`, `main` e `footer`. Os três ficam num `Keyed.node` cuja chave depende da tela (`setup`, `pair-<índice>`, `round`, `over`). Por isso cada troca de tela recria o rodapé com botões novos, todos com `btn--lg` e no mesmo lugar:

- preparação: Começar (`Start`);
- anúncio: Começar rodada (`GameMsg Play`);
- rodada: Pular (`Jump`) à esquerda e Acertou (`Correct`) à direita;
- fim: Continuar (`GameMsg Restart`).

Hoje nenhum handler em `updateGame` checa o status. `Correct` soma ponto em qualquer estado, e dois `Restart` seguidos avançam duas duplas. O Elm redesenha no próximo quadro de animação, então dois cliques no mesmo quadro chegam os dois ao botão antigo. Além disso, um toque que chega depois do redesenho cai no botão novo da tela seguinte.

O `Tick` que leva a `GameOver` é a única troca que não vem de um toque. Ela acontece enquanto o jogador ainda está tocando em Acertou.

## Goals / Non-Goals

**Goals:**
- Barrar as duas formas do problema: a ação que chega à tela errada (checagem de status) e o toque que cai no botão novo (bloqueio por tempo).
- Fazer o bloqueio no `update`, para que valha para toque, clique e teclado sem depender do DOM.

**Non-Goals:**
- Toque duplo em Acertou durante a rodada (`guard-correct-double-tap`).
- Bloquear os botões que não ficam no rodapé (ex.: remover jogador).
- Mudar o layout para afastar os botões das telas seguintes.

## Decisions

### 1. Duas camadas: status e janela de tempo

**Status.** Cada mensagem de ação passa a agir só no status a que pertence. Em qualquer outro status ela vira no-op:

| Mensagem | Status em que vale |
|---|---|
| `Play` | `GameShowingPlayers` |
| `Correct`, `Jump` | `GamePlaying` |
| `Restart` | `GameOver` |

`Start` já só existe na tela de preparação. Ainda assim ele passa a checar `model.status == Preparing`, pelo mesmo motivo.

Essa camada deixa explícita a regra do spec, de que uma ação só vale na tela a que pertence. Ela não depende do tempo: um `Correct` que chegue em `GameOver` é descartado mesmo depois que o bloqueio acaba.

**Janela de tempo.** Ao entrar numa tela nova, o modelo fica "bloqueado" por 500 ms. Enquanto isso, `Play`, `Restart`, `Correct` e `Jump` são ignorados. Isso cobre o toque que cai no botão novo depois do redesenho, que a checagem de status não resolve, porque nesse caso o status já é o da tela nova. Também cobre os cliques que chegam no mesmo quadro: o `update` trata uma mensagem por vez, então o segundo `Restart` já encontra o bloqueio ativado pelo primeiro.

Alternativa considerada: só a janela de tempo. Ela resolveria todos os casos do roadmap, mas a regra "a ação pertence à tela" ficaria implícita e dependeria de toda transição futura lembrar de bloquear. A checagem de status custa uma linha por mensagem. Ficam as duas.

### 2. Estado do bloqueio com token

No `Model` (e não no `GameModel`, porque `Start` sai da preparação):

```elm
, tapGuard : TapGuard

type alias TapGuard =
    { locked : Bool
    , token : Int
    }
```

Função auxiliar, chamada em cada transição:

```elm
lockTaps : Model -> ( Model, Cmd Msg )
-- locked = True, token = token + 1
-- Cmd: Process.sleep 500 |> Task.perform (\_ -> ReleaseTaps novoToken)
```

Nova mensagem de topo `ReleaseTaps Int`. Ela só desbloqueia se o token for o atual. Assim, um timer antigo não libera cedo demais o bloqueio de uma troca mais recente (ex.: duas trocas em menos de 500 ms).

Alternativa considerada: guardar o instante da troca (`Time.Posix`) e comparar com o instante do toque. Seria preciso pegar a hora com `Time.now` em cada clique e mudar todos os `onClick`. O token com `Process.sleep` segue o padrão que já existe (`DeletePlayer` → `RemovePlayer`) e não mexe nas views.

### 3. Onde ativar

- `Start`: depois de montar o modelo da partida.
- `Play`: junto do `newWordIndex` e do `keepAwake True`.
- `Tick`: só quando `justEnded`, junto do `vibrate` e do `keepAwake False`.
- `Restart`: depois de somar os pontos.

Cada um junta o `Cmd` do bloqueio com `Cmd.batch` aos comandos que já tinha.

### 4. Sem mudança visual

Os botões não recebem `disabled` durante o bloqueio. O estilo `.btn:disabled` (opacidade 0.45) piscaria a cada troca, e a animação `screen-in` do rodapé (`--dur-slow`, 360 ms) já cobre boa parte da janela. Leitores de tela não são afetados na prática, porque navegar até o botão leva mais que 500 ms.

### 5. Duração de 500 ms

Um toque duplo leva por volta de 300 ms e toques rápidos ficam abaixo disso. 500 ms cobre esses casos com folga e ainda é curto para quem quer avançar de propósito. O valor fica numa constante (`tapGuardMillis`) para ajustar depois.

## Risks / Trade-offs

- [Um toque intencional rápido logo após a troca é ignorado] → A janela é curta e o botão continua lá. Basta tocar de novo.
- [Leitor de tela anuncia o botão como ativo durante o bloqueio] → Na prática não chega a tocar nele dentro de 500 ms (Decisão 4).
- [Tempo de aba em segundo plano: `Process.sleep` pode atrasar e manter o bloqueio por mais tempo] → Ao voltar, o timer dispara logo. O pior caso é um toque ignorado.
- [Interação com `guard-correct-double-tap`] → Aquela change vai tratar `Correct` dentro da rodada. O bloqueio daqui vale só logo depois da troca e pode ser reaproveitado ou mantido separado lá.
