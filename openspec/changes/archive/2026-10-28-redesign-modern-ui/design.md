# Design

## Context

Motivação e escopo: ver `proposal.md`. Requisitos: `specs/ui-theme`, `specs/game-setup` e `specs/game-round`.

Estado atual relevante:

- Toda a interface está em `src/Main.elm` (`Browser.element`), com componentes do `material-components-web-elm` 9.1.0 (custom elements `mdc-*`, cujo JS e CSS são importados em `src/index.js`). O `mdgriffith/elm-ui` está no `elm.json`, mas não é importado.
- `src/index.css` só ajusta o tema do MDC. Fontes: Roboto e Material Icons, pelo Google Fonts.
- O tempo da rodada vem de um `input type="time"` guardado como `String` (`"MM:SS"` interpretado como minutos:segundos) e convertido para `Time.Posix` em `Start`. `getTimeString` formata o tempo restante com `Time.toMinute zone`, o que dá resultado errado em fusos com deslocamento que não é hora cheia.
- `Tick` chega a cada 1s via `Time.every 1000`, só no estado `GamePlaying`. Ele decrementa `currentTime` e `toShowJump` (bloqueio do Pular, reiniciado em 5 a cada palavra nova).
- `AddUser` não valida o nome: enviar o formulário com o campo vazio adiciona um jogador sem nome.
- O projeto não tem testes automatizados. A verificação é `elm make`, `yarn build` e um roteiro manual no navegador.

## Goals / Non-Goals

**Goals:**
- Reescrever só a camada de apresentação. `update` muda apenas onde o spec pede (tempo por presets, validação do nome, foco, remoção animada, portas).
- Manter o CSS como única fonte do visual: o Elm emite classes e algumas custom properties (`--progress`); cores, espaços e animações ficam no CSS.

**Non-Goals:**
- Adotar framework de CSS (Tailwind etc.) ou etapa de build de CSS além do que o Parcel já faz.
- Mudar `Words.elm` ou corrigir o sorteio (`Random.int 0 arrayLength` pode gerar um índice fora do array; o ramo `Nothing` já sorteia de novo).
- Persistência, roteamento ou PWA.

## Decisions

### 1. `Html` puro com CSS próprio, sem MDC e sem elm-ui

As views usam `Html`/`Html.Attributes` com classes no estilo BEM curto (`.btn`, `.btn--primary`, `.chip`, `.player`, `.word`...). O `material-components-web-elm` e o `mdgriffith/elm-ui` saem do `elm.json`, e o pacote npm sai do `package.json`.

*Por quê:* o visual do MDC é difícil de descaracterizar, os custom elements limitam animações (ripple próprio, shadow DOM parcial) e o JS do MDC pesa no bundle. O elm-ui gera estilos inline, o que atrapalha media queries (`prefers-color-scheme`, `prefers-reduced-motion`) e keyframes.

*Alternativa:* manter o MDC e reestilizar. Foi descartada pelo usuário.

### 2. Design tokens em custom properties

`src/index.css` começa com tokens em `:root`: cores semânticas (`--bg`, `--surface`, `--surface-2`, `--text`, `--text-muted`, `--accent`, `--on-accent`, `--success`, `--on-success`, `--warning`, `--danger`, `--border`), espaços (`--space-1..6`, em escala de 4px), raios (`--radius-sm/md/lg/full`), sombras e durações (`--dur-fast: 120ms`, `--dur: 220ms`, `--dur-slow: 360ms`) com easing (`--ease-out: cubic-bezier(.2,.8,.2,1)`). O tema escuro redefine só as cores, dentro de `@media (prefers-color-scheme: dark)`. `color-scheme: light dark` no `:root` faz controles nativos e barras de rolagem acompanharem o tema.

Direção visual: fundo neutro levemente frio (claro: quase branco; escuro: quase preto azulado), superfícies em cartão com borda sutil em vez de sombra forte, uma cor de destaque (índigo/violeta) para ações e seleção, verde para "Acertou" (nunca vermelho), âmbar e depois vermelho para o alerta de tempo. O tom exato pode ser ajustado na implementação, desde que passe nos contrastes do spec.

*Alternativa:* classes utilitárias. Foram descartadas porque, para um app de quatro telas, tokens mais componentes ficam menores e mais legíveis.

### 3. Tipografia e ícones

- Fonte: **Plus Jakarta Sans** (400, 600, 800) via Google Fonts com `display=swap` e fallback `system-ui, sans-serif`. O cronômetro e os números usam `font-variant-numeric: tabular-nums`, para os dígitos não "dançarem".
- Ícones: novo módulo `src/Icons.elm` com funções `play`, `plus`, `x`/`trash`, `check`, `skip`, `clock`, `users` e `trophy`, em SVG inline (`viewBox 0 0 24 24`, traço `currentColor`, `aria-hidden="true"`). Isso exige `elm/svg` como dependência direta. O link do Material Icons é removido.

*Alternativa:* manter a fonte Material Icons. Foi descartada porque é um download grande para oito ícones e o texto do ícone pisca antes de a fonte carregar.

### 4. Estrutura de layout

Toda tela renderiza o mesmo esqueleto:

```
.app            (min-height: 100dvh; grid: header / main / footer; max-width: 480px; margin: auto)
  .app__header  (título ou cronômetro)
  .app__main    (conteúdo; overflow-y: auto)
  .app__footer  (ação principal; position: sticky; bottom: 0;
                 padding-bottom: max(var(--space-4), env(safe-area-inset-bottom)))
```

- `src/index.html`: `<html lang="pt-BR">`, `viewport` com `viewport-fit=cover`, duas `<meta name="theme-color">` com `media` para claro e escuro.
- Inputs com `font-size: 16px` (evita o zoom do iOS); `touch-action: manipulation` em `button`, `label` e `input` (evita zoom por duplo toque); `-webkit-tap-highlight-color: transparent`, já que o feedback vem do `:active`.
- Botões: altura mínima de 48px (os principais, 56px), `:active { transform: scale(.97) }`, `:focus-visible` com anel em `--accent`, `:disabled` com opacidade reduzida e `pointer-events: none`.

### 5. Telas

- **Preparação:** cabeçalho com o nome do jogo. Bloco "Tempo da rodada" em controle segmentado. Bloco "Jogadores" com campo e botão de adicionar, lista em cartões (inicial do nome num círculo, nome e botão remover) ou estado vazio. Rodapé com "Começar" e, abaixo, a dica "Falta(m) N jogador(es)".
- **Anúncio da dupla:** cartão central com "Ana dá as dicas" → seta → "Beto adivinha", e o tempo da rodada parado. Placar abaixo. Rodapé com "Começar rodada".
- **Rodada:** cabeçalho com cronômetro grande e barra de progresso. Centro com a palavra em tamanho fluido (`clamp(2.5rem, 13vw, 4.5rem)`, peso 800, `overflow-wrap: anywhere`, `hyphens: auto`) e o contador de acertos abaixo. Rodapé com dois botões grandes lado a lado: "Pular" (secundário, cerca de 1/3) e "Acertou" (verde, cerca de 2/3). Sem placar.
- **Fim da rodada:** "Tempo esgotado!" e os pontos da dupla em destaque (adivinhador e quem deu as dicas), depois o placar. Rodapé com "Continuar".

O placar é um componente único: lista ordenada por pontos, com posição, nome e pontos, e o primeiro colocado destacado com o ícone `trophy` e a cor de destaque. Em empate na liderança, destacar todos os empatados.

### 6. Tempo da rodada por presets

`PreparingModel.time : String` vira `roundSeconds : Int`, com valor inicial 60. `InputTime String` vira `SelectTime Int`. Os presets ficam numa lista `[ 30, 60, 90, 120, 180 ]`, com rótulos "30s", "1min", "1min30", "2min" e "3min". `Start` deixa de fazer o parse de string e passa a usar `Time.millisToPosix (roundSeconds * 1000)`.

O controle é um `fieldset` com `input type="radio"` visualmente oculto mais um `label` estilizado como segmento. Assim vêm de graça a acessibilidade (setas, leitor de tela) e o estado `:checked` no CSS.

`getTimeString` passa a calcular minutos e segundos direto dos milissegundos, sem `Time.Zone`. Com isso saem `zone`, `AdjustTimeZone` e o `Task.perform Time.here` do `init`. O resultado é o mesmo em fusos de hora cheia e passa a ficar correto nos outros.

### 7. Validação do nome e foco

`AddUser` aplica `String.trim` e ignora nome vazio. O botão de adicionar fica `disabled` quando o nome, sem espaços nas pontas, está vazio. Depois de adicionar, `update` retorna `Task.attempt (always NoOp) (Browser.Dom.focus "player-name")`, para manter o teclado aberto (isso exige um `Msg` `NoOp`). O campo usa `enterkeyhint="done"`, `autocapitalize="words"`, `autocomplete="off"` e `maxlength` (24), e fica dentro do `form` com `onSubmit`.

### 8. Animações

Todas em CSS (`@keyframes` + `transition`). O Elm só provoca o (re)mount dos elementos:

- **Entrada de tela:** o conteúdo de `.app__main` e do rodapé fica num `Html.Keyed` com chave por tela (`"setup"`, `"pair-<índice>"`, `"round"`, `"over"`). A chave nova monta um elemento novo, que roda `@keyframes screen-in` (fade + translateY de 12px, `--dur-slow`).
- **Troca de palavra:** `Html.Keyed` com chave `String.fromInt (List.length drawnIndex)`, que muda a cada sorteio mesmo que a palavra se repita no futuro. Animação `word-in`: fade + scale de .92 para 1, `--dur`.
- **Acerto:** o número do contador fica num nó keyed pelo próprio valor e roda `pop` (scale 1 → 1.25 → 1). O botão Acertou ganha um "flash" com `:active`.
- **Lista de jogadores:** `Html.Keyed.ul` com chave pelo id. Item novo roda `item-in`. Para a **saída**, `DeletePlayer id` primeiro põe o id em `removing : Set Int` (o item ganha a classe `.player--leaving`, que roda `item-out`, colapsando altura e opacidade em 200ms) e agenda `Process.sleep 200 |> Task.perform (always (RemovePlayer id))`, que remove de fato. Itens em `removing` contam como removidos para a regra de começar.
- **Cronômetro:** a barra usa `transform: scaleX(var(--progress))` com `transition: transform 1s linear`. O `--progress` vem do Elm (`currentTime / time`), então a barra anda suave entre os ticks sem dessincronizar. Com 10s ou menos, `.timer--alert` troca a cor e liga uma pulsação suave (`pulse`, 1s).
- **Bloqueio do Pular:** um anel em `conic-gradient` com `--progress` vindo de `toShowJump / 5`, com transição de 1s, mais o número no centro. Ao liberar, o anel some e aparece o ícone `skip`.

Movimento reduzido: um bloco `@media (prefers-reduced-motion: reduce)` zera `transform` nas animações (mantém só a opacidade, com `--dur-fast`), desliga `pulse` e põe a transição da barra em `none`.

*Alternativa:* animar a saída com bibliotecas JS ou Web Animations via portas. Foi descartada: o estado `removing` é pequeno e mantém tudo em Elm e CSS.

### 9. Portas para vibração e tela acesa

`Main` vira `port module` com:

```elm
port vibrate : List Int -> Cmd msg
port keepAwake : Bool -> Cmd msg
```

- `Correct` → `vibrate [ 30 ]`.
- `Tick` que leva a `GameOver` → `Cmd.batch [ vibrate [ 120, 80, 120, 80, 240 ], keepAwake False ]`.
- `Play` → `keepAwake True`.

Em `src/index.js`: `vibrate` chama `navigator.vibrate?.(pattern)`. `keepAwake` guarda a intenção numa variável e chama `navigator.wakeLock?.request("screen")` (ou `release()` no sentinel). Um listener de `visibilitychange` pede o lock de novo quando a aba volta a ficar visível e a intenção ainda é `true`, porque o navegador solta o lock ao esconder a aba. Toda chamada fica em `try/catch` ou `.catch(() => {})`. A inscrição nas portas verifica `app.ports?.vibrate`, porque o Elm elimina portas não usadas.

*Alternativa:* chamar `navigator.vibrate` por um atributo ou evento no HTML. Não há como fazer isso sem JS, e as portas são o caminho idiomático.

### 10. Dependências

- `elm.json`: remover `aforemny/material-components-web-elm` e `mdgriffith/elm-ui` de `direct`, e mover `elm/svg` de `indirect` para `direct`. Ajustar as indiretas ao que o compilador exigir (`elm/json` e `elm/url` continuam, por causa do `elm/browser`).
- `package.json`/`yarn.lock`: `yarn remove material-components-web-elm`.
- `src/index.js`: remover os dois imports do MDC.

## Risks / Trade-offs

- [Perda do ripple e das animações prontas do MDC] → feedback por `:active` (escala e cor) e as animações da decisão 8, que cobrem todas as interações.
- [Wake Lock só existe em contexto seguro e em navegadores recentes (Safari 16.4+)] → recurso progressivo, com falha silenciosa. `localhost` conta como seguro no desenvolvimento.
- [Vibração não existe no iOS] → recurso progressivo. O feedback visual (pop do contador) já basta sozinho.
- [Google Fonts indisponível ou lento] → `display=swap` e fallback `system-ui`. O layout não depende da métrica da fonte.
- [Tick de 1s não alinhado ao momento em que a palavra aparece: o primeiro decremento do bloqueio pode vir antes de 1s] → é o comportamento atual, que o spec preserva ("5 segundos" medidos em ticks). A barra com transição linear esconde o salto.
- [Estado `removing` cria uma janela de 200ms em que o jogador ainda está no `Dict`] → a regra de começar e o `Start` consideram `players` menos `removing`, e o `RemovePlayer` é idempotente.
- [Palavras muito longas em tela de 320px] → tamanho fluido com `clamp`, mais `overflow-wrap: anywhere` e `hyphens: auto` com `lang="pt-BR"`.

## Migration Plan

Mudança só de cliente, entregue em um único commit (ou uma sequência curta) na `master`. Não há dados nem estado persistido. Rollback: `git revert` do(s) commit(s) e `yarn install`, para restaurar o pacote npm do MDC.

## Open Questions

- Tom exato da cor de destaque e do fundo escuro: ajustar visualmente na implementação, respeitando os contrastes do spec `ui-theme`.
