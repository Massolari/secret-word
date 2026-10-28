# Design

## Context

Hoje o bloqueio do Pular usa o campo `toShowJump : Int` do `GameModel`. `NewWordIndex` o volta para 5, e cada `Tick` global (`Time.every 1000`) diminui 1. `viewSkipButton` recebe esse número e desenha um anel de progresso (`.skip-ring`). O tempo da rodada fica em `currentTime : Time.Posix`, usado como milissegundos restantes. Cada `Tick` subtrai 1000, e `currentTime <= 0` leva a `GameOver`. `viewTimer` deriva texto, barra e alerta (`<= 10000`) de `currentTime`.

Ver proposal.md para a motivação e specs/game-round/spec.md para o comportamento.

## Goals / Non-Goals

**Goals:**
- Fazer Pular descontar 5 s do tempo restante, usando o mesmo estado que o cronômetro já usa.
- Remover todo o código do bloqueio por palavra.

**Non-Goals:**
- Mudar a forma como o cronômetro conta o tempo (ticks × tempo real). Isso fica para a change `timer-elapsed-time`.
- Proteger contra toque duplo no Pular. Dois toques rápidos e intencionais custam 10 s, e isso é o esperado (cenário "Pulos seguidos"). A proteção contra toques que vazam entre telas é da change `guard-screen-transition-taps`.
- Animar um "−5s" flutuante no cronômetro. A queda imediata do número e da barra já é o retorno visual.

## Decisions

### 1. Descontar direto de `currentTime`

`Jump` subtrai 5000 ms de `currentTime`. Não há outro estado, como um acumulador de penalidade.

- Assim, texto, barra e alerta de `viewTimer` reagem sozinhos, e o `Tick` seguinte continua a partir do novo valor.
- Como o desconto é um múltiplo de 1000, o tempo continua em segundos inteiros, alinhado com a contagem por ticks.
- Alternativa descartada: guardar `penalty : Int` e subtrair na exibição. Isso duplicaria a lógica de fim da rodada (o `Tick` teria de considerar a penalidade) sem nenhum ganho hoje.

### 2. Limite: pular só com mais de 5 s restantes

A regra é `canSkip = currentTime > 5000`, numa função única usada por `update` e por `view`.

- Com isso o pulo nunca leva o tempo a zero. O fim da rodada continua acontecendo só no `Tick`, que é o único lugar que registra a última palavra como erro, vibra e libera o wake lock. Não é preciso duplicar essa transição em `Jump`.
- `update` também checa a regra. Assim, um toque que chegue depois de o botão ficar desabilitado (renderização atrasada) é ignorado: `( model, Cmd.none )`, sem histórico nem sorteio.
- `Jump` não precisa olhar o `status`, porque `viewPlaying` só existe em `GamePlaying`. A checagem de tempo já cobre o fim da rodada, quando `currentTime` é 0.
- Alternativa descartada: permitir pular com exatamente 5 s e terminar a rodada. Isso criaria um segundo caminho para `GameOver`, e a palavra pulada seria registrada duas vezes, como pulo e como a palavra na tela quando o tempo acabou.

### 3. Botão: custo no rótulo, sem anel

`viewSkipButton : Bool -> Html Msg` recebe `canSkip`. O botão é sempre o mesmo `btn btn--secondary btn--lg` com `Icons.skip` e o texto "Pular", seguido de um `span.skip-cost` com "−5s" (sinal de menos U+2212).

- Ganha `aria-label "Pular (perde 5 segundos)"`, para o leitor de tela não ler "menos cinco s".
- Fica com `disabled (not canSkip)` e mantém o `onClick` (um botão desabilitado não dispara clique).
- `.skip-cost` usa peso menor e a cor `--text-muted`, para o custo ficar visível sem competir com o texto principal.
- O estilo `.actions .btn--secondary:disabled { opacity: 0.75 }` continua valendo para o estado sem tempo.

### 4. Remoções

- Sai o campo `toShowJump`: o tipo, o `initGame`, o `newToShowJump` e o campo do record no `Tick`, e o reset em `NewWordIndex`.
- No CSS saem `.skip-ring`, `.skip-ring::before` e `.skip-ring__num`. Também sai `.skip-ring` do seletor `transition: none` no bloco de `prefers-reduced-motion`, que fica só com `.timer__fill`.

## Risks / Trade-offs

- [A barra anima a queda de 5 s com a mesma transição de 1 s usada a cada tick, então o salto parece suave e pode passar despercebido.] → O número MM:SS muda na hora, e o rótulo "−5s" já avisou o custo. Se nos testes manuais a queda não ficar clara, adicionar um destaque curto no cronômetro em outra change.
- [A change `timer-elapsed-time` vai trocar `currentTime` por um cálculo a partir do tempo real.] → Ela precisa preservar o desconto, por exemplo deslocando o instante de término em 5 s. Isso fica registrado no roadmap como dependência.
- [Mudança de regra: quem joga em grupo pode pular muito no começo e esvaziar a rodada.] → É o comportamento pedido. O custo visível no botão e o limite de 5 s restantes evitam que a rodada acabe por acidente.
