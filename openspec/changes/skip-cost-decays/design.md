# Design

## Context

`currentTime` guarda os milissegundos restantes da rodada. O `Tick` roda a cada 200 ms e desconta o tempo real decorrido, então o valor não fica em segundos inteiros. O cronômetro mostra esse tempo arredondado para cima (`secondsLeft`). `Jump` subtrai um `skipCost` fixo de 5000 ms, e `canSkip` é `currentTime > skipCost`. A palavra nova chega de forma assíncrona em `NewWordIndex`, e o fim da rodada só acontece em `advanceRound`.

O jogo não guarda quando a palavra atual apareceu. Ver proposal.md para a motivação e specs/game-round/spec.md para o comportamento.

## Goals / Non-Goals

**Goals:**
- Fazer o custo de pular cair com o tempo da palavra na tela, sem o custo passar do valor mostrado no botão.
- Manter um único caminho para o fim da rodada.

**Non-Goals:**
- Animar a troca do custo no botão.
- Mudar o bloqueio de 500 ms do Acertou ou a proteção de troca de tela.

## Decisions

### 1. Guardar o tempo restante de quando a palavra apareceu

`GameModel` ganha `wordStartTime : Time.Posix`, na mesma unidade de `currentTime`. `NewWordIndex` o define como `currentTime` ao aceitar a palavra. Esse é o único lugar que troca a palavra e cobre início da rodada, Acertou e Pular.

O custo sai da diferença entre os dois valores:

```elm
skipCost gameModel =
    max 0 (maxSkipCost - (wordStartTime - currentTime))
```

- A pausa funciona sem código extra: `currentTime` fica parado durante a pausa, então o tempo da palavra também fica. Uma interrupção detectada no `Tick` também não desconta nada.
- Em `Jump`, `currentTime` já foi descontado quando `NewWordIndex` chega, então a nova palavra começa do tempo certo.
- Alternativa descartada: guardar o instante real (`Time.Posix` do relógio) em que a palavra apareceu. Isso exigiria compensar as pausas e ler o relógio em `NewWordIndex`.
- Alternativa descartada: um contador `wordElapsed` somado no `Tick`. É mais um estado para manter em sincronia, sem ganho.

### 2. Descontar o valor exato e mostrar o valor arredondado para cima

`Jump` desconta `skipCost` em milissegundos. O botão mostra esse valor arredondado para cima, reaproveitando `secondsLeft`, e assim o rótulo cai 1 por segundo.

- O total fica exato: tempo na tela + custo = 5 s para qualquer pulo antes de 5 s. Em termos de cronômetro, pular leva `currentTime` a `wordStartTime − 5000`.
- O custo real nunca passa do que o botão promete.
- `currentTime − custo` fica constante enquanto a palavra está na tela. É isso que permite a regra simples da Decisão 3.
- Alternativa descartada: custo em degraus (descontar exatamente o número mostrado). Com 5,5 s restantes e o botão mostrando 5, `currentTime − custo` cairia de 0,5 s para −0,1 s e voltaria a subir a cada troca de degrau. O botão ficaria alternando entre habilitado e desabilitado, ou um pulo poderia terminar a rodada. Também faria o jogador pagar a mais: quem pula aos 0,9 s pagaria 5 s, e a palavra consumiria 5,9 s.

### 3. `canSkip` depende só de quando a palavra apareceu

`canSkip gameModel = wordStartTime > maxSkipCost`, e o valor não muda enquanto a palavra está na tela.

- Se a palavra apareceu com mais de 5 s, pular nunca zera o tempo: antes de 5 s na tela o resultado é `wordStartTime − 5000 > 0`, e depois disso o custo é zero com `currentTime > 0`.
- Se a palavra apareceu com 5 s ou menos, a rodada acaba antes de o custo zerar, e qualquer pulo terminaria a rodada. Por isso o botão fica desabilitado. Era o mesmo na regra original: a rodada acabava antes de o Pular ser liberado.
- `Jump` continua sem mudar `status`, e a palavra pulada não corre risco de ser registrada duas vezes no histórico.
- `update` continua checando `canSkip` em `Jump`, como hoje.

### 4. Botão

`viewSkipButton` passa a receber também o custo. Com os segundos arredondados (`n`):

| `n` | Texto | `aria-label` |
| --- | --- | --- |
| 2 a 5 | Pular −Ns | `Pular (perde N segundos)` |
| 1 | Pular −1s | `Pular (perde 1 segundo)` |
| 0 | Pular | `Pular (sem custo)` |

Com o botão desabilitado, o custo continua caindo como nos outros casos. Não vale criar um estado especial, porque o botão apagado já mostra que não dá para pular.

### 5. Testes

Os testes que usam `remaining N` ficam com a mesma palavra desde o início da rodada, então pulariam de graça. Eles passam a chegar ao tempo desejado e tocar em Acertou antes (novo helper), para terem uma palavra recém-sorteada.

Os helpers `skip` e `skipButton` passam a localizar o botão pelo texto "Pular". Isso é mais do que limpeza: `RoundTest` usa `ensureViewHasNot skipButton`, e com o seletor no `aria-label` antigo essas checagens continuariam passando mesmo com o botão na tela, sempre que o custo não fosse 5 s.

## Risks / Trade-offs

- [Se a palavra apareceu num tempo quebrado, o número no botão pode divergir em 1 da queda vista no cronômetro: por exemplo, palavra em 52,4 s, cronômetro em 00:50, botão "−3s" e pulo para 47,4 s, que aparece como 00:48.] → A diferença é menor que 1 s, e o custo nunca passa do valor mostrado. É o preço de evitar o problema dos degraus (Decisão 2).
- [O botão muda de texto a cada segundo enquanto o jogador lê a palavra, o que pode distrair.] → `.skip-cost` usa cor apagada e peso menor, e só muda nos 5 primeiros segundos de cada palavra. Se incomodar nos testes manuais, ajustar em outra change.
- [Pular continua não exigindo espera: quem pula na hora gasta os mesmos 5 s, mas sem tentar a palavra.] → É o que pedimos. Quando dá para tentar, esperar é sempre melhor, porque esse tempo não custa nada a mais e a palavra ainda pode sair.
