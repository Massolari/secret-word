# Proposal

## Why

O sorteio de palavras é feito por rejeição: sorteia um índice qualquer e, se ele já saiu (ou não existe), sorteia de novo. Quando todas as 5456 palavras já saíram na partida, nenhum índice é aceito e o app entra num loop infinito de sorteios que congela a aba. Além disso, `Random.int 0 arrayLength` inclui `arrayLength`, um índice que não existe e que hoje só é descartado por mais uma volta no loop.

## What Changes

- O sorteio passa a escolher só entre as palavras ainda não sorteadas na partida. Cada sorteio termina em um passo, sem tentativa e erro.
- O intervalo do sorteio é corrigido para cobrir exatamente os índices válidos da lista (`0` a `tamanho - 1`).
- Definição do esgotamento: quando todas as palavras da lista já saíram na partida, o sorteio começa um novo ciclo com a lista inteira, deixando de fora só a palavra que está na tela, para que ela não se repita logo em seguida. A partida continua normalmente, sem aviso. *(Esta é uma suposição deste planejamento, já que o roadmap pedia para definir o comportamento. As alternativas descartadas estão no design.)*
- O requisito "Acertar" deixa de prometer uma palavra "ainda não sorteada na partida" em qualquer caso, e passa a remeter à nova regra de sorteio, que vale também para "Pular" e para a primeira palavra da rodada.

## Capabilities

### New Capabilities

_Nenhuma._

### Modified Capabilities

- `game-round`: novo requisito "Sorteio de palavras" (sem repetição na partida, sempre finito, novo ciclo ao esgotar a lista). O requisito "Acertar" passa a remeter a ele.

## Impact

- `src/Main.elm`: `randomWord`, `newWordIndex`, o tratamento de `NewWordIndex`, os campos `generator` e `drawnIndex` de `GameModel` e os pontos que pedem uma nova palavra (`StartRound`, `Correct`, `Jump`). A chave do `Keyed.node` da palavra em `viewPlaying` hoje deriva de `drawnIndex` e precisa continuar mudando a cada nova palavra.
- Sem novas dependências (só `elm/random` e `elm/core`). Sem mudança visual.
- Fora do escopo: toque duplo em "Acertou" (`guard-correct-double-tap`) e bloqueio do "Pular" (`skip-lock-per-word`), que têm changes próprias no roadmap.
