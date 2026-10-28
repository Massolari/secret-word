# Design

## Context

Hoje o `GameModel` guarda só a palavra atual (`word`), o contador de acertos (`score`) e os índices já sorteados na partida inteira (`drawnIndex`). Nada registra o que aconteceu com cada palavra. O fluxo relevante em `src/Main.elm`:

- `Correct` soma 1 em `score` e pede um novo índice; `Jump` só pede um novo índice. A troca de `word` acontece depois, de forma assíncrona, em `NewWordIndex`.
- `Tick` coloca `status = GameOver` quando o tempo zera.
- `Restart` soma os pontos ao placar, zera `score` e volta para `GameShowingPlayers`.
- `viewGameOver` monta a tela com `.card.result` e o `viewScoreboard`.

Como `drawnIndex` impede repetição na partida, uma palavra aparece no máximo uma vez por rodada.

## Goals / Non-Goals

**Goals:**
- Registrar o resultado de cada palavra no momento da ação, sem depender do sorteio assíncrono.
- Exibir o histórico na tela de fim, no mesmo estilo visual das listas que já existem (placar e resultado).

**Non-Goals:**
- Histórico da partida inteira ou entre rodadas.
- Corrigir o toque duplo rápido em Acertou antes da nova palavra chegar (hoje conta dois acertos para a mesma palavra). O histórico reflete o que o contador registra.
- Permitir corrigir um resultado na tela de fim (ex.: marcar um erro como acerto).

## Decisions

### 1. Modelo: lista de pares com um tipo de resultado

Adicionar ao `GameModel`:

```elm
type WordResult
    = Guessed
    | Missed

, history : List ( String, WordResult )
```

A lista é guardada **invertida** (cada registro entra com `::`) e revertida só na view. Assim cada registro é O(1) e segue o mesmo padrão de `drawnIndex`.

Alternativa considerada: um `Bool` no lugar de `WordResult`. Foi descartada porque o tipo próprio deixa o `case` da view legível e abre espaço para outros resultados no futuro sem mudar a forma do dado.

Alternativa considerada: derivar `score` do histórico (`List.length` dos `Guessed`). Fica fora para não mexer na pontuação nesta change; o cenário "Contagem coerente com os pontos" do spec garante que os dois andam juntos.

### 2. Onde registrar

- `Correct`: `( gameModel.word, Guessed ) :: history`.
- `Jump`: `( gameModel.word, Missed ) :: history`.
- `Tick` que muda o status para `GameOver`: registrar a palavra na tela como `Missed`, **exceto** quando ela já for o primeiro item do histórico. Esse caso acontece se o tempo acabar entre um toque em Acertou/Pular e a chegada de `NewWordIndex`: a palavra já foi registrada e `word` ainda não mudou. Como as palavras não se repetem na partida, comparar a string basta.
- `Restart`: `history = []`.
- `Start`: não precisa mexer, porque `initGame` já começa com `history = []`.

### 3. View na tela de fim

Nova seção depois do `.card.result` e antes do placar, seguindo o padrão de `viewScoreboard`:

```
section.section
  h2.section__title  "Palavras"  + span.section__count (quantidade de acertos/total, ex. "3/5")
  ol.card.history
    li.history__row.history__row--guessed  [Icons.check] "Cacto"
    li.history__row.history__row--missed   [Icons.close] "Ducha"
```

- O ícone (`check` ou um novo `Icons.close`, um "x") resolve a distinção sem depender de cor. A cor vem de `--success` para acerto e `--text-muted` para erro, com a palavra do erro também em `--text-muted`. Erro não usa `--danger`: pular faz parte do jogo, e o vermelho daria peso demais.
- Acessibilidade: o ícone fica `aria-hidden` (padrão de `Icons.icon`), e cada `li` ganha um texto visualmente oculto (`.visually-hidden`, que já existe) com "acertou" ou "errou". Assim o leitor de tela anuncia, por exemplo, "Cacto, acertou".
- A palavra quebra linha com `overflow-wrap: anywhere`, como `.pair__name`, para palavras longas em telas de 320px.
- Ordem: o placar fica por último porque o `main` já rola; o histórico fica perto dos pontos da rodada, que é o contexto dele.

Alternativa considerada: duas listas separadas (acertos e erros). Foi descartada porque perde a ordem da rodada, que ajuda a lembrar da palavra.

### 4. Estilo

Novas regras `.history`, `.history__row`, `.history__row + .history__row`, `.history__icon` e os modificadores em `src/index.css`, copiando as medidas de `.scoreboard` (padding, borda entre linhas, altura mínima um pouco menor, por volta de 44px, já que não há avatar). Sem animação nova, então não precisa de regra extra em `prefers-reduced-motion`.

## Risks / Trade-offs

- [Rodada longa com muitas palavras deixa a tela de fim comprida] → O `main` já tem rolagem e o botão Continuar fica fixo no footer. A lista usa linhas compactas.
- [Toque duplo em Acertou registra a mesma palavra duas vezes] → É o comportamento atual do contador; fica fora do escopo (ver Non-Goals) e o histórico continua coerente com os pontos.
- [Comparar por string no fim do tempo] → Seguro porque `drawnIndex` impede repetição de índice e `Words.words` não tem duplicatas (spec `word-list`).
