# Design

## Context

Hoje o `GameModel` guarda um `generator : Generator Int` fixo (`randomWord (Array.length Words.words)`, que é `Random.int 0 tamanho`, com o limite superior inclusivo) e um `drawnIndex : List Int` com os índices que já saíram. `StartRound`, `Correct` e `Jump` emitem `newWordIndex gameModel.generator`. Em `NewWordIndex index`, se o índice já está em `drawnIndex`, ou se `Array.get` devolve `Nothing` (o índice `tamanho`), o handler emite outro sorteio. Com a lista esgotada, isso nunca para.

O sorteio é assíncrono (`Random.generate` → `Cmd`), então dois toques seguidos podem emitir dois sorteios calculados a partir do mesmo estado. `drawnIndex` não é zerado em `Restart`, então vale para a partida inteira. Ele também serve de chave do `Keyed.node` da palavra em `viewPlaying` (`List.length gameModel.drawnIndex`), que reinicia a animação de entrada a cada palavra.

Não há testes automatizados no projeto. Os changes anteriores verificam com `npx elm make` e jogando no navegador.

## Goals / Non-Goals

**Goals:**
- Um sorteio por palavra, sempre dentro dos índices válidos, sem tentativa e erro.
- Comportamento definido e testável quando a lista se esgota.

**Non-Goals:**
- Impedir que um toque duplo em Acertou mostre duas palavras seguidas (`guard-correct-double-tap`).
- Persistir as palavras sorteadas entre recarregamentos ou entre partidas.

## Decisions

### 1. Sortear entre as palavras restantes, não rejeitar

O gerador passa a depender do estado: a cada pedido, monta o conjunto de índices ainda não sorteados (`0 .. tamanho - 1` menos os sorteados) e sorteia uma posição `Random.int 0 (restantes - 1)` dentro dele. O campo fixo `generator` sai do `GameModel`. `newWordIndex` passa a receber o `GameModel` (ou o conjunto de sorteados) e constrói o gerador na hora. Isso também resolve o off-by-one: `randomWord` deixa de existir na forma atual e o limite superior passa a ser sempre `tamanho_do_conjunto - 1`.

`drawnIndex` vira `Set Int`, para que a pertinência e a diferença não dependam de `List.member` em 5456 itens.

*Alternativa considerada:* embaralhar todos os índices no início da partida e consumir a lista em ordem. É finito e eficiente, mas `elm/random` não tem `shuffle` (precisaria de `elm-community/random-extra` ou de um Fisher–Yates próprio), e a lista embaralhada precisaria ser regenerada a cada ciclo. Montar o conjunto restante (O(n) com n = 5456, uma vez por palavra) é barato e mantém o modelo simples.

*Alternativa considerada:* manter a rejeição só com o intervalo corrigido e um limite de tentativas. Continua degradando perto do esgotamento e não define o que acontece depois.

### 2. Esgotamento: novo ciclo, sem a palavra da tela

Quando o conjunto restante está vazio, o pool do sorteio passa a ser a lista inteira menos o índice da palavra na tela, e os sorteados recomeçam a partir da palavra escolhida. Se a lista tiver uma palavra só, o pool é a lista inteira. Para saber o índice da palavra na tela, o modelo guarda `currentIndex : Maybe Int` em vez de depender só de `word : String`.

*Por que esta e não outras:* encerrar a partida ou mostrar um aviso exigiria uma tela nova para um caso que, com 5456 palavras e rodadas de até 3 minutos, só aparece depois de dezenas de horas de jogo. Repetir sem controle (sortear de toda a lista) reintroduziria repetições logo no começo da partida. Um novo ciclo silencioso mantém a promessa "sem repetição até passar pela lista toda". Essa escolha é uma suposição do planejamento e está registrada na proposal.

### 3. Respostas atrasadas em `NewWordIndex`

Como dois sorteios podem sair do mesmo estado, o handler revalida o índice recebido contra o estado atual. Se ele já saiu no ciclo atual (e não é um começo de ciclo), o handler emite um novo sorteio a partir do estado atual, cujo pool já exclui esse índice. Isso termina: cada re-sorteio parte de um pool que não contém o índice rejeitado, e só há re-sorteio quando existem pedidos concorrentes, que são finitos. O caminho `Array.get … Nothing` deixa de existir na prática. Se ele acontecer, o handler não re-sorteia: mantém a palavra atual, para não recriar um loop.

### 4. Chave da animação da palavra

A chave do `Keyed.node` deixa de ser o tamanho de `drawnIndex`, que cai ao reiniciar o ciclo e poderia coincidir com um valor anterior. Ela passa a ser um contador `drawCount : Int` que só cresce na partida, incrementado a cada palavra aceita.

## Risks / Trade-offs

- [Montar o conjunto restante a cada palavra custa O(n)] → n = 5456 e ocorre no máximo algumas vezes por segundo. É desprezível no celular. Se a lista crescer muito, dá para trocar por um embaralhamento único sem mudar o spec.
- [Verificar o esgotamento exige passar por 5456 palavras] → a verificação manual troca `Words.words` temporariamente por uma lista de 3 palavras, sem commitar, e confere o novo ciclo. A lógica de pool fica numa função pura, fácil de revisar isoladamente.
- [Respostas atrasadas ainda podem mostrar duas palavras seguidas num toque duplo] → fora do escopo, coberto por `guard-correct-double-tap`.
