# Design

## Context

`viewScoreboard` em `src/Main.elm` ordena `Dict.toList model.players` por `negate player.score` e usa `List.indexedMap`, mostrando `index + 1` como posição. A liderança já é decidida pela pontuação (`topScore > 0 && player.score == topScore`), por isso o empate na liderança já funciona; só a posição numérica dos demais vem do índice. `List.sortBy` é estável, então a ordem entre empatados é a de `Dict.toList` (pela chave do jogador) e continua a mesma.

## Goals / Non-Goals

**Goals:**
- Derivar a posição exibida da pontuação, sem mexer em ordenação, destaque, estilos ou modelo.

**Non-Goals:**
- Desempate (por exemplo, por ordem de cadastro ou por quem pontuou antes).
- Texto acessível para a posição do líder: o troféu continua só visual, como hoje.
- Mudar a ordem das linhas entre jogadores empatados.

## Decisions

### 1. Classificação de competição ("1, 2, 2, 4")

A posição é `1 + número de jogadores com pontuação estritamente maior`. Com `[5, 3, 3, 1]`: 1, 2, 2, 4.

- **Alternativa considerada:** classificação densa ("1, 2, 2, 3"). Descartada porque esconde quantos jogadores estão à frente: quem aparece em 3º teria três pessoas na frente. A classificação de competição é a usada em esportes e bate com o número da linha quando não há empate.
- O exemplo do roadmap (`[5, 3, 3]` → 🏆, 2, 2) dá o mesmo resultado nas duas; a escolha só aparece depois de um empate.

### 2. Calcular no `let` de `viewScoreboard`, a partir das pontuações

Montar `scores = List.map (Tuple.second >> .score) players` (já usado para `topScore`) e calcular a posição de cada jogador com `1 + List.length (List.filter (\s -> s > player.score) scores)`. Trocar `List.indexedMap` por `List.map`, porque o índice deixa de ser usado.

- É O(n²), irrelevante para o número de jogadores de uma partida presencial.
- **Alternativa considerada:** percorrer a lista já ordenada com um fold, carregando a posição e a pontuação anterior. É O(n), mas exige mais código e depende da ordenação estar correta; a contagem direta é autoexplicativa e independe da ordem.

### 3. Liderança sem mudança

O troféu continua decidido por `leader`, como hoje. Um líder sempre tem posição 1 pela regra acima, então troféu e número nunca divergem. Com todos em 0, `leader` é falso para todos e a posição calculada é 1 para todos, como pede o spec.

## Risks / Trade-offs

- [Vários jogadores com a mesma posição podem parecer erro para quem espera números seguidos] → É o comportamento pedido no roadmap e o padrão de classificações esportivas.
- [A lista numerada (`ol`) continua anunciando "1 de 4, 2 de 4..." para leitores de tela, independente da posição exibida] → Fora do escopo; não piora o que existe hoje.
