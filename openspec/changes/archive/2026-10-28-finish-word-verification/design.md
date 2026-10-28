# Design

## Context

Estado atual de `src/Words.elm` (21.581 linhas):

- `verified`: 987 palavras de A a H. A tem cerca de 300; B a G têm cerca de 100 cada; H tem 61.
- `all = Array.append verified (Array.fromList [...])`: o array literal guarda as 20.567 palavras **pendentes**. São as sobras de A a H, a partir do ponto onde a curadoria manual parou em cada letra (ex.: A a partir de "Andino", B a partir de "Bessarábio", H só "Houveu"), mais as letras I a Z inteiras.
- 44 palavras aparecem duplicadas entre as pendentes, e "Cacto" e "Ducha" estão nas duas listas.
- Há entradas corrompidas por OCR, com espaços internos ("Anti antidiarreico", "Dum Ping", "Ombus Man") ou sem sentido ("Ainsa", "Apreai", "Houveu").

Na curadoria de 2020 (commit `509d4b5`, letra H) foram aprovadas 61 de 484 palavras, cerca de 13%. Exemplos do critério usado:

- Aprovadas: Hambúrguer, Helicóptero, Hipopótamo, Hospital, Hacker, Herdar, Humilde.
- Rejeitadas: Hábil, Habitual, Haver, Hadoque, Háfnio, Haplologia, Haraquiri, Hanseático.

O padrão favorece substantivos concretos e verbos ou adjetivos fáceis de explicar por dicas.

Em `src/Main.elm`, a escolha de lista aparece em `allWords` (modelo), `ChangeWord` (mensagem), no switch da `viewPreparing`, em `Words.get model.allWords` (sorteio e tamanho do gerador) e em `Words.all` (gerador inicial em `initGame`).

## Goals / Non-Goals

**Goals:**
- Decidir cada uma das 20.567 palavras pendentes de forma reprodutível e auditável durante a implementação.
- Garantir mecanicamente as regras de formato e unicidade do spec `word-list`.
- Deixar o código com uma única lista e sem o switch.

**Non-Goals:**
- Reavaliar o critério de dificuldade das 987 já verificadas. Elas entram como estão, com exceção de correções de formato (duplicatas e erros óbvios de grafia, como "Historia" → "História").
- Criar níveis de dificuldade, categorias ou dicas.
- Corrigir ou reescrever palavras pendentes corrompidas. Elas são simplesmente descartadas.

## Decisions

### 1. Classificação feita por Claude, em lotes, sem revisão manual
As palavras pendentes são extraídas para um arquivo de trabalho fora do repositório (scratchpad), uma por linha, e divididas em lotes de cerca de 400 palavras, mantendo a ordem original. Para cada lote, Claude aplica os critérios do spec `word-list` e grava apenas as **aprovadas** em `aprovadas/<lote>.txt`. Os lotes são independentes, então podem ser processados em paralelo.

- *Alternativa:* listas de frequência de palavras, como um corpus de frequência do português. Foi descartada porque frequência não mede "dá para descrever no jogo": palavras frequentes como "Haver" ou "Habitual" foram rejeitadas em 2020. Também traria uma dependência externa.
- *Alternativa:* revisão manual por amostragem. O usuário optou por aplicar direto.

### 2. Critério de aprovação (calibração)
Uma palavra é aprovada quando todas as condições abaixo valem:

1. Um adulto brasileiro médio a reconhece de imediato.
2. Dá para dar dicas sobre ela sem dizê-la, o que favorece substantivos concretos, ações e características comuns.
3. Não é técnica, científica, erudita, arcaica, regional rara, gentílico obscuro nem nome próprio pouco conhecido.
4. Está bem formada (regras de formato do spec).

Plurais e flexões redundantes entram só quando a forma base não está na lista. Caso de dúvida conta como rejeição. A estimativa inicial era aprovar entre 10% e 20% das palavras de cada letra. Na implementação, a taxa geral ficou em cerca de 22% (4.472 de 20.520 palavras únicas). As letras pequenas ficaram mais altas: X com 65% e Z com 45%, porque têm poucas palavras e quase todas comuns. Depois da revisão, a taxa foi aceita como reflexo do critério e não forçada para dentro da faixa, porque o objetivo é aprovar todas as palavras que passam.

### 3. Apenas seleção, nunca reescrita
Toda palavra aprovada tem de existir literalmente na lista pendente de origem. Um script de validação confere isso e impede que palavras inventadas ou "corrigidas" entrem por engano. Há duas exceções:

- As correções de grafia explícitas nas 987 verificadas (Non-Goals), registradas numa lista curta de substituições.
- A **correção só de acentuação** em palavras pendentes, como "Arvore" → "Árvore" e "Aniversario" → "Aniversário". Ela fica registrada no arquivo de aprovadas como `Origem=>Corrigida`. O validador exige que a origem exista e que as duas formas sejam iguais quando os acentos são removidos. Sem essa exceção, palavras ótimas que só perderam o acento na origem seriam descartadas.

### 4. Geração do `Words.elm` por script
Um script em Python (no scratchpad, fora do repo) junta as verificadas e as aprovadas e faz três coisas:

1. Remove duplicatas.
2. Valida o formato: começa com maiúscula, não tem espaço nem pontuação e é única.
3. Ordena alfabeticamente ignorando acentos.

Depois o script reescreve `src/Words.elm` no estilo elm-format atual, com uma única função `words : Array String`. Remover `all`, `verified` e `get` deixa o módulo simples e o diff de `Main.elm` pequeno.

### 5. Simplificação do `Main.elm`
- Remover `allWords` do `Model` e do `init`.
- Remover `ChangeWord` de `PreparingMsg` e seu branch no `update`.
- Remover a célula do switch na `viewPreparing`.
- Trocar `Words.get model.allWords` e `Words.all` por `Words.words`.
- Remover `.word-switch` de `src/index.css`.
- Se o import de `switch`/`switchConfig` ficar sem uso, removê-lo também.

## Risks / Trade-offs

- [Classificação subjetiva com erros de borda, para mais ou para menos] → Critério explícito e calibrado com as decisões de 2020, conferência da taxa de aprovação por letra e histórico do git para recuperar palavras.
- [Palavra inventada ou com grafia alterada entrar na lista] → A validação "existe na origem" (Decisão 3) bloqueia a geração.
- [Lista final pequena em letras raras (X, Z, U)] → Aceitável. Não há cota mínima por letra.
- [Remoção do switch é perceptível para quem usava "Todas as palavras"] → Intencional, já que todas as palavras restantes são boas.

## Migration Plan

Não há dados persistidos nem estado salvo do jogador, então basta fazer o deploy do novo build. Para rollback, basta reverter o commit.
