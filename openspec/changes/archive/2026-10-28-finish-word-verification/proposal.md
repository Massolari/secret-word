# Proposal

## Why

O jogo sorteia muitas palavras difíceis ou obscuras, o que estraga a partida. Em 2020 começou uma curadoria manual em `src/Words.elm`: as palavras aprovadas iam para `verified` e as rejeitadas eram apagadas de `all`. Como o trabalho era lento, parou no meio. Só 987 palavras foram verificadas, todas de A a H, e ainda restam **20.567 palavras sem revisão**, entre elas lixo de digitalização como `"Phil ips"`, `"Araini ng"` e `"Houveu"`. Esta change termina a curadoria com classificação automatizada feita por Claude.

## What Changes

- Classificar cada uma das 20.567 palavras pendentes (o trecho de `all` que fica fora de `verified`) como **aprovada** ou **rejeitada**, seguindo critérios explícitos de "palavra boa para o jogo" (ver design).
- Mover as aprovadas para a lista final e **apagar** as rejeitadas, o lixo e as duplicatas. Isso segue o que os commits de curadoria anteriores faziam.
- Sem cota por letra: entra toda palavra que atender aos critérios.
- As 987 palavras já verificadas continuam, deduplicadas.
- **BREAKING (UI)**: remover o switch "Palavras verificadas / Todas as palavras" da tela de preparação, o campo `allWords` do modelo, a mensagem `ChangeWord` e a função `Words.get`. Ao final existe **uma única lista** de palavras.
- Remover a regra CSS `.word-switch`, que deixa de ser usada.

## Capabilities

### New Capabilities

- `word-list`: o conjunto de palavras que o jogo pode sortear, com os critérios de qualidade e as regras de formato que toda palavra da lista deve cumprir.

### Modified Capabilities

(nenhuma; ainda não há specs no projeto)

## Impact

- `src/Words.elm`: reescrito com uma única lista (`words`) no lugar de `all`/`verified`/`get`. O arquivo deve ficar bem menor.
- `src/Main.elm`: remoção de `allWords`, `ChangeWord`, do switch na view e dos usos de `Words.get`/`Words.all`. O gerador de índice passa a usar o tamanho da lista única.
- `src/index.css`: remoção de `.word-switch`.
- Não há dependências novas. A classificação é feita uma vez, durante a implementação, e não roda em tempo de execução.
- Risco: a classificação automática pode errar em casos de borda. Como foi pedido aplicar direto, sem revisão manual, o histórico do git é o caminho para recuperar alguma palavra.
