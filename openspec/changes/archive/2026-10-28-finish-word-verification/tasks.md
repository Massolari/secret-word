# Tasks

## 1. Extração e preparação

- [x] 1.1 Extrair do array literal de `all` em `src/Words.elm` as palavras pendentes para `pendentes.txt` e as de `verified` para `verificadas.txt`, ambos no scratchpad, uma palavra por linha e na ordem original. Verificar que `wc -l` dá 20.567 e 987.
- [x] 1.2 Dividir `pendentes.txt` em lotes de cerca de 400 palavras, mantendo a ordem. Verificar que a soma das linhas dos lotes dá 20.567.
- [x] 1.3 Escrever o script de validação (scratchpad) que recebe os arquivos de aprovadas e falha nos seguintes casos: palavra que não existe literalmente em `pendentes.txt`, palavra com espaço ou pontuação, palavra que não começa com maiúscula, ou duplicata. Também deve imprimir a taxa de aprovação por letra. Testar com um arquivo contendo "Phil ips" e uma palavra inventada e confirmar que ele falha.

## 2. Classificação

- [x] 2.1 Classificar cada lote com os critérios da Decisão 2 do design e do spec `word-list`, gravando só as aprovadas em `aprovadas/<lote>.txt`. Verificar que existe um arquivo de aprovadas para cada lote.
- [x] 2.2 Rodar o script de validação sobre todas as aprovadas. Verificar que passa sem erros e que a taxa por letra é razoável (faixa inicial de 10% a 20% revista no design: a taxa real ficou em cerca de 22%).
- [x] 2.3 Corrigir em `verificadas.txt` apenas os erros óbvios de formato ou grafia (ex.: "Historia" → "História") e anotar as substituições. Verificar que o validador de formato (sem a checagem de origem) passa em `verificadas.txt`.

## 3. Geração do `Words.elm`

- [x] 3.1 Escrever o script gerador: junta verificadas e aprovadas, remove duplicatas, ordena ignorando acentos e emite `src/Words.elm` com `module Words exposing (words)` e uma única função `words : Array String`, no formato do elm-format. Verificar que "Cacto" e "Ducha" aparecem uma única vez e que nenhuma das entradas "Phil ips", "Houveu", "Androceu" ou "Háfnio" está no arquivo (`grep`).
- [x] 3.2 Verificar que o arquivo gerado tem palavras de pelo menos as letras I, M, P, S e T e que a contagem total está registrada no resumo da implementação.

## 4. Remoção do switch em `Main.elm`

- [x] 4.1 Remover `allWords` do `Model` e do `init`, `ChangeWord` de `PreparingMsg` e do `update`, a célula do switch em `viewPreparing` e o import de `Material.Switch`. Trocar `Words.get model.allWords` e `Words.all` por `Words.words`. Verificar que `grep -n "allWords\|ChangeWord\|Words.get\|Words.all" src/Main.elm` não retorna nada.
- [x] 4.2 Remover a regra `.word-switch` de `src/index.css`. Verificar que `grep word-switch src/` não retorna nada.

## 5. Integração

- [x] 5.1 Compilar com `elm make src/Main.elm --output=/dev/null` e verificar que não há erros nem avisos de imports sem uso.
- [x] 5.2 Rodar o app (`make dev` ou `yarn start`) e verificar que a tela de preparação não tem o switch e que uma partida sorteia palavras normalmente, incluindo de letras além de H.
