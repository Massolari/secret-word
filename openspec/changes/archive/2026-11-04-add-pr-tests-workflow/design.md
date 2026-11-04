# Design

## Context

Veja `proposal.md` para a motivação. O que molda a solução:

- `yarn test` chama `elm-test`, que compila `src/` e `tests/`. Numa instalação limpa com `yarn install --frozen-lockfile`, os 158 testes passam em menos de 1 s depois de compilar.
- O binário do Elm vem do npm: `elm` declara `@elm_binaries/linux_x64` como dependência opcional, e essa versão está no `yarn.lock`. Não há download de binário fora do `yarn install`.
- Os pacotes Elm do `elm.json` não vêm do npm. O compilador os baixa de `package.elm-lang.org` para `~/.elm`.
- O repositório é público, usa Yarn 1 (sem campo `packageManager`), tem o Actions habilitado e não exige fixar actions por SHA.

## Goals / Non-Goals

**Goals:**

- Um único arquivo de workflow, sem mudar scripts nem dependências do projeto.
- Rodar nos PRs a mesma coisa que o desenvolvedor roda localmente: `yarn test`.

**Non-Goals:**

- Matriz de versões do Node ou de sistemas operacionais. O jogo roda no navegador; o Node só serve de ferramenta.
- Workflows reutilizáveis ou actions próprias.

## Decisions

O arquivo fica assim:

```yaml
name: Tests

on:
  pull_request:

permissions:
  contents: read

jobs:
  test:
    runs-on: ubuntu-latest
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v7
      - uses: actions/setup-node@v7
        with:
          node-version-file: .nvmrc
          cache: yarn
      - uses: actions/cache@v6
        with:
          path: ~/.elm
          key: elm-${{ runner.os }}-${{ hashFiles('elm.json') }}
      - run: yarn install --frozen-lockfile
      - run: yarn test
```

### 1. Gatilho `pull_request`, sem filtros

Os tipos padrão (`opened`, `synchronize`, `reopened`) cobrem o pedido. Sem filtro de `branches`, um PR empilhado em outra branch também é testado. Sem filtro de `paths`, porque uma verificação filtrada nunca reporta resultado nos PRs que não tocam nesses caminhos, e o PR fica preso se ela virar obrigatória.

Alternativa descartada: `pull_request_target`. Ele roda com permissões da branch base, o que seria arriscado junto com código vindo de forks em um repositório público. Com `pull_request`, o PR de um fork roda com token somente leitura. Outro efeito útil do `pull_request`: o workflow já roda no próprio PR que o adiciona, o que permite verificá-lo antes do merge.

### 2. Token com `contents: read`

O job só lê o código. Declarar a permissão mínima evita depender do padrão configurado no repositório.

### 3. Node a partir do `.nvmrc`

`node-version-file: .nvmrc` mantém uma única fonte para a versão. Fixar `24` no workflow criaria uma segunda fonte que pode ficar desatualizada.

### 4. `--frozen-lockfile`

Faz a execução falhar se o `yarn.lock` não bater com o `package.json`, em vez de atualizá-lo em silêncio. Assim os testes rodam com as versões que foram revisadas no PR.

### 5. Cache de `~/.elm` além do cache do Yarn

`cache: yarn` cobre as dependências do npm, mas não os pacotes Elm. O cache de `~/.elm`, com chave no hash do `elm.json`, deixa as execuções mais rápidas e, principalmente, evita que uma indisponibilidade de `package.elm-lang.org` derrube a CI de um PR que não mudou dependências Elm.

Alternativa descartada: cachear também `elm-stuff/`. Os artefatos de compilação economizariam pouco, já que o projeto compila em segundos, e arriscariam resultados vindos de artefatos antigos.

### 6. Actions fixadas por versão maior

`@v7`, `@v7` e `@v6` são as versões maiores atuais das actions oficiais, e recebem correções sem manutenção por aqui. Fixar por SHA protege mais contra uma tag adulterada, mas exige um processo de atualização que o projeto não tem, e o job roda sem segredos e com token somente leitura, o que limita o estrago.

### 7. Nome do job estável: `test`

Regras de proteção de branch referenciam o nome do job. Um nome curto e estável permite marcar a verificação como obrigatória mais tarde sem precisar mudar o arquivo.

## Risks / Trade-offs

- [Os testes só foram rodados localmente em macOS com Node 26, não em Linux com Node 24] → A primeira execução no PR que adiciona o workflow confirma isso antes do merge (tarefa 2.1).
- [O workflow depende do Yarn 1 vir pré-instalado na imagem `ubuntu-latest`] → Se a imagem deixar de trazê-lo, a execução falha de forma visível. A correção é instalar o Yarn em um passo antes do `setup-node`.
- [`package.elm-lang.org` fora do ar quando o `elm.json` muda e o cache ainda não existe] → A execução fica vermelha sem culpa do PR. É raro e se resolve rodando o job de novo.
- [Sem proteção de branch, a verificação vermelha só avisa, não impede o merge] → É uma decisão consciente desta change (veja `proposal.md`).

## Migration Plan

Não há migração. Para reverter, basta apagar `.github/workflows/tests.yml`.
