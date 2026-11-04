# Proposal

## Why

O projeto tem 158 testes (`yarn test`), mas eles só rodam quando alguém lembra de rodá-los na própria máquina: o repositório não tem `.github/`, então um PR pode ser mesclado em `master` com testes quebrados sem ninguém perceber. A change `add-unit-test-suite` deixou a CI de fora de propósito, para uma change própria. Esta é ela.

## What Changes

- Novo workflow do GitHub Actions em `.github/workflows/tests.yml` que roda `yarn test` em todo pull request, ao abrir, ao receber novos commits e ao ser reaberto.
- O resultado aparece como uma verificação no PR: verde quando os testes passam, vermelha quando algum teste falha ou quando os testes nem chegam a rodar (erro de compilação, lockfile desatualizado).
- O workflow usa a mesma versão do Node do projeto (`.nvmrc`) e as dependências exatas do `yarn.lock`.

Fora do escopo:

- Bloquear o merge quando a verificação falha. Isso é uma regra de proteção da branch `master`, configurada no GitHub e não no repositório. Depois da primeira execução, o dono do repositório pode marcar a verificação como obrigatória.
- Rodar os testes em pushes direto em `master`.
- Rodar `yarn build`, lint ou formatação, e publicar o site.

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

(nenhuma; o workflow é ferramenta de desenvolvimento e não muda o comportamento do jogo, por isso a change usa `skip_specs: true`, como `add-unit-test-suite`)

## Impact

- Novo arquivo `.github/workflows/tests.yml`. Nenhuma mudança em `src/`, `tests/`, `package.json` ou `yarn.lock`.
- Cada PR passa a consumir minutos do GitHub Actions, que são gratuitos em repositório público.
- Dependências externas novas: as actions oficiais `actions/checkout`, `actions/setup-node` e `actions/cache`.
