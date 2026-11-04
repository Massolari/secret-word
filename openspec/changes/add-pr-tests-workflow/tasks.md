# Tasks

## 1. Workflow

- [x] 1.1 Criar `.github/workflows/tests.yml` com o conteúdo definido em `design.md`. Verificar com `actionlint`, se estiver disponível, que o arquivo é válido; a validação definitiva é a tarefa 2.1. Verificar com `git status` que nenhum outro arquivo fora de `.github/` e da change mudou.

## 2. Verificação em um PR real

- [ ] 2.1 Publicar a branch e abrir um PR para `master`. Verificar que a verificação `test` aparece no PR e fica verde, que o log mostra Node 24 e que os 158 testes passam em Linux.
- [ ] 2.2 Enviar um novo commit ao PR e verificar que a verificação roda de novo e que o log do passo `actions/cache` mostra a restauração de `~/.elm` (cache hit).
- [ ] 2.3 Enviar um commit temporário com um teste que falha e verificar que a verificação fica vermelha. Reverter o commit e verificar que ela volta ao verde e que o diff final do PR não toca em `tests/`.

## Workflow follow-up

- Arquivar a change depois do merge.
- Opcional: marcar `test` como verificação obrigatória nas regras de proteção da branch `master`, nas configurações do repositório no GitHub.
