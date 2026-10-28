# Tasks

## 1. Corrigir o wake lock em `src/index.js`

- [ ] 1.1 Adicionar `pendingRequest` e reescrever `requestWakeLock` para retornar cedo quando houver pedido pendente ou lock ativo e não liberado, guardar o sentinel só se `!lock.released` e, no fim do pedido (sucesso ou falha), zerar `pendingRequest` e liberar se `wantsWakeLock` for `false` (design, decisões 1 e 3). Verificar lendo o diff: nenhum caminho deixa `pendingRequest` preso.
- [ ] 1.2 Fazer o listener de `release` zerar `wakeLock` só quando `wakeLock === lock` (design, decisão 2), mantendo `releaseWakeLock` síncrono na referência (decisão 4). Verificar lendo o diff.
- [ ] 1.3 Rodar `yarn build` e verificar que o build termina sem erros.

## 2. Verificação manual no navegador

- [ ] 2.1 Com `yarn start`, colar no console, antes de iniciar a partida, um `navigator.wakeLock` simulado cujo `request()` resolve depois de ~1s com um sentinel falso (`released`, `release()` que dispara `release` de forma assíncrona, contador global de sentinels ativos). Verificar que a rodada começa e termina normalmente com o simulado.
- [ ] 2.2 Cenário "volta ao app logo depois de começar": começar a rodada e disparar `document.dispatchEvent(new Event("visibilitychange"))` antes de 1s. Verificar que o contador chega a 1 (e não 2) e volta a 0 depois do fim da rodada.
- [ ] 2.3 Cenário "rodada termina antes de o pedido ser atendido": usar rodada curta ou forçar o fim antes de o simulado resolver. Verificar que o contador fica em 0 depois que o pedido resolve.
- [ ] 2.4 Cenário "nova rodada logo depois do fim": com `release()` simulado atrasado, terminar uma rodada e começar a próxima imediatamente. Verificar que o contador fica em 1 durante a nova rodada (o evento atrasado do lock antigo não apaga a referência) e em 0 depois do fim dela.
- [ ] 2.5 Num navegador real com suporte (Chrome no Android ou desktop), jogar uma rodada completa sem o simulado. Verificar que não aparecem erros no console e que a tela não apaga durante a rodada.

## Workflow follow-up

- Arquivar a change com `/opsx:archive` depois da revisão.
- Remover o item 5 de `roadmap-changes.md` quando a change for arquivada, se o roadmap ainda estiver em uso.
