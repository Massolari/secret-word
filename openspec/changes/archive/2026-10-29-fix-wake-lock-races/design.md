# Design

## Context

O wake lock vive só em `src/index.js`. O Elm manda `keepAwake True` em `Play` e `keepAwake False` no `Tick` que encerra a rodada. O JS guarda `wantsWakeLock` (o que a rodada quer) e `wakeLock` (o `WakeLockSentinel` atual), e pede de novo em `visibilitychange` porque o navegador libera o lock sozinho quando a página fica oculta.

Hoje o código faz assim:

- `requestWakeLock` só olha para `wakeLock`, que continua `null` durante o `await navigator.wakeLock.request(...)`. Duas chamadas nesse intervalo (`keepAwake True` seguido de `visibilitychange`, ou fim e começo de rodada em sequência) criam dois sentinels. O segundo sobrescreve o primeiro, que fica ativo sem referência.
- O listener de `release` faz `wakeLock = null` sem conferir de qual sentinel veio. `releaseWakeLock` zera a referência na hora, mas o evento chega depois, de forma assíncrona. Se nesse meio-tempo um novo lock foi atribuído, o evento do antigo apaga a referência ao novo, e o próximo `releaseWakeLock` não tem o que liberar.

Restrições: a Screen Wake Lock API não tem cancelamento de um `request()` em andamento. O projeto não tem testes de JS nem lint, e o build é Parcel (`yarn build`).

## Goals / Non-Goals

**Goals:**
- Nunca ter mais de um sentinel vivo ao mesmo tempo, e nenhum depois que `wantsWakeLock` vira `false` e o pedido em andamento termina.
- Fazer com que eventos de sentinels antigos não mexam no estado atual.

**Non-Goals:**
- Mudar quando o Elm pede ou libera (a porta e as chamadas em `Main.elm` ficam iguais).
- Tratar a pausa da rodada em segundo plano (é a change `timer-elapsed-time`).
- Adicionar infraestrutura de testes de JS.

## Decisions

### 1. Guardar o pedido pendente em vez de serializar com uma fila

Uma variável `pendingRequest` (a promise do `request()` em andamento) faz `requestWakeLock` retornar cedo enquanto houver pedido pendente ou lock ativo. Quando a promise termina, o código decide o destino do sentinel com base em `wantsWakeLock` *naquele momento*. Isso cobre tanto "pediu de novo enquanto pendente" (o pedido é absorvido) quanto "a rodada acabou enquanto pendente" (libera ao chegar).

Alternativa considerada: encadear todas as operações numa fila de promises (`queue = queue.then(...)`). Funciona, mas é mais código e mais difícil de ler para dois estados simples. Como o estado desejado já está em `wantsWakeLock`, basta reconciliar no fim do pedido.

Esboço:

```js
async function requestWakeLock() {
    if (!navigator.wakeLock || pendingRequest || (wakeLock && !wakeLock.released)) return
    pendingRequest = navigator.wakeLock.request("screen")
    try {
        const lock = await pendingRequest
        if (!lock.released) {
            lock.addEventListener("release", () => {
                if (wakeLock === lock) wakeLock = null
            })
            wakeLock = lock
        }
    } catch (_) {
    } finally {
        pendingRequest = null
    }
    if (!wantsWakeLock) releaseWakeLock()
}
```

### 2. O listener de `release` compara identidade

Cada listener fecha sobre o próprio sentinel e só zera `wakeLock` se `wakeLock === lock`. É a menor mudança que corrige a segunda corrida, e funciona igual para liberação pedida pelo jogo e para liberação feita pelo navegador (página oculta).

### 3. Conferir `released` em vez de confiar só no evento

Se a página fica oculta enquanto o pedido está pendente, o sentinel pode chegar já liberado, com o evento disparado antes de o listener existir. Por isso o código só guarda o sentinel quando `!lock.released`, e o guarda de `requestWakeLock` trata um `wakeLock` já liberado como ausente. Assim, o `visibilitychange` seguinte consegue pedir de novo.

### 4. `releaseWakeLock` continua síncrono na referência

`releaseWakeLock` continua zerando `wakeLock` na hora e chamando `release()` sem esperar. Com a decisão 2, o evento atrasado do lock antigo é inofensivo, e um `keepAwake True` logo em seguida pode pedir um lock novo sem esperar a liberação do antigo. Durante esse intervalo curto pode haver dois sentinels, mas o antigo já está sendo liberado e nenhum fica sem dono.

## Risks / Trade-offs

- [Um `keepAwake True` durante um pedido pendente que vai falhar fica sem lock até o próximo `visibilitychange`] → Aceitável: falha de wake lock já é silenciosa pelo spec. Se o pedido falhou, outro pedido imediato provavelmente falharia pelo mesmo motivo.
- [Sem testes automatizados, a regressão pode voltar] → A verificação manual usa um `navigator.wakeLock` simulado com atraso controlado (ver tasks), que dá para repetir rápido. O código fica pequeno e isolado num arquivo.
- [Navegadores sem `released` no sentinel] → A propriedade faz parte da API desde o início em Chrome e Safari. Onde faltar, `lock.released` é `undefined`, e o comportamento cai no atual (o sentinel é guardado).
