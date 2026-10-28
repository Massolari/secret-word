# Tasks

## 1. Estado do bloqueio do Acertou

- [ ] 1.1 Adicionar o campo `correctGuard : TapGuard` ao `Model`, iniciado como `{ locked = False, token = 0 }` em `init`, e a mensagem de topo `ReleaseCorrect Int`, que desbloqueia só quando o token é o atual, igual a `ReleaseTaps` (Decisão 1 do design). Verificar com `npx elm make src/Main.elm --output=/dev/null`.

## 2. Travar e liberar

- [ ] 2.1 Criar `lockCorrect`, no formato de `lockTaps`, que incrementa o token de `correctGuard`, marca `locked = True` e agenda `Process.sleep tapGuardMillis |> Task.perform (\_ -> ReleaseCorrect token)`. Chamar no ramo de `NewWordIndex` que define a palavra, sem mexer nos ramos que sorteiam de novo. Verificar que compila.
- [ ] 2.2 Em `Correct` e `Jump` (só no ramo em que `canSkip` é verdadeiro), marcar `correctGuard.locked = True` com o token incrementado, sem agendar liberação (Decisão 2). Verificar que compila e que `Jump` com `canSkip` falso não mexe no bloqueio.
- [ ] 2.3 Em `acceptsGameMsg`, no caso `Correct`, exigir também `not model.correctGuard.locked`, sem mudar `Jump` (Decisão 3). Verificar que compila e que o botão Acertou não recebe `disabled` nem classe nova (Decisão 4).

## 3. Verificação integrada

- [ ] 3.1 Rodar `yarn start`, começar uma rodada, esperar a primeira palavra e tocar duas vezes rápido em Acertou. Verificar o cenário "Toque duplo em Acertou": o contador sobe 1, e a palavra que apareceu depois do primeiro toque continua na tela e, no fim da rodada, não aparece como acerto no histórico.
- [ ] 3.2 Tocar em Pular e logo em seguida em Acertou. Verificar que o contador não muda e a palavra nova continua na tela. Tocar duas vezes rápido em Pular com tempo sobrando e verificar que o cronômetro cai 10 s ("Pulos seguidos" continua valendo).
- [ ] 3.3 Fazer vários acertos esperando cerca de 1 s entre eles, pelo toque e pelo teclado (Enter no botão focado). Verificar que todos contam e que, no fim da rodada, os acertos do histórico batem com os pontos.
