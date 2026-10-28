# Tasks

## 1. Ações valem só na própria tela

- [ ] 1.1 Em `updateGame`, fazer `Play` agir só em `GameShowingPlayers`, `Correct` e `Jump` só em `GamePlaying` e `Restart` só em `GameOver`. Em qualquer outro status, devolver `( model, Cmd.none )`. Em `update`, fazer `Start` agir só em `Preparing`. Verificar com `npx elm make src/Main.elm --output=/dev/null`.

## 2. Bloqueio curto depois da troca de tela

- [ ] 2.1 Adicionar `TapGuard` (`locked : Bool`, `token : Int`) e o campo `tapGuard` no `Model`, iniciado como desbloqueado em `init`. Adicionar a constante `tapGuardMillis = 500` e a mensagem de topo `ReleaseTaps Int`, que desbloqueia só quando o token é o atual (Decisão 2 do design). Verificar que compila.
- [ ] 2.2 Criar `lockTaps`, que incrementa o token, marca `locked = True` e devolve `Process.sleep tapGuardMillis |> Task.perform (\_ -> ReleaseTaps token)`. Chamar em `Start`, `Play`, `Restart` e no `Tick` com `justEnded`, juntando ao `Cmd` que cada um já tem com `Cmd.batch`. Verificar que compila e que o `Tick` comum (sem fim de rodada) não ativa o bloqueio.
- [ ] 2.3 Ignorar `Play`, `Restart`, `Correct` e `Jump` enquanto `tapGuard.locked` for `True`. Verificar que compila e que os botões não recebem `disabled` nem classe nova (Decisão 4).

## 3. Verificação integrada

- [ ] 3.1 Rodar `yarn start`, numa rodada de 30s tocar repetidamente em Acertou até o tempo acabar, e verificar o cenário "Acertou no fim do tempo": a tela de fim continua visível e os acertos do histórico batem com os pontos.
- [ ] 3.2 Tocar duas vezes rápido em Continuar e verificar que o anúncio da próxima dupla aparece e continua visível, e que o placar recebeu os pontos uma vez só.
- [ ] 3.3 Tocar duas vezes rápido em Começar rodada e verificar que o contador começa em 0 e a primeira palavra não está marcada como acerto. Fazer o mesmo com Começar na preparação e verificar que o anúncio da primeira dupla continua visível.
- [ ] 3.4 Esperar mais de meio segundo em cada tela e verificar que Começar, Começar rodada, Acertou, Pular (depois dos 5 s) e Continuar respondem a um toque normal, inclusive pelo teclado (Enter no botão focado).
