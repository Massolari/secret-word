# Tasks

## 1. Regra do pulo

- [ ] 1.1 Remover o campo `toShowJump` do `GameModel` e de `initGame`, o cálculo `newToShowJump` e o campo correspondente no `Tick`, e o `toShowJump = 5` em `NewWordIndex`. Verificar com `npx elm make src/Main.elm --output=/dev/null` (vai acusar o uso em `viewPlaying`, resolvido em 2.1).
- [ ] 1.2 Criar `skipCost = 5000` e `canSkip : GameModel -> Bool` (`Time.posixToMillis currentTime > skipCost`) em `src/Main.elm` (Decisão 2 do design).
- [ ] 1.3 Em `Jump`, quando `canSkip`, subtrair `skipCost` de `currentTime`, registrar a palavra como `Missed` e sortear uma nova. Caso contrário, devolver `( model, Cmd.none )`. Verificar no código que `Jump` nunca muda o `status`, já que o fim da rodada continua só no `Tick`.

## 2. Botão Pular

- [ ] 2.1 Reescrever `viewSkipButton : Bool -> Html Msg` (Decisão 3): ícone, "Pular", `span.skip-cost` com "−5s", `aria-label "Pular (perde 5 segundos)"`, `disabled (not canSkip)` e `onClick (GameMsg Jump)`. Chamar com `canSkip gameModel` em `viewPlaying`. Verificar que `npx elm make src/Main.elm --output=/dev/null` compila sem erros.
- [ ] 2.2 Em `src/index.css`, remover `.skip-ring`, `.skip-ring::before` e `.skip-ring__num`, tirar `.skip-ring` do bloco `prefers-reduced-motion` e adicionar `.skip-cost` (peso menor, `--text-muted`). Verificar com `grep -n skip-ring src/` que não sobra nenhuma referência e, no navegador, que "−5s" é legível nos temas claro e escuro.

## 3. Verificação integrada

- [ ] 3.1 Rodar `yarn start` e jogar uma rodada de 1min. Verificar os cenários "Pular logo que a palavra aparece" e "Pulos seguidos" do spec: o botão funciona assim que a palavra aparece, cada toque tira 5 s do cronômetro e da barra, e o contador de acertos não muda.
- [ ] 3.2 Na mesma rodada, pular até restar pouco tempo. Verificar os cenários "Limite exato", "Tempo insuficiente" e "Pulo leva aos últimos segundos": o alerta aparece ao cruzar 10 s, e com 5 s ou menos o botão fica desabilitado sem trocar a palavra.
- [ ] 3.3 Deixar o tempo acabar e verificar na tela de fim que o histórico lista as palavras puladas como erro e a última palavra uma única vez.
- [ ] 3.4 Com um leitor de tela (VoiceOver), verificar que o botão é anunciado como "Pular (perde 5 segundos)" e como indisponível quando desabilitado.

## Workflow follow-up

- Atualizar `roadmap-changes.md`: marcar o item 3 (`skip-lock-per-word`) como substituído por `skip-costs-time` e anotar no item 4 (`timer-elapsed-time`) que o desconto de 5 s do Pular precisa ser preservado.
- Arquivar a change depois de revisada e testada.
