# Tasks

## 1. Estados, mensagens e pontuação

- [ ] 1.1 Adicionar `GameConfirmingEnd GameStatus` e `GameFinished` a `GameStatus`, e `AskEnd`, `CancelEnd`, `ConfirmEnd` e `NewGame` a `GameMsg` (Decisões 1 e 2). Ligar as variantes novas em `viewGame` com views provisórias. Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [ ] 1.2 Extrair de `Restart` o helper `addRoundPoints : GameModel -> Dict Int Player -> Dict Int Player` (Decisão 4) e usar o helper em `Restart`. Verificar que compila e que, no navegador, uma rodada de 5 acertos de Ana para Beto ainda dá +5 para Beto e +2 para Ana depois de Continuar.
- [ ] 1.3 Trocar o `screen : Maybe GameStatus` de `acceptsGameMsg` por `Maybe (GameStatus -> Bool)`, conforme a tabela da Decisão 2. Verificar que compila e que Começar rodada, Acertou, Pular, Continuar rodada e Continuar funcionam como antes.
- [ ] 1.4 Implementar `AskEnd`, `CancelEnd`, `ConfirmEnd` e `NewGame` em `updateGame` conforme a Decisão 3, todas com `lockTaps`. `ConfirmEnd` soma os pontos só com origem `GameOver`. `NewGame` volta a `Preparing` com `gameModel = initGame` e todos os jogadores com `score = 0`, mantendo `preparingModel`. Verificar que compila.

## 2. Telas

- [ ] 2.1 Criar `viewEndButton` (`btn btn--ghost btn--block`, "Encerrar partida", `GameMsg AskEnd`) e acrescentá-lo ao rodapé de `viewGameShowingPlayers`, `viewPaused` e `viewGameOver`, depois da ação principal. Verificar no navegador que o botão aparece nas três telas e não aparece na rodada em andamento (cenários "Anúncio da dupla" e "Rodada em andamento").
- [ ] 2.2 Adicionar `.btn--danger` em `src/index.css`, seguindo o padrão de `.btn--success` com `var(--danger)`, mais o estilo do texto do cartão de confirmação. Verificar o contraste do texto nos temas claro e escuro.
- [ ] 2.3 Criar `viewConfirmEnd` (`key = "confirm-end"`) conforme a Decisão 5, com o texto de apoio por origem e os botões Encerrar e Voltar ao jogo. Verificar no navegador os cenários "Abrir a confirmação", "Voltar ao jogo do anúncio", "Voltar ao jogo da pausa", "Voltar ao jogo do fim da rodada" e "Sem diálogo do navegador".
- [ ] 2.4 Criar o helper `leaders` (Decisão 5), usá-lo em `viewScoreboard` para o destaque e o troféu e criar `viewFinished` (`key = "final"`) com "Fim de jogo", o subtítulo do resultado, `viewScoreboard` e o botão Nova partida. Verificar no navegador os cenários de "Placar final" ("Encerrar no fim da rodada", "Encerrar no anúncio", "Encerrar com a rodada pausada", "Ninguém pontuou") e o cenário "Placar final" do requisito "Placar".

## 3. Verificação integrada

- [ ] 3.1 Rodar `yarn start`, jogar uma partida com Ana, Beto e Caio em rodadas de 2min, encerrar e tocar em Nova partida. Verificar os cenários de "Voltar à preparação depois da partida": mesmos jogadores na mesma ordem, 2min selecionado, placar zerado no primeiro anúncio e primeira dupla Ana → Beto, inclusive depois de remover Caio e adicionar Davi.
- [ ] 3.2 Verificar a proteção de toques nas trocas novas: toque duplo em Encerrar partida, em Encerrar e em Nova partida (cenários de "Proteção contra toques na troca de tela"). Verificar também que os botões do rodapé das telas novas não aparecem com o estilo de desabilitado durante a trava.
- [ ] 3.3 No placar final, trocar de aba e voltar e verificar o cenário "Sem pausa no placar final". Numa tela de 320 × 568 do modo dispositivo do DevTools, conferir que anúncio, pausa, fim da rodada, confirmação e placar final cabem sem rolagem horizontal e que o rodapé fica acima da safe area.
- [ ] 3.4 Rodar `yarn build` e verificar que o build termina sem erros.
