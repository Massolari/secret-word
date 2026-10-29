# Tasks

## 1. Estados, mensagens e pontuação

- [ ] 1.1 Adicionar `GameConfirmingEnd GameStatus` e `GameFinished` a `GameStatus`, e `AskEnd`, `CancelEnd`, `ConfirmEnd` e `NewGame` a `GameMsg` (Decisões 1 e 2). Ligar as variantes novas em `viewGame` com views provisórias. Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [ ] 1.2 Extrair de `Restart` o helper `addRoundPoints : GameModel -> Dict Int Player -> Dict Int Player` (Decisão 4) e usar o helper em `Restart`. Verificar que compila e que, no navegador, uma rodada de 5 acertos de Ana para Beto ainda dá +5 para Beto e +2 para Ana depois de Continuar.
- [ ] 1.3 Trocar o `screen : Maybe GameStatus` de `acceptsGameMsg` por `Maybe (GameStatus -> Bool)`, conforme a tabela da Decisão 2. Verificar que compila e que Começar rodada, Acertou, Pular, Continuar rodada e Continuar funcionam como antes.
- [ ] 1.4 Implementar `AskEnd`, `CancelEnd`, `ConfirmEnd` e `NewGame` em `updateGame` conforme a Decisão 3, todas com `lockTaps`. Com origem `GamePlaying`, `AskEnd` grava `lastTick = Nothing` e envia `keepAwake False`, e `CancelEnd` faz o mesmo que `Resume` (`lastTick = Nothing`, `tickNow`, `keepAwake True`). `ConfirmEnd` soma os pontos só com origem `GameOver`. `NewGame` volta a `Preparing` com `gameModel = initGame` e todos os jogadores com `score = 0`, mantendo `preparingModel`. Verificar que compila.

## 2. Telas

- [ ] 2.1 Criar `viewEndButton` (`btn btn--ghost btn--block`, "Encerrar partida", `GameMsg AskEnd`) e acrescentá-lo ao rodapé de `viewGameShowingPlayers`, `viewPaused` e `viewGameOver`, depois da ação principal. Verificar no navegador que o botão aparece nas três telas (cenário "Anúncio da dupla").
- [ ] 2.2 Em `viewPlaying`, montar o header com `viewTimer` e um `btn btn--ghost btn--icon` com `Icons.close`, `aria-label "Encerrar partida"` e `GameMsg AskEnd` (Decisão 5), deixando `viewPaused` como está. Verificar no navegador os cenários "Rodada em andamento" e "Leitor de tela na rodada".
- [ ] 2.3 Adicionar `.btn--danger` em `src/index.css`, seguindo o padrão de `.btn--success` com `var(--danger)`, mais o estilo do texto do cartão de confirmação e a linha do header da rodada (Decisão 6). Verificar o contraste do texto nos temas claro e escuro.
- [ ] 2.4 Criar `viewConfirmEnd` (`key = "confirm-end"`) conforme a Decisão 5, com o texto de apoio por origem e os botões Encerrar e Voltar ao jogo. Verificar no navegador os cenários "Abrir a confirmação", "Voltar ao jogo do anúncio", "Voltar ao jogo da pausa", "Voltar ao jogo do fim da rodada" e "Sem diálogo do navegador".
- [ ] 2.5 Verificar no navegador os cenários de "Rodada parada durante a confirmação": abrir a confirmação com cerca de 00:40, esperar 1 minuto e conferir que o tempo não andou, depois tocar em Voltar ao jogo e conferir que a rodada volta na hora, com a mesma palavra, o mesmo contador e a contagem a partir do tempo parado.
- [ ] 2.6 Criar o helper `leaders` (Decisão 5), usá-lo em `viewScoreboard` para o destaque e o troféu e criar `viewFinished` (`key = "final"`) com "Fim de jogo", o subtítulo do resultado, `viewScoreboard` e o botão Nova partida. Verificar no navegador os cenários de "Placar final" ("Encerrar no fim da rodada", "Encerrar no anúncio", "Encerrar com a rodada pausada", "Encerrar durante a rodada", "Ninguém pontuou") e o cenário "Placar final" do requisito "Placar".

## 3. Verificação integrada

- [ ] 3.1 Rodar `yarn start`, jogar uma partida com Ana, Beto e Caio em rodadas de 2min, encerrar e tocar em Nova partida. Verificar os cenários de "Voltar à preparação depois da partida": mesmos jogadores na mesma ordem, 2min selecionado, placar zerado no primeiro anúncio e primeira dupla Ana → Beto, inclusive depois de remover Caio e adicionar Davi.
- [ ] 3.2 Verificar a proteção de toques nas trocas novas: toque duplo em Encerrar partida, em Voltar ao jogo durante a rodada, em Encerrar e em Nova partida, e o ícone de encerrar tocado logo depois de Começar rodada (cenários de "Proteção contra toques na troca de tela"). Verificar também que os botões do rodapé das telas novas não aparecem com o estilo de desabilitado durante a trava.
- [ ] 3.3 No placar final, trocar de aba e voltar e verificar o cenário "Sem pausa no placar final". Numa tela de 320 × 568 do modo dispositivo do DevTools, conferir que anúncio, rodada em andamento (com o ícone no cabeçalho e uma palavra longa), pausa, fim da rodada, confirmação e placar final cabem sem rolagem horizontal e que o rodapé fica acima da safe area.
- [ ] 3.4 Rodar `yarn build` e verificar que o build termina sem erros.
