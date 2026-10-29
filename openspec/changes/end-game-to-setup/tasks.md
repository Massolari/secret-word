# Tasks

## 1. Estados, mensagens e pontuação

- [x] 1.1 Adicionar `GameConfirmingEnd GameStatus` e `GameFinished` a `GameStatus`, e `AskEnd`, `CancelEnd`, `ConfirmEnd` e `NewGame` a `GameMsg` (Decisões 1 e 2). Ligar as variantes novas em `viewGame` com views provisórias. Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [x] 1.2 Extrair de `Restart` o helper `addRoundPoints : GameModel -> Dict Int Player -> Dict Int Player` (Decisão 4) e usar o helper em `Restart`. Verificar que compila e que, no navegador, uma rodada de 5 acertos de Ana para Beto ainda dá +5 para Beto e +2 para Ana depois de Continuar.
- [x] 1.3 Trocar o `screen : Maybe GameStatus` de `acceptsGameMsg` por `Maybe (GameStatus -> Bool)`, conforme a tabela da Decisão 2. Verificar que compila e que Começar rodada, Acertou, Pular, Continuar rodada e Continuar funcionam como antes.
- [x] 1.4 Implementar `AskEnd`, `CancelEnd`, `ConfirmEnd` e `NewGame` em `updateGame` conforme a Decisão 3, todas com `lockTaps`. Em `GamePlaying`, `AskEnd` reaproveita `pauseRound` e abre a confirmação com origem `GamePaused`. `CancelEnd` só restaura a origem, sem ramo especial. `ConfirmEnd` soma os pontos só com origem `GameOver`. `NewGame` volta a `Preparing` com `gameModel = initGame` e todos os jogadores com `score = 0`, mantendo `preparingModel`. Verificar que compila.

## 2. Telas

- [x] 2.1 Criar `viewEndButton` (`btn btn--ghost btn--block`, "Encerrar partida", `GameMsg AskEnd`) e acrescentá-lo ao rodapé de `viewGameShowingPlayers`, `viewPaused` e `viewGameOver`, depois da ação principal. Verificar no navegador que o botão aparece nas três telas (cenário "Anúncio da dupla").
- [x] 2.2 Em `viewPlaying`, montar o header com `viewTimer` e um `btn btn--ghost btn--icon` com `Icons.close`, `aria-label "Encerrar partida"` e `GameMsg AskEnd` (Decisão 5), deixando `viewPaused` como está. Verificar no navegador os cenários "Rodada em andamento" e "Leitor de tela na rodada".
- [x] 2.3 Adicionar `.btn--danger` em `src/index.css`, seguindo o padrão de `.btn--success` com `var(--danger)`, mais o estilo do texto do cartão de confirmação e a linha do header da rodada (Decisão 6). Verificar o contraste do texto nos temas claro e escuro.
- [x] 2.4 Criar `viewConfirmEnd` (`key = "confirm-end"`) conforme a Decisão 5, com o texto de apoio por origem e os botões Encerrar e Voltar ao jogo. Verificar no navegador os cenários "Abrir a confirmação", "Voltar ao jogo do anúncio", "Voltar ao jogo da pausa", "Voltar ao jogo do fim da rodada" e "Sem diálogo do navegador".
- [x] 2.5 Verificar no navegador os cenários "Encerrar durante a rodada" e "Voltar ao jogo durante a rodada": abrir a confirmação pelo ícone com cerca de 00:40, esperar 1 minuto, tocar em Voltar ao jogo e conferir que aparece a rodada pausada com o mesmo tempo. Depois, tocar em Continuar rodada e conferir que voltam a mesma palavra e o mesmo contador, com a contagem a partir do tempo parado.
- [x] 2.6 Criar o helper `leaders` (Decisão 5), usá-lo em `viewScoreboard` para o destaque e o troféu e criar `viewFinished` (`key = "final"`) com "Fim de jogo", o subtítulo do resultado, `viewScoreboard` e o botão Nova partida. Verificar no navegador os cenários de "Placar final" ("Encerrar no fim da rodada", "Encerrar no anúncio", "Encerrar com a rodada pausada", "Encerrar durante a rodada", "Ninguém pontuou") e o cenário "Placar final" do requisito "Placar".

## 3. Verificação integrada

- [x] 3.1 Rodar `yarn start`, jogar uma partida com Ana, Beto e Caio em rodadas de 2min, encerrar e tocar em Nova partida. Verificar os cenários de "Voltar à preparação depois da partida": mesmos jogadores na mesma ordem, 2min selecionado, placar zerado no primeiro anúncio e primeira dupla Ana → Beto, inclusive depois de remover Caio e adicionar Davi.
- [x] 3.2 Verificar a proteção de toques nas trocas novas: toque duplo em Encerrar partida, em Voltar ao jogo durante a rodada, em Encerrar e em Nova partida, e o ícone de encerrar tocado logo depois de Começar rodada (cenários de "Proteção contra toques na troca de tela"). Verificar também que os botões do rodapé das telas novas não aparecem com o estilo de desabilitado durante a trava.
- [ ] 3.3 No placar final, trocar de aba e voltar e verificar o cenário "Sem pausa no placar final". Numa tela de 320 × 568 do modo dispositivo do DevTools, conferir que anúncio, rodada em andamento (com o ícone no cabeçalho e uma palavra longa), pausa, fim da rodada, confirmação e placar final cabem sem rolagem horizontal e que o rodapé fica acima da safe area.
- [x] 3.4 Rodar `yarn build` e verificar que o build termina sem erros.
