# Tasks

## 1. Ponte com o navegador

- [x] 1.1 Declarar em `src/Main.elm` os ports `protectGame : Bool -> Cmd msg`, `leaveGame : () -> Cmd msg` e `backPressed : (() -> msg) -> Sub msg` (Decisão 1), junto dos ports existentes. Verificar com `npx elm make src/Main.elm --output=/dev/null` (um port só entra no JS compilado se for usado, então pode ser preciso ligar 2.x antes de testar o JS).
- [x] 1.2 Em `src/index.js`, implementar `guarded`, o handler de `beforeunload`, `protectGame`, `leaveGame` e o listener de `popstate` conforme as Decisões 2 e 3, no estilo `app.ports?.x?.subscribe` já usado no arquivo. Verificar com `yarn build` que o build termina sem erros.

## 2. Estado e lógica da proteção

- [x] 2.1 Adicionar `exitPrompt : Bool` ao `Model` (`False` em `init`) e as mensagens `BackPressed`, `StayInGame` e `LeaveGame` ao `Msg`, com o comportamento da Decisão 4 (pausa via `pauseRound` se `GamePlaying`, foco em Continuar partida, `lockTaps` ao fechar, `leaveGame ()` ao sair). Verificar que compila.
- [x] 2.2 Fazer `Start` enviar `protectGame True` junto com o `Cmd` atual. Verificar que compila.
- [x] 2.3 Incluir `not model.exitPrompt` em `acceptsGameMsg` para as ações de tela. Verificar que compila e que `Tick`, `NewWordIndex` e `VisibilityChanged` continuam liberados.
- [x] 2.4 Reescrever `subscriptions` para assinar `backPressed (\_ -> BackPressed)` sempre que `model.status == Playing`, em `Sub.batch` com o tick e a visibilidade que já existem em `GamePlaying`. Verificar que compila e, no navegador, que a rodada continua contando e pausando ao trocar de aba como antes.

## 3. Diálogo de saída

- [x] 3.1 Criar `viewExitPrompt` (Decisão 5): fundo, `role="dialog"`, `aria-modal`, `aria-labelledby`, título "Sair da partida?", texto "O placar e a vez de cada dupla serão perdidos.", botões Sair (`btn--ghost`) e Continuar partida (`btn--primary`, embaixo, com o id usado no foco) e Esc → `StayInGame`. Acrescentar ao `Keyed.node` de `view` com a chave `"exit-prompt"` quando `exitPrompt`. Verificar que compila.
- [x] 3.2 Adicionar em `src/index.css` `.dialog-backdrop` e `.dialog` usando os tokens, `fade-in` na entrada e a área segura embaixo. Verificar no navegador, nos temas claro e escuro e com movimento reduzido ligado no DevTools, que o diálogo cobre a tela, só aparece com fade (sem deslizar) com movimento reduzido e que tocar no fundo não faz nada.

## 4. Verificação integrada

- [x] 4.1 Rodar `yarn start`, cadastrar dois jogadores e, ainda na preparação, recarregar e usar o voltar. Verificar os cenários "Recarregar na preparação" e "Voltar na preparação": nada é perguntado.
- [ ] 4.2 Começar a partida e recarregar no anúncio da dupla, cancelar, recarregar de novo e confirmar. Verificar os cenários "Recarregar durante a partida", "Desistir de recarregar" e "Confirmar o recarregamento".
- [x] 4.3 Abrir o jogo a partir de outra página (ex.: `about:blank` e depois a URL do jogo), começar a partida e usar o voltar no anúncio, numa rodada com cerca de 00:30, e no fim da rodada. Verificar os cenários "Voltar no anúncio da dupla", "Voltar no meio da rodada", "Diálogo aberto por um tempo" (esperar 1 min) e "Voltar no fim da rodada".
- [x] 4.4 Com o diálogo aberto, usar o voltar de novo, apertar Esc, tocar em Continuar partida e tocar no fundo onde fica o botão do rodapé. Verificar os cenários "Voltar com o diálogo aberto", "Teclado", "Continuar depois de voltar na rodada", "Proteção continua ativa" e "Toque fora do diálogo".
- [x] 4.5 Tocar em Sair e verificar o cenário "Sair pelo diálogo": o navegador volta para a página anterior sem outra pergunta.
- [ ] 4.6 Num celular Android (Chrome), repetir 4.3 a 4.5 com o gesto de voltar e conferir que a guarda não é pulada. Num iPhone (Safari), conferir o gesto de voltar e registrar no resumo da change que recarregar não pede confirmação lá (limitação aceita no design).
- [x] 4.7 Rodar `yarn build` e verificar que o build termina sem erros.
