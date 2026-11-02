# Tasks

## 1. Estado e navegação no Elm

- [ ] 1.1 Em `src/Main.elm`, adicionar `Instructions` a `Status`, `OpenInstructions` e `CloseInstructions` a `Msg`, `InstructionsEntry Bool` e `ScrollToTop` a `Effect`, e o port `instructionsEntry : Bool -> Cmd msg`. Tratar os efeitos em `perform` (`ScrollToTop` com `Browser.Dom.setViewport 0 0`) e em `simulateWith` de `tests/Harness.elm` (porta `"instructionsEntry"` e `Cmd.none`). Dar a `view` um ramo temporário. Verificar com `npx elm make src/Main.elm --output=/dev/null`.
- [ ] 1.2 Implementar `OpenInstructions`, `CloseInstructions` e o ramo de `Instructions` em `BackPressed` conforme a tabela da Decisão 2, e assinar `backPressed` em `subscriptions` também em `Instructions`. Verificar que compila e que os testes existentes de `BackPressed` continuam passando com `yarn test`.
- [ ] 1.3 Criar `tests/InstructionsTest.elm`, com o cabeçalho de cenários manuais no estilo de `SetupTest.elm`, cobrindo em `update`: os efeitos de cada linha da tabela da Decisão 2; `OpenInstructions` e `CloseInstructions` ignorados com `tapGuard` travado; `BackPressed` aceito com `tapGuard` travado; `BackPressed` na preparação continua sem efeito; `Start` ignorado em `Instructions`. Verificar com `yarn test`.

## 2. Telas

- [ ] 2.1 Adicionar `Icons.help` em `src/Icons.elm` e o botão "Como jogar" no `header` de `viewPreparing` (Decisão 4). Testar "Abrir as instruções sem jogadores" e "Partida em andamento" (nenhuma tela da partida tem o botão). Verificar com `yarn test`.
- [ ] 2.2 Criar `viewInstructions` com o título, as seções e o texto da Decisão 5 e o botão Voltar no rodapé, substituindo o ramo temporário de `view`. Testar os cenários "Preparação e duplas", "Rodada", "Pontuação e fim da partida", "Voltar pelo botão", "Voltar pelo navegador", "Voltar com a preparação preenchida", "Começar depois de voltar", "Toque duplo em Voltar", "Toque depois do bloqueio", "Voltar do navegador logo depois de abrir" e "Leitor de tela" (`h1` e um `h2` por seção). Verificar com `yarn test`.
- [ ] 2.3 Adicionar em `src/index.css` o espaçamento do botão no cabeçalho e `.instructions__text`, usando os tokens existentes. Verificar no navegador, nos temas claro e escuro e a 320px, os cenários "Celular pequeno" e "Instruções mais compridas que a tela", e que o botão tem pelo menos 48px de altura.

## 3. Histórico do navegador

- [ ] 3.1 Em `src/index.js`, implementar `instructionsOpen`, a limpeza da entrada ao carregar, o port `instructionsEntry` e os novos ramos do `popstate` conforme a Decisão 3, no estilo `app.ports?.x?.subscribe`. Verificar com `yarn build` que o build termina sem erros.
- [ ] 3.2 Com `yarn start`, abrir o jogo a partir de outra página (ex.: `about:blank` e depois a URL do jogo) e verificar no navegador os cenários "Voltar do navegador depois do botão", "Voltar duas vezes", "Avançar depois de voltar", "Partida depois de visitar as instruções" (pelo botão, pelo voltar e pelo voltar seguido de avançar) e "Recarregar nas instruções". Registrar os cenários manuais no cabeçalho de `tests/InstructionsTest.elm`.

## 4. Documentação

- [ ] 4.1 Adicionar `game-instructions` à tabela de specs do `README.md`. Verificar que a linha aparece na tabela e que `openspec validate add-instructions-screen --strict` passa.

## 5. Verificação integrada

- [ ] 5.1 Com jogadores cadastrados e um nome digitado no campo, abrir e fechar as instruções pelos dois caminhos, começar a partida, usar o voltar e confirmar que o diálogo "Sair da partida?", Continuar partida e Sair funcionam como especificado em `game-session`. Verificar também "Voltar na preparação" e "Recarregar na preparação", e rodar `yarn test` e `yarn build` sem erros.
