# Proposal

## Why

A partida só existe na memória da página: jogadores, placar, ordem das duplas e palavras já sorteadas. Recarregar a página, fechar a aba ou usar o voltar do navegador (o gesto de voltar no celular é fácil de fazer sem querer no meio da rodada) apaga tudo sem nenhum aviso. Como o app não tem rotas, a página tem uma só entrada no histórico, e o voltar sai do jogo direto.

## What Changes

- Do começo da partida (toque em Começar na preparação) em diante, recarregar, fechar a aba ou navegar para outro endereço passa a pedir a confirmação nativa do navegador, quando ele oferece suporte.
- Durante a partida, o voltar do navegador (botão ou gesto) não sai mais da página direto. O jogo mostra um diálogo próprio, "Sair da partida?", avisando que o placar será perdido, com as opções **Continuar partida** e **Sair**.
  - Se a rodada estiver em andamento, ela pausa antes do diálogo aparecer, e o tempo não corre enquanto ele está aberto.
  - **Continuar partida** fecha o diálogo e mantém a tela em que o jogo estava. Uma rodada pausada continua pausada até o toque em Continuar rodada.
  - **Sair** desliga a proteção e conclui o voltar, saindo da página.
  - Um novo voltar com o diálogo aberto fecha o diálogo, como Continuar partida.
- Na tela de preparação nada muda: recarregar e voltar seguem o comportamento normal do navegador.
- A decisão pendente do roadmap ficou em **só confirmar saída**. Salvar a partida e retomar depois fica fora desta change.

## Capabilities

### New Capabilities

- `game-session`: a partida como um todo, do toque em Começar até ela deixar de existir. Nesta change cobre a proteção contra saída acidental (recarregar, fechar, voltar). É onde a change `end-game-to-setup` poderá descrever o encerramento da partida.

### Modified Capabilities

(nenhuma)

## Impact

- `src/Main.elm`: novos ports `protectGame : Bool -> Cmd msg` e `leaveGame : () -> Cmd msg` (saída) e `backPressed : (() -> msg) -> Sub msg` (entrada); um campo no `Model` para o diálogo de saída aberto; mensagens para o voltar, Continuar partida e Sair; `Start` liga a proteção; `subscriptions` passa a ouvir `backPressed` durante a partida; ações da partida ficam bloqueadas com o diálogo aberto; nova view do diálogo sobre a tela atual.
- `src/index.js`: listener de `beforeunload` ligado só enquanto a proteção está ativa; entrada extra no histórico (`history.pushState`) e listener de `popstate` para interceptar o voltar e avisar o Elm; saída com `history.go(-2)`.
- `src/index.css`: estilo do diálogo e do fundo escurecido, nos temas claro e escuro e respeitando movimento reduzido.
- Sem dependências novas. Sem mudança em pontuação, cronômetro ou sorteio.
- Limitação conhecida: o iOS Safari praticamente ignora `beforeunload`, então lá recarregar não pede confirmação. O voltar interceptado funciona em todos os navegadores com `history.pushState`.
