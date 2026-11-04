# Proposal

## Why

As regras do jogo só estão escritas no README, que quem joga no celular nunca vê. Um grupo novo precisa que alguém explique como funcionam as duplas, o custo de pular e a pontuação, e o app não oferece nenhum lugar para consultar isso.

## What Changes

- A tela de preparação (a página inicial) ganha um botão **Como jogar**, no cabeçalho.
- Nova tela de instruções, em texto, com o objetivo e os passos da partida: preparação, duplas, rodada (Acertou e Pular, que custa 5 segundos), pontuação e como a partida termina.
- O botão **Voltar**, no rodapé da tela de instruções, retorna à preparação. O voltar do navegador (botão ou gesto) faz o mesmo em vez de sair da página.
- Ir às instruções e voltar não altera nada da preparação: jogadores, tempo escolhido e nome digitado no campo continuam como estavam.
- As instruções só abrem pela preparação. Consultá-las durante a partida fica fora desta change.

## Capabilities

### New Capabilities

- `game-instructions`: como o jogador chega às instruções do jogo, o que elas explicam e como ele volta sem perder a preparação.

### Modified Capabilities

(nenhuma)

## Impact

- `src/Main.elm`: novo estado de tela, mensagens para abrir e fechar as instruções, novos efeitos, novo port, view da tela e botão na preparação. O tratamento de `BackPressed` e `subscriptions` passam a cobrir as instruções.
- `src/index.js`: entrada extra no histórico enquanto as instruções estão abertas e tratamento dela no `popstate`, sem alterar a proteção da partida.
- `src/index.css`: estilos do botão no cabeçalho e do conteúdo das instruções, nos dois temas.
- `src/Icons.elm`: novo ícone de ajuda.
- `tests/`: `Harness.elm` simula os novos efeitos, e os cenários da capability ganham testes.
- `README.md`: a tabela de specs ganha `game-instructions`.
- Sem dependências novas. Sem mudança em pontuação, cronômetro ou sorteio.
