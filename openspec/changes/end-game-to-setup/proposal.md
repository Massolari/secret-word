# Proposal

## Why

Depois de tocar em Começar, a partida não tem fim: o ciclo anúncio → rodada → fim da rodada → anúncio se repete para sempre. Para encerrar, trocar os jogadores ou mudar o tempo da rodada, o grupo precisa recarregar a página, perdendo o placar sem ver quem ganhou. No meio de uma rodada, o único jeito de "acabar logo" é tocar em Pular até o tempo acabar.

## What Changes

- Novo botão **Encerrar partida** em todas as telas da partida:
  - No anúncio da dupla, na rodada pausada e no fim da rodada, ele fica no rodapé, abaixo da ação principal e com menos destaque que ela.
  - Na rodada em andamento, ele é um ícone (✕) no cabeçalho, ao lado do cronômetro, longe de Pular e Acertou.
- Tocar em Encerrar partida abre uma **confirmação** dentro do app: "Encerrar a partida?", com as opções **Encerrar** e **Voltar ao jogo**. Voltar ao jogo leva à mesma tela de antes, sem mudar nada. Vinda da rodada pausada, a rodada continua pausada.
- Durante a rodada em andamento, tocar em Encerrar partida **pausa a rodada** (a mesma pausa de quando o jogo sai da tela) e abre a confirmação. O cronômetro para e o tempo com a confirmação aberta não é descontado. Voltar ao jogo leva à tela de rodada pausada, e Continuar rodada retoma com a mesma palavra, o mesmo contador e o tempo que restava.
- Ao confirmar, aparece a nova tela de **placar final**: título de fim de jogo, quem venceu (ou empate) e o placar completo, com as mesmas regras de ordenação, posição e troféu do placar atual.
  - Encerrando no fim da rodada, os pontos dessa rodada entram no placar final, como se o grupo tivesse tocado em Continuar.
  - Encerrando no anúncio, o placar final é o placar atual.
  - Encerrando durante a rodada ou com ela pausada, a rodada interrompida não dá pontos.
- No placar final, o botão **Nova partida** volta à tela de preparação com os mesmos jogadores, na mesma ordem, e o mesmo tempo de rodada já escolhido. Lá o grupo pode adicionar e remover jogadores e trocar o tempo antes de começar de novo.
- A nova partida começa do zero: pontuações zeradas, ordem das duplas a partir da primeira e sorteio de palavras sem memória da partida anterior.
- A proteção contra toques na troca de tela passa a valer também para as novas telas (confirmação e placar final) e para a volta à preparação.

Fora do escopo: proteger a partida contra recarregar a página ou contra o voltar do navegador (change `protect-game-from-reload`) e qualquer persistência do placar.

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

- `game-round`: novos requisitos "Encerrar partida", "Confirmação para encerrar" e "Placar final". Os requisitos "Placar" (passa a aparecer também no placar final) e "Proteção contra toques na troca de tela" (novas trocas de tela, incluindo rodada → confirmação → pausa, e o botão Encerrar partida do cabeçalho da rodada) mudam.
- `game-setup`: novo requisito "Voltar à preparação depois da partida". A preparação mantém jogadores e tempo, e a partida seguinte começa do zero.

## Impact

- `src/Main.elm`: novos estados de tela no fluxo da partida (confirmação de encerramento e placar final) e novas mensagens para pedir, cancelar e confirmar o encerramento e para voltar à preparação. A soma dos pontos da rodada sai de `Restart` para um helper reaproveitado ao encerrar no fim da rodada. A volta à preparação zera `gameModel` e as pontuações dos jogadores. `acceptsGameMsg` ganha as novas mensagens. Há novas views para a confirmação e o placar final. O botão Encerrar partida entra nas views de anúncio, pausa e fim da rodada, e como ícone no header de `viewPlaying`, ao lado de `viewTimer`. Abrir a confirmação na rodada reaproveita a pausa existente (`pauseRound`), e a volta passa pela tela de pausa e pelo `Resume`.
- `src/index.css`: estilo da confirmação, do placar final e do cabeçalho da rodada com o ícone, reaproveitando `.card`, `.btn--ghost`, `.btn--icon` e `.scoreboard`. Possivelmente uma variante de botão de perigo com `--danger`, que já existe nos dois temas.
- `src/index.js`: sem mudança.
- Sem dependências novas.
