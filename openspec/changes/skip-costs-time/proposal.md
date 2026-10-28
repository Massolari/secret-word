# Proposal

## Why

Hoje o botão Pular fica bloqueado nos primeiros 5 segundos de cada palavra. Esse bloqueio é contado pelos ticks do cronômetro da rodada, por isso na prática dura entre 4 e 5 segundos (item `skip-lock-per-word` do roadmap). Em vez de acertar a contagem, vamos trocar a regra: pular passa a custar tempo, não a exigir espera. A pressão continua, mas o jogador escolhe quando pular. Assim o bug some junto com o bloqueio.

## What Changes

- **BREAKING (regra do jogo)**: o botão Pular deixa de ter bloqueio por palavra. Ele fica disponível desde que a palavra aparece.
- Tocar em Pular tira 5 segundos do tempo restante da rodada, na hora. Além disso, sorteia uma nova palavra e marca a palavra pulada como erro no histórico, como já acontece hoje.
- O botão Pular fica desabilitado quando restam 5 segundos ou menos, então um pulo nunca termina a rodada sozinho. Quando o tempo restante cai para esse limite, o botão é desabilitado sem passar por nenhum outro estado.
- O botão mostra o custo ("Pular −5s") e anuncia para leitores de tela que pular custa 5 segundos. Saem o anel de contagem regressiva (5→1) e o texto "Pular disponível em N segundos".
- O cronômetro e a barra de progresso mostram o novo tempo logo após o pulo. Se o tempo cair para 10 segundos ou menos, o alerta de últimos segundos aparece normalmente.

## Capabilities

### New Capabilities

_Nenhuma._

### Modified Capabilities

- `game-round`: o requisito "Pular com bloqueio" é substituído por um requisito em que pular custa 5 segundos do cronômetro e fica desabilitado quando não sobra tempo suficiente.

## Impact

- `src/Main.elm`:
  - sai o campo `toShowJump` do `GameModel`, junto com a contagem em `Tick` e o reset em `NewWordIndex`;
  - `Jump` passa a descontar 5000 ms de `currentTime` e a ignorar o toque quando não há tempo suficiente;
  - `viewSkipButton` passa a depender do tempo restante.
- `src/index.css`: saem as regras `.skip-ring` e `.skip-ring__num` (incluindo a do media query), que não serão mais usadas.
- `roadmap-changes.md`: o item 3 (`skip-lock-per-word`) fica resolvido por esta change. A change 4 (`timer-elapsed-time`) precisa manter o desconto de 5 s ao trocar a contagem de ticks por tempo real.
- Sem novas dependências.
