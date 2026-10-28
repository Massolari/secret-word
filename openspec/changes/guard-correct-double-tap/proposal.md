# Proposal

## Why

Hoje cada toque em Acertou soma um ponto e sorteia outra palavra na hora. Um toque duplo acidental dá +2 na rodada e marca como acerto uma palavra que apareceu e sumiu antes de alguém vê-la. Os dois toques entram no placar e no histórico, e o jogador não tem como desfazer. A proteção que já existe só vale nas trocas de tela, não entre uma palavra e outra.

## What Changes

- Logo depois que uma nova palavra aparece na rodada, o botão Acertou ignora toques e teclas por um curto período (500 ms). Depois disso volta a responder normalmente.
- O bloqueio começa no próprio toque que pede a nova palavra, em Acertou ou Pular, e termina 500 ms depois que a nova palavra aparece. Assim, um segundo toque que chegue antes de a palavra trocar também não conta.
- Um toque ignorado não tem efeito nenhum: não soma ponto, não entra no histórico, não sorteia palavra e não vibra.
- A aparência do Acertou não muda durante o bloqueio, igual à proteção na troca de tela.
- O Pular não ganha bloqueio: pulos seguidos continuam valendo, como o requisito "Pular custa tempo" já define.

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

- `game-round`: o requisito "Acertar" passa a ignorar toques em Acertou nos primeiros 500 ms depois de uma nova palavra aparecer, com novos cenários para o toque duplo, para Acertou logo depois de Pular e para o toque liberado. Os demais requisitos não mudam.

## Impact

- `src/Main.elm`: um bloqueio novo, só para o Acertou, separado do `tapGuard` da troca de tela. Ele é armado em `Correct` e `Jump`, renovado em `NewWordIndex` quando a palavra aparece e liberado por uma mensagem com token. `Correct` passa a checar esse bloqueio antes de agir.
- Sem mudanças em CSS, `index.js` ou dependências.
- Fora do escopo: limitar pulos acidentais seguidos no Pular, que o spec atual permite de propósito.
