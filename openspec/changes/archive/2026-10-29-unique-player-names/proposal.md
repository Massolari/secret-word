# Proposal

## Why

Hoje dá para cadastrar duas "Ana" (ou "Ana" e "ana "), e na partida só a cor do avatar diferencia uma da outra. Isso confunde o anúncio das duplas, o placar e o histórico, que mostram só o nome (item `unique-player-names` do roadmap).

## What Changes

- A tela de preparação passa a impedir o cadastro de um jogador cujo nome já existe na lista. A comparação ignora maiúsculas/minúsculas, espaços nas pontas e espaços repetidos no meio ("Ana", "ana" e " ANA " são o mesmo nome; "Ana  Maria" e "ana maria" também).
- Quando o nome digitado repete um jogador da lista, aparece abaixo do campo uma mensagem curta (por exemplo, "Já tem um jogador chamado Ana."), o campo fica marcado como inválido e o botão de adicionar fica desabilitado. Confirmar pelo teclado não adiciona ninguém e o texto digitado continua no campo, para o jogador corrigir.
- A mensagem some assim que o nome deixa de repetir, seja porque o jogador mudou o texto, seja porque removeu o jogador de mesmo nome.
- Um jogador que está saindo da lista (animação de remoção em andamento) já não conta como repetido.
- A comparação não ignora acentos: "Ana" e "Aná" continuam sendo nomes diferentes. O nome salvo continua sendo o texto digitado sem os espaços das pontas, como hoje.

## Capabilities

### New Capabilities

_Nenhuma._

### Modified Capabilities

- `game-setup`: novo requisito "Nome de jogador repetido", com a regra de comparação e o aviso descritos acima. O requisito "Adicionar jogador" continua como está.

## Impact

- `src/Main.elm`:
  - nova função de normalização do nome e checagem contra `activePlayers`;
  - `AddUser` ignora o envio quando o nome repete;
  - `viewPreparePlayers` mostra a mensagem, marca o campo com `aria-invalid` e liga a mensagem ao campo por `aria-describedby`, e desabilita o botão de adicionar.
- `src/index.css`: estilo da mensagem de erro e do campo inválido, usando o token `--danger` que já existe nos dois temas.
- `roadmap-changes.md`: o item 10 fica resolvido por esta change.
- Sem novas dependências.
