# Tasks

## 1. Regra de nome repetido

- [ ] 1.1 Criar em `src/Main.elm` `normalizeName : String -> String` (`String.words >> String.join " " >> String.toLower`) e `duplicatePlayer : Model -> Maybe Player`, que busca em `activePlayers` (Decisão 2 do design). Verificar que `npx elm make src/Main.elm --output=/dev/null` compila sem erros.
- [ ] 1.2 Em `AddUser`, só inserir quando o nome sem espaços nas pontas não é vazio e `duplicatePlayer model` é `Nothing`. Caso contrário, devolver `( model, Cmd.none )` sem limpar o campo (Decisão 3). Verificar com `elm make` e conferir no código que `InputPlayer`, `DeletePlayer` e `RemovePlayer` não mudaram.

## 2. Aviso e botão

- [ ] 2.1 Em `viewPreparePlayers`, envolver o `form.add-player` num `div.add-player-group` com o `p#player-name-error.field-error` (`aria-live "polite"`, sempre presente e vazio sem repetido). Quando há repetido, o `p` mostra "Já tem um jogador chamado <nome cadastrado>.", o input recebe `aria-invalid "true"` e `aria-describedby "player-name-error"`, e o botão de adicionar fica desabilitado (Decisão 4). Verificar que `elm make` compila sem erros.
- [ ] 2.2 Em `src/index.css`, adicionar `.add-player-group`, `.field-error`, `.field-error:not(:empty)` e a borda `--danger` para `.field[aria-invalid="true"]`, incluindo o `:focus` (Decisão 5). Verificar no navegador, nos temas claro e escuro, que o aviso é legível, que a borda fica vermelha mesmo com o campo focado e que, sem aviso, o espaço entre o campo e a lista é o mesmo de antes.

## 3. Verificação integrada

- [ ] 3.1 Rodar `yarn start` e, na preparação, verificar os cenários do spec "Mesmo nome com outra caixa e espaços", "Espaços repetidos no meio" e "Acento diferencia nomes": com "Ana" e "Ana Maria" na lista, digitar "  ANA ", "ana   maria" e "Aná" e conferir o aviso e o estado do botão em cada caso.
- [ ] 3.2 Verificar os cenários "Confirmar pelo teclado com nome repetido", "Aviso some ao corrigir o nome" e "Remover o jogador de mesmo nome": Enter com "ana" não adiciona e mantém texto e foco; trocar para "Bia" tira o aviso; com "ana" no campo, remover "Ana" tira o aviso na hora e Enter adiciona "ana".
- [ ] 3.3 Com VoiceOver, verificar o cenário "Leitor de tela": ao focar o campo com nome repetido, ele é anunciado como inválido e o aviso é lido. Também conferir que o aviso é anunciado uma vez ao aparecer durante a digitação.

## Workflow follow-up

- Atualizar `roadmap-changes.md`: marcar o item 10 (`unique-player-names`) como resolvido por esta change.
- Arquivar a change depois de revisada e testada.
