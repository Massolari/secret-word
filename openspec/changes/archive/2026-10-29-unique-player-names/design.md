# Design

## Context

A preparação guarda o texto digitado em `preparingModel.playerName` e os jogadores em `model.players : Dict Int Player`. Quem está saindo da lista fica em `preparingModel.removing` por 200 ms antes do `RemovePlayer`, e `activePlayers` já filtra esses ids. `AddUser` só faz `String.trim` e recusa vazio. O botão de adicionar usa a mesma condição em `viewPreparePlayers`. O projeto só depende de `elm/core` (não há `List.Extra`). Já existem `.hint` com `aria-live "polite"` e o token `--danger` nos dois temas.

Ver proposal.md para a motivação e specs/game-setup/spec.md para o comportamento.

## Goals / Non-Goals

**Goals:**
- Uma única regra de "nome repetido" usada por `update` e por `view`.
- Aviso acessível, ligado ao campo.

**Non-Goals:**
- Ignorar acentos ou outros caracteres na comparação.
- Mudar o nome salvo (continua só com `String.trim`) ou permitir editar um jogador já cadastrado.
- Validar nomes em outras telas: a lista só muda na preparação.

## Decisions

### 1. Repetição derivada, sem estado novo

O jogador repetido é calculado a cada render a partir de `playerName` e `activePlayers`. Não entra nenhum campo como `duplicateError` no `PreparingModel`.

- O aviso some sozinho quando o texto muda ou quando um `DeletePlayer` põe o jogador de mesmo nome em `removing`, que é o que o spec pede.
- Alternativa descartada: guardar o erro ao tentar adicionar. Isso exigiria limpar o erro em `InputPlayer`, `DeletePlayer` e `RemovePlayer`, e o aviso só apareceria depois de um envio.

### 2. Normalização e busca

- `normalizeName : String -> String` faz `String.words >> String.join " " >> String.toLower`. `String.words` já descarta espaços das pontas e junta sequências internas, e `String.toLower` mantém os acentos.
- `duplicatePlayer : Model -> Maybe Player` normaliza o texto digitado, devolve `Nothing` quando ele fica vazio e, caso contrário, procura em `Dict.values (activePlayers model)` o primeiro jogador com o mesmo nome normalizado (`List.filter` + `List.head`, já que não há `List.find` no core).
- Retornar o `Player` em vez de `Bool` permite que o aviso mostre o nome como foi cadastrado ("Já tem um jogador chamado Ana."), mesmo se o jogador digitou "ana".

### 3. `AddUser` e o botão

- `AddUser` só insere quando o nome sem espaços nas pontas não é vazio e `duplicatePlayer` é `Nothing`. Caso contrário devolve `( model, Cmd.none )`: o texto fica no campo, e o foco também, porque o envio pelo teclado não o tira do input.
- O botão de adicionar fica com `disabled (String.isEmpty (String.trim preparingModel.playerName) || duplicatePlayer model /= Nothing)`. A comparação `/=` em `Maybe Player` funciona, já que `Player` só tem `String` e `Int`.

### 4. Aviso e acessibilidade

- O `form.add-player` passa a ficar dentro de um `div.add-player-group`, junto com um `p` com `id "player-name-error"`, `class "field-error"` e `aria-live "polite"`. Esse `p` está sempre no DOM e fica vazio quando não há repetido. Um `aria-live` só funciona de forma confiável se o elemento já existia antes de o texto mudar.
- O grupo não tem `gap`, então o `p` vazio não ocupa espaço nem herda o `gap` da `.section`.
- O input ganha `attribute "aria-invalid" "true"` e `attribute "aria-describedby" "player-name-error"` só quando há repetido. Assim o leitor de tela anuncia o campo como inválido e lê o aviso ao focar.
- Alternativa descartada: `role "alert"`. Ele interrompe a leitura a cada tecla que mantém o nome repetido, o que fica agressivo durante a digitação.

### 5. Estilo

- `.add-player-group`: `display: flex; flex-direction: column`, sem `gap`.
- `.field-error`: `font-size: 0.875rem`, `color: var(--danger)`. Só `.field-error:not(:empty)` ganha `margin-top: var(--space-1)`.
- `.field[aria-invalid="true"]` (também no `:focus`): `border-color: var(--danger)`. A cor da borda não é o único sinal, porque o texto do aviso também aparece.

## Risks / Trade-offs

- [O aviso aparece enquanto o jogador ainda digita, por exemplo ao escrever "Ana" a caminho de "Anabela".] → É um aviso passageiro e não bloqueia a digitação; some na próxima letra. O anúncio `polite` espera o leitor de tela terminar o que está lendo.
- [Comparar sem ignorar acentos deixa passar "Ana" e "Aná".] → Escolha deliberada: são grafias diferentes que o grupo consegue distinguir na tela. Se incomodar, `normalizeName` é o único ponto a mudar.
- [Inserir o aviso empurra a lista de jogadores para baixo.] → O aviso tem uma linha e só aparece com nome repetido. Não vale reservar espaço fixo na tela no caso comum.
