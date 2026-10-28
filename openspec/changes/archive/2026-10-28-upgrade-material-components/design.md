# Design

## Context

Motivação: ver `proposal.md`.

Hoje todo o uso do Material está nas views de `src/Main.elm`, com a API 2.x de records de config e funções importadas sem qualificação (`raisedButton`, `textField`, `fab`, `extendedFab`, `iconButton`, `list`/`listItem`, `layoutGrid*`, `headline4`/`headline5`). O JS/CSS do componente é carregado do unpkg em `src/index.html`, fixo na 2.1.2.

Na 9.1.0 (conferido no `docs.json` e no código-fonte da tag):

- Toda config é opaca e montada com builders: `X.config |> X.setFoo ...`.
- Ícones são tipados: `Button.icon "play_arrow"`, `Fab.icon "add"`, `IconButton.icon "delete"`.
- `Material.List.list : Config msg -> ListItem msg -> List (ListItem msg) -> Html msg` exige pelo menos um item.
- Os componentes continuam sendo custom elements (`mdc-button`, `mdc-fab`, `mdc-icon-button`, `mdc-list`, `mdc-text-field`), definidos pelo JS do pacote. Esse JS precisa estar na mesma versão do pacote Elm.

O projeto não tem testes automatizados. A verificação é a compilação mais um roteiro manual no navegador.

## Goals / Non-Goals

**Goals:**
- Compilar com `aforemny/material-components-web-elm` 9.1.0 sem mudar o comportamento funcional. O visual pode evoluir dentro dos critérios da decisão 5.
- Manter a versão do JS/CSS amarrada à do pacote Elm por meio de um único arquivo versionado (`package.json`).

**Non-Goals:**
- Mudar o fluxo do jogo ou a estrutura das telas (quais telas existem e em que ordem).
- Mudar a lógica do jogo, o modelo ou as mensagens.
- Remover o `mdgriffith/elm-ui`, que está no `elm.json` mas não é importado.

## Decisions

### 1. Imports qualificados por módulo

Usar `import Material.Button as Button`, `import Material.Fab as Fab`, `import Material.Fab.Extended as ExtendedFab`, `import Material.IconButton as IconButton`, `import Material.LayoutGrid as LayoutGrid`, `import Material.List as List_`... Na 9.x vários módulos expõem nomes iguais (`config`, `icon`, `setOnClick`, `text`), que colidem entre si e com `Html.text`, então importá-los sem qualificação não funciona. Para evitar colisão com `List` do core, usar `Material.List as MList` e `Material.List.Item as MListItem`. `Material.Typography` pode continuar com `exposing (headline4, headline5)`, porque esses nomes não colidem.

*Alternativa:* expor tudo com nomes renomeados. Foi descartada porque a 9.x não permite renomear itens expostos individualmente.

### 2. Equivalência direta 2.x → 9.x

A tabela é o ponto de partida da migração, não uma obrigação. Qualquer componente pode ser trocado por outro da 9.x, seguindo os critérios da decisão 5.

| 2.1.2 | 9.1.0 |
|---|---|
| `raisedButton { buttonConfig \| icon = Just i, trailingIcon = True, disabled = d, onClick = Just m } label` | `Button.raised (Button.config \|> Button.setIcon (Just (Button.icon i)) \|> Button.setTrailingIcon True \|> Button.setDisabled d \|> Button.setOnClick m) label` |
| `textField { textFieldConfig \| label, type_, onInput, value }` | `TextField.filled (TextField.config \|> TextField.setLabel ... \|> TextField.setType (Just t) \|> TextField.setOnInput f \|> TextField.setValue (Just v))` |
| `fab { fabConfig \| onClick, exited } "add"` | `Fab.fab (Fab.config \|> Fab.setOnClick m \|> Fab.setExited e) (Fab.icon "add")` |
| `extendedFab { extendedFabConfig \| additionalAttributes = [ disabled True ] } label` | `ExtendedFab.fab (ExtendedFab.config \|> ExtendedFab.setAttributes [ disabled True ]) label` |
| `iconButton { iconButtonConfig \| onClick } "delete"` | `IconButton.iconButton (IconButton.config \|> IconButton.setOnClick m) (IconButton.icon "delete")` |
| `layoutGrid` / `layoutGridInner` / `layoutGridCell` / `span12` | `LayoutGrid.layoutGrid` / `LayoutGrid.inner` / `LayoutGrid.cell` / `LayoutGrid.span12` |
| `list { listConfig \| nonInteractive = True, twoLine = t } items` | `MList.list (MList.config \|> MList.setInteractive False \|> MList.setTwoLine t) first rest` |
| `listItem listItemConfig [...]`, `listItemMeta` | `MListItem.listItem MListItem.config [...]`, `MListItem.meta` |
| `listItemText [] [ listItemPrimaryText [] p, listItemSecondaryText [] s ]` | `MListItem.text [] { primary = p, secondary = s }` |

O `textField` da 2.x é filled por padrão, então o equivalente é `TextField.filled`, não `outlined`. O import de `span3`, que não é usado, sai.

### 3. Listas não vazias

As duas listas (jogadores na preparação e placar no jogo) passam a converter os itens com `case items of [] -> text "" ; first :: rest -> MList.list cfg first rest`. Na preparação isso só formaliza o `if List.isEmpty` que já existe. No jogo a lista nunca fica vazia, porque é preciso ter pelo menos 2 jogadores para começar, mas o tipo exige o mesmo tratamento. Um helper local (`viewList`) evita repetir o código.

### 4. Assets via npm em vez de CDN

Adicionar `material-components-web-elm@9.1.0` como devDependency, sem `^`, e importar em `src/index.js` antes de `Elm.Main.init`:

```js
import "material-components-web-elm/dist/material-components-web-elm.js"
import "material-components-web-elm/dist/material-components-web-elm.css"
```

As duas tags do unpkg saem de `src/index.html`. A fonte do Google Fonts (Roboto e Material Icons) continua.

*Por quê:* o README da 9.1.0 exige que o JS/CSS seja exatamente da mesma versão do pacote Elm. Com a versão fixa no `package.json`, o `yarn.lock` cuida disso e a build não depende do unpkg em produção. O Parcel 2 já empacota JS e CSS importados.

*Alternativa:* só trocar `2.1.2` por `9.1.0` nas URLs do unpkg. É mais simples, mas deixa a versão duplicada em dois arquivos sem ligação entre eles e mantém a dependência de um CDN externo.

### 5. Liberdade visual com coerência semântica

O visual não precisa reproduzir a 2.x: cores, espaçamento e estilo podem mudar, e componentes podem ser trocados. A regra é que cada elemento comunique corretamente o seu papel:

- As cores vêm do tema do MDC (variáveis `--mdc-theme-*` em `src/index.css`), não de estilos espalhados pelo código.
- "Acerto" (`done`) usa cor positiva ou primária, nunca vermelha nem de erro.
- "Pular" (`redo`) usa cor neutra ou secundária, sem competir visualmente com o acerto.
- "Remover jogador" (`delete`) pode usar a cor de erro, porque é uma ação destrutiva.
- Estados desabilitados continuam visivelmente desabilitados ("Começar" sem jogadores suficientes, contagem para pular).
- Uma troca de componente só vale se o novo cumprir o mesmo papel no mesmo lugar. Exemplo: o FAB estendido que mostra a contagem para pular pode virar um componente com `setDisabled` nativo, como `Button`, desde que continue lido como "pular, ainda indisponível".

*Por quê:* o usuário liberou mudanças visuais, mas um visual contraintuitivo (como acerto em vermelho) prejudica o jogo mais do que manter o estilo antigo.

## Risks / Trade-offs

- **Mudanças visuais podem ficar inconsistentes entre telas** → concentrar as cores no tema (decisão 5) e conferir as três telas no roteiro manual.
- **O `disabled` no FAB estendido pode não ter efeito no `mdc-fab`**, já que a 9.x não oferece `setDisabled` para FAB → é o mesmo mecanismo usado hoje (um atributo HTML). Se no roteiro manual o botão de pular reagir durante a contagem, trocar o componente por um com `setDisabled` nativo, conforme a decisão 5.
- **A ordem de carregamento do JS**: os custom elements precisam estar definidos antes ou logo depois da primeira renderização → importar o JS antes do `Elm.Main.init`. Elementos definidos depois também são atualizados pelo navegador.
- **Alterações sem commit em `src/Main.elm`** (trabalho em andamento do usuário) → a implementação edita o arquivo no estado atual e não deve descartar nem reverter essas alterações. Se possível, fazer commit ou stash delas antes de aplicar a change.
- **O bundle fica maior**, porque o JS/CSS do MDC passa a ir no bundle em vez de vir do CDN → é aceitável para um app pequeno, e o tamanho é o mesmo que já era baixado do unpkg.

## Migration Plan

Fazer tudo em um único commit. Para desfazer, basta reverter esse commit, já que não há dados nem estado persistido envolvidos.
