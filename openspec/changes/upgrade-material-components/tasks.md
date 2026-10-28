# Tasks

## 1. Dependências e assets

- [x] 1.1 Antes de mexer no código, confirmar com o usuário o que fazer com as alterações sem commit em `src/Main.elm`, `src/Words.elm` e `src/index.css` (commit, stash ou trabalhar por cima). Verificar com `git status --short`.
- [x] 1.2 Atualizar `aforemny/material-components-web-elm` para `9.1.0` em `elm.json`, sem mexer nos outros pacotes. Verificar com `git diff elm.json`, que deve mostrar apenas essa linha, mais as indiretas se o compilador exigir.
- [x] 1.3 Adicionar `material-components-web-elm@9.1.0` (versão exata, sem `^`) às devDependencies com `yarn add --dev --exact material-components-web-elm@9.1.0`. Verificar que `node_modules/material-components-web-elm/dist/material-components-web-elm.js` e o `.css` existem.
- [x] 1.4 Importar o JS e o CSS do pacote no topo de `src/index.js`, antes de `Elm.Main.init`, e remover de `src/index.html` as duas tags do unpkg `material-components-web-elm@2.1.2`, mantendo o link do Google Fonts. Verificar com `grep -n unpkg src/index.html`, que não deve retornar nada.

## 2. Migração das views em `src/Main.elm`

- [x] 2.1 Trocar os imports de `Material.*` por imports qualificados (`Button`, `Fab`, `ExtendedFab`, `IconButton`, `LayoutGrid`, `TextField`, `MList`, `MListItem`; `Typography` continua expondo `headline4` e `headline5`), conforme a decisão 1 da design, e remover o `span3`, que não é usado. Verificar com `elm make src/Main.elm --output=/dev/null`, que deve acusar só erros de uso nas views, não de import.
- [x] 2.2 Adicionar o helper `viewList` (decisão 3), que recebe uma config e uma lista de `ListItem` e renderiza `text ""` quando a lista é vazia ou `MList.list cfg first rest` quando não é. Verificar que o compilador aceita a assinatura.
- [x] 2.3 Migrar `viewPreparing`, `viewPrepareTime` e `viewPreparePlayers` para a API 9.x, partindo da tabela da decisão 2 (trocas de componente permitidas pela decisão 5): `Button.raised` com ícone `play_arrow` à direita e as mesmas regras de `disabled`, `TextField.filled` com `setType (Just "time")` e `(Just "text")`, `Fab.fab` com `setExited`, `IconButton` de `delete` em `MListItem.meta` e a lista de jogadores via `viewList`. Verificar que `elm make` não acusa mais erros nessas funções.
- [x] 2.4 Partindo da tabela da decisão 2 (trocas de componente permitidas pela decisão 5), migrar `viewGame` (placar com `MList.setTwoLine True` e `MListItem.text [] { primary, secondary }` via `viewList`), `viewGameShowingPlayers`, `viewPlaying` (`ExtendedFab.fab` com `setAttributes [ disabled True ]`), `viewPlayingButton` (`Fab.fab` com `Fab.icon`) e `viewGameOver`. Verificar que `elm make src/Main.elm --output=/dev/null` termina com `Success!`.

## 3. Verificação integrada

- [x] 3.1 Rodar `yarn build` com Node 24 e confirmar que termina sem erros e que o `dist/` contém o CSS do MDC no bundle (por exemplo, com `grep -l mdc-button dist/*.css`).
- [x] 3.2 Rodar `yarn start` e, no navegador, percorrer o roteiro sem nenhum erro no console:
  - preparação: o campo de tempo funciona; o FAB `add` só aparece com nome preenchido; adicionar e remover jogadores pelo ícone `delete`; "Começar" fica desabilitado com menos de 2 jogadores ou tempo vazio;
  - jogo: o placar aparece em duas linhas, ordenado por pontuação decrescente; os botões `done` e `redo` funcionam; durante a contagem para pular, o controle de pular mostra o número e não reage ao clique;
  - fim da rodada: os pontos aparecem e "Continuar" volta ao fluxo;
  - coerência semântica (decisão 5): "acerto" não usa cor de erro ou perigo; "pular" não compete visualmente com "acerto"; os estados desabilitados são evidentes; cada componente trocado continua cumprindo o mesmo papel.
- [x] 3.3 Ajustar o tema e os estilos (`src/index.css`, variáveis `--mdc-theme-*`) conforme a decisão 5, corrigindo também alinhamentos quebrados (listas em `.list-inline-block mdc-list`, botões em `.action-button`). Registrar na tarefa as mudanças visuais e as trocas de componente feitas, com o motivo de cada uma, e repetir o roteiro da 3.2.
  - **Remover jogador não funcionava:** na 9.x o `mdc-list` com `setInteractive False` aplica `pointer-events: none` inline na lista, e o ícone `delete` deixava de receber clique. Corrigido com `.list-inline-block .mdc-deprecated-list-item__meta { pointer-events: auto }`, mantendo a lista não interativa.
  - **Tema:** `--mdc-theme-primary: #00796b` (teal, para "Começar" e "Continuar") e `--mdc-theme-secondary: #2e7d32` (verde, para os FABs `add` e `done`, que são ações positivas). O MDC fixa em CSS o roxo do rótulo focado do text field, por isso ele foi sobrescrito para `var(--mdc-theme-primary)`.
  - **Pular (`redo`):** FAB neutro (cinza `#e0e0e0` com ícone escuro) via override de `--mdc-theme-secondary` em `.action-button--skip`, para não competir com o acerto verde.
  - **Contagem para pular:** troca de `ExtendedFab.fab` com `disabled True` por `Fab.fab` com `Fab.customIcon` mostrando o número. Motivo: o `disabled` não tinha efeito visual no `mdc-fab` (é property, não atributo), e o FAB estendido tinha outro tamanho e ficava desalinhado. Agora ele ocupa o mesmo lugar e tem o mesmo tamanho do botão `redo`, com aparência desabilitada (`.fab--disabled`: fundo `rgba(0,0,0,.12)`, texto `rgba(0,0,0,.38)`, sem sombra, `pointer-events: none`) e `aria-disabled="true"`. O import de `Material.Fab.Extended` saiu.
  - **Alinhamento:** `vertical-align: middle` nos FABs de `.action-button`.
  - **Build:** o `package.json` ganhou `"targets": { "default": { "context": "browser" } }`. Com `engines.node` e sem alvo de navegador, o Parcel 2 tratava a build como Node e deixava os imports de `node_modules` externos, então o MDC não entrava no bundle.
  - O roteiro da 3.2 foi repetido (Chrome headless via CDP, cliques reais de mouse): 23/23 verificações passaram, e o console só mostra o aviso de modo DEBUG do Elm em dev.

## Workflow follow-up

- Fazer commit da change (por exemplo, "Migrar material-components-web-elm para 9.1.0") depois da revisão do usuário.
- Arquivar com `/opsx:archive`. Não há sync de specs, porque a change usa `skip_specs: true`.
