# Proposal

## Why

O projeto usa `aforemny/material-components-web-elm` **2.1.2**, de 2020. A versão atual é a **9.1.0**. Ficar sete majors atrás impede correções e componentes novos, e prende o JS/CSS a um CDN de 2020. Depois da migração para Elm 0.19.3 e Parcel 2, esta é a última dependência importante desatualizada.

## What Changes

- Atualizar `aforemny/material-components-web-elm` de 2.1.2 para 9.1.0 no `elm.json`.
- Reescrever as chamadas de Material em `src/Main.elm` para a API 9.x. A API trocou records de config (`{ buttonConfig | ... }`) por builders (`Button.config |> Button.setOnClick ...`) e renomeou ou moveu vários módulos e funções (`Material.Fab.Extended`, `Material.List.Item`, `LayoutGrid.cell`/`inner`, `TextField.filled`, ícones tipados).
- Ajustar as listas de jogadores: na 9.x, `Material.List.list` exige um primeiro item, então lista vazia precisa ser tratada no código.
- Trocar o JS/CSS do componente, hoje carregado do unpkg na versão 2.1.2 em `src/index.html`, pelo pacote npm `material-components-web-elm@9.1.0`, importado em `src/index.js` e empacotado pelo Parcel.
- Preservar o comportamento funcional: o fluxo das telas, as regras de habilitar e desabilitar, os textos e a pontuação. O visual pode mudar (cores, espaçamento, estilo) e componentes podem ser trocados por outros da 9.x, desde que o resultado faça sentido no contexto. Por exemplo, a ação de "acerto" nunca deve ter uma cor de erro ou perigo.

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

(nenhuma; é uma atualização de dependência sem mudança de comportamento, por isso a change usa `skip_specs: true`)

## Impact

- `elm.json`: a versão do pacote Material vai para 9.1.0. As dependências indiretas podem mudar, já que a 9.1.0 exige `elm/virtual-dom >= 1.0.3`, e o projeto já tem a 1.0.5.
- `src/Main.elm`: todas as views que usam Material (`viewPreparing`, `viewPrepareTime`, `viewPreparePlayers`, `viewGame`, `viewGameShowingPlayers`, `viewPlaying`, `viewPlayingButton`, `viewGameOver`) e os imports.
- `src/index.html` e `src/index.js`: troca da carga dos assets pelo CDN pela importação via npm.
- `package.json` e `yarn.lock`: nova dependência `material-components-web-elm@9.1.0`.
- `src/index.css`: o seletor `.list-inline-block mdc-list` continua válido, porque a 9.1.0 ainda renderiza a lista como o elemento `mdc-list`. Tema e estilos podem ser ajustados livremente, conforme a decisão 5 da design.
