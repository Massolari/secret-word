# Proposal

## Why

Quando a rodada termina, a tela de fim mostra só os pontos. Os jogadores não sabem quais palavras foram acertadas e quais ficaram para trás, e é comum querer rever ("qual era aquela que a gente pulou?"). Mostrar o histórico da rodada na mesma tela dos pontos fecha a rodada com esse resumo.

## What Changes

- Durante a rodada, o jogo passa a registrar cada palavra exibida e o resultado dela: **acertada** (tocou em Acertou) ou **errada** (tocou em Pular, ou o tempo acabou com ela na tela).
- A tela de fim da rodada ("Tempo esgotado!") passa a mostrar, junto com os pontos, a lista das palavras da rodada na ordem em que apareceram, cada uma marcada como acerto ou erro de forma distinguível por ícone e cor (não só por cor).
- O histórico é zerado ao tocar em Continuar; cada rodada mostra apenas as próprias palavras.
- Uma rodada sem palavras exibidas não acontece (a primeira palavra é sorteada ao começar), mas o histórico deve lidar com lista vazia sem quebrar.

## Capabilities

### New Capabilities

(nenhuma)

### Modified Capabilities

- `game-round`: novo requisito "Histórico de palavras da rodada": a tela de fim da rodada mostra as palavras exibidas e o resultado de cada uma. Os requisitos existentes não mudam.

## Impact

- `src/Main.elm`: novo campo no `GameModel` para o histórico da rodada; `Correct`, `Jump` e o `Tick` que encerra a rodada passam a registrar o resultado da palavra atual; `Restart` zera o histórico; `viewGameOver` ganha a seção do histórico.
- `src/Icons.elm`: pode precisar de um ícone de "x" para erro, caso ainda não exista.
- `src/index.css`: estilos da lista de palavras (no padrão de `.card`, `.section` e `.result`), incluindo o caso de `prefers-reduced-motion`.
- Sem dependências novas e sem mudança na pontuação.
