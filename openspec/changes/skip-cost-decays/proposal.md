# Proposal

## Why

A change `skip-costs-time` trocou "Pular liberado depois de 5 segundos" por "Pular custa 5 segundos", e isso mudou mais do que o previsto. Antes, depois de 5 segundos numa palavra difícil, pular não custava nada a mais. Hoje, quem tenta por 20 segundos ainda perde 5, ou seja, a palavra consome 25 segundos. Quem insiste é punido, e cada palavra pulada pode custar bem mais do que na regra original.

Com um custo que começa em 5 segundos e cai junto com o tempo da palavra na tela, recuperamos o efeito da regra original: uma palavra pulada consome 5 segundos de rodada (tempo na tela + custo), ou o tempo que ficou na tela, se tiver sido mais. A diferença é que o jogador não precisa mais esperar sem poder fazer nada.

## What Changes

- **BREAKING (regra do jogo)**: o custo de pular deixa de ser fixo. Ele começa em 5 segundos quando a palavra aparece e cai 1 segundo a cada segundo de rodada, até zerar. A partir daí, pular é de graça.
- O custo volta a 5 segundos a cada nova palavra, seja depois de Acertou ou de Pular.
- O tempo em pausa não reduz o custo.
- O botão mostra o custo atual ("−5s", "−4s"… "−1s") e o anuncia para leitores de tela. Sem custo, o botão mostra só "Pular".
- O botão passa a ficar desabilitado só quando a palavra apareceu com 5 segundos ou menos restantes. Uma palavra que apareceu antes disso pode ser pulada até o fim da rodada, mesmo com menos de 5 segundos restantes. Um pulo continua sem poder terminar a rodada.

## Capabilities

### New Capabilities

_Nenhuma._

### Modified Capabilities

- `game-round`: o requisito "Pular custa tempo" passa a ter um custo que diminui com o tempo da palavra na tela, e muda a condição para o botão ficar desabilitado.

## Impact

- `src/Main.elm`: `GameModel` ganha o tempo restante no momento em que a palavra apareceu. `skipCost` e `canSkip` passam a depender dele, e `Jump` e `viewSkipButton` usam o custo atual.
- `tests/`: os testes de Pular chegam ao tempo desejado com a mesma palavra na tela, então passariam a pular de graça e precisam ser reescritos. Os helpers `skip` e `skipButton` hoje dependem do texto fixo "Pular (perde 5 segundos)".
- `README.md`: a regra do Pular muda.
- `src/index.css`: uma regra para esconder o custo sem liberar o espaço dele. Acima de 360 px, as colunas de `.actions` crescem com o conteúdo, e os botões mudariam de largura quando o custo some. `.skip-cost` já usa algarismos de largura fixa.
- Sem novas dependências.
