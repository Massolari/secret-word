# Palavra Secreta

Jogo de adivinhar palavras para jogar em grupo, presencialmente, passando um único celular de mão em mão. Um jogador dá dicas, outro tenta adivinhar a palavra na tela antes que o tempo acabe.

Feito em [Elm](https://elm-lang.org/), empacotado com [Parcel](https://parceljs.org/), e pensado primeiro para celular.

## Como jogar

1. **Preparação** — cadastre pelo menos 2 jogadores (nomes repetidos não são aceitos) e escolha o tempo da rodada: 30s, 1min, 1min30, 2min ou 3min.
2. **Anúncio da dupla** — o jogo mostra quem dá as dicas e quem adivinha. As duplas seguem a ordem de cadastro: cada jogador dá dicas para cada um dos outros e, ao fim, a ordem recomeça. Com Ana, Beto e Caio: Ana→Beto, Ana→Caio, Beto→Ana, Beto→Caio, Caio→Ana, Caio→Beto.
3. **Rodada** — quem dá as dicas vê a palavra e tenta fazer o parceiro adivinhá-la.
   - **Acertou** soma 1 acerto e sorteia outra palavra.
   - **Pular** troca a palavra, mas custa 5 segundos do tempo restante.
4. **Fim da rodada** — quando o tempo zera, quem adivinhou ganha 1 ponto por acerto e quem deu as dicas ganha metade (arredondada para baixo). A tela lista as palavras da rodada, marcando acertos e erros, e mostra o placar.

As palavras não se repetem na partida até a lista inteira ter saído.

### Detalhes pensados para o uso real

- A rodada **pausa sozinha** quando a tela é bloqueada, a aba fica oculta ou outro app vai para a frente; o tempo fora da tela não é descontado.
- A tela **fica acesa** durante a rodada (Screen Wake Lock, quando disponível).
- O celular **vibra** a cada acerto e quando o tempo acaba (quando disponível).
- Os botões ignoram toques nos primeiros 500 ms após cada troca de tela, para evitar toques duplos acidentais.
- Tema claro/escuro automático, respeito a "movimento reduzido", alvos de toque de pelo menos 48px e suporte a leitor de tela.

## Rodando localmente

Requer Node.js 24 ou mais recente (veja `.nvmrc`) e Yarn.

```sh
yarn install
yarn start      # servidor de desenvolvimento em http://localhost:1234
yarn build      # build de produção em dist/
```

O conteúdo de `dist/` é estático e pode ser servido por qualquer hospedagem de arquivos.

## Estrutura

```
src/
  index.html   # página de entrada
  index.js     # inicializa o Elm e implementa as ports (vibração e wake lock)
  index.css    # estilos e temas
  Main.elm     # estado do jogo, telas e lógica das rodadas
  Words.elm    # lista curada de palavras
  Icons.elm    # ícones SVG
openspec/
  specs/       # especificação do comportamento do jogo
```

### Lista de palavras

`src/Words.elm` contém uma lista única e curada de palavras de uso comum no português brasileiro: uma palavra por entrada, com inicial maiúscula, sem duplicatas e sem termos técnicos, arcaicos ou de nicho. Para adicionar ou remover palavras, edite essa lista seguindo esses critérios (definidos em `openspec/specs/word-list/spec.md`).

## Desenvolvimento com OpenSpec

O comportamento do jogo é especificado em [`openspec/specs/`](openspec/specs/), dividido por área:

| Spec | Cobre |
| --- | --- |
| `game-setup` | Cadastro de jogadores e escolha do tempo |
| `game-round` | Duplas, rodada, cronômetro, pontuação, placar, pausa |
| `ui-theme` | Tema, layout mobile-first, toque, animações |
| `word-list` | Critérios da lista de palavras |

Mudanças são feitas pelo fluxo do [OpenSpec](https://github.com/Fission-AI/OpenSpec): cada mudança é proposta como uma *change* (proposta, design, deltas de spec e tarefas), implementada e depois arquivada em `openspec/changes/archive/`, atualizando as specs principais. No Claude Code, os comandos `/opsx:propose`, `/opsx:apply` e `/opsx:archive` conduzem esse ciclo.
