# Design

## Context

`src/Main.elm` é um `Browser.element` sem rotas. `Model.status` é `Preparing` ou `Playing`, e `view` escolhe uma `Screen` (`key`, `header`, `main`, `footer`) a partir dele. O estado da preparação fica em `Model.players` e `Model.preparingModel`, fora da view, então sobrevive a uma troca de tela.

O voltar do navegador só é interceptado durante a partida: `src/index.js` empilha uma entrada no histórico (`protectGame`) e o `popstate` avisa o Elm pelo port `backPressed`. Na preparação ele é ignorado (`if (!guarded) return`), e o voltar sai da página, o que apagaria os jogadores cadastrados se valesse também nas instruções.

`update` devolve `Effect` como dados, e `tests/Harness.elm` simula cada um num `case` exaustivo. O harness não assina `subscriptions`, então os testes mandam `BackPressed` com `ProgramTest.update`.

Os comportamentos exigidos estão em `specs/game-instructions/spec.md`.

## Goals / Non-Goals

**Goals:**
- O voltar do navegador nas instruções nunca descarta a preparação.
- O Elm continua decidindo o estado. O JS só cuida do histórico.
- A proteção da partida (`protectGame`, `leaveGame`) não muda de comportamento.

**Non-Goals:**
- Abrir as instruções durante a partida.
- Instruções ilustradas ou em etapas.
- Mudar a URL ou criar rotas.
- Mover foco na troca de tela: as outras telas também não movem.

## Decisions

### 1. `Instructions` é um novo `Status`

```elm
type Status
    = Preparing
    | Instructions
    | Playing
```

`view` ganha o ramo `viewInstructions`, e `subscriptions` assina `backPressed` em `Instructions` e `Playing`. `Start` continua aceitando só `Preparing`, então a partida não começa a partir das instruções.

Alternativa considerada: um `Bool` em `PreparingModel`. Permitiria o estado sem sentido de instruções abertas durante `Playing`, e cada ponto que olha `status` precisaria olhar o campo também.

Alternativa considerada: um diálogo sobre a preparação, como `viewExitPrompt`. O texto é longo para um diálogo, e rolar dentro dele no celular é pior que uma tela inteira, que já tem rodapé fixo.

### 2. Mensagens e efeitos

```elm
type Msg = ... | OpenInstructions | CloseInstructions

type Effect = ... | InstructionsEntry Bool | ScrollToTop
```

| Origem | Aceita quando | Novo status | Efeitos |
| --- | --- | --- | --- |
| `OpenInstructions` | `Preparing`, sem `tapGuard` | `Instructions` | `InstructionsEntry True`, `ScrollToTop`, `lockTaps` |
| `CloseInstructions` | `Instructions`, sem `tapGuard` | `Preparing` | `InstructionsEntry False`, `ScrollToTop`, `lockTaps` |
| `BackPressed` | `Instructions`, com ou sem `tapGuard` | `Preparing` | `ScrollToTop`, `lockTaps` |

`BackPressed` ignora o `tapGuard` porque o navegador já consumiu a entrada do histórico. Se a mensagem fosse descartada, a tela ficaria nas instruções sem entrada para o próximo voltar, e ele sairia da página.

`lockTaps` ao fechar impede que um toque duplo em Voltar caia em Começar, que ocupa o mesmo lugar no rodapé. `ScrollToTop` usa `Browser.Dom.setViewport 0 0`. Sem ele, voltar com o texto rolado até o fim deixaria a preparação rolada, sem o cabeçalho.

### 3. Histórico: uma entrada própria, separada da guarda

Novo port `instructionsEntry : Bool -> Cmd msg`. O voltar reaproveita `backPressed`, que o Elm só assina nas telas em que ele tem efeito.

```js
let instructionsOpen = false

// Depois de recarregar nas instruções, esta entrada não abre mais nada
if (history.state?.secretWordInstructions) history.replaceState(null, "")

app.ports?.instructionsEntry?.subscribe((open) => {
    if (open) {
        instructionsOpen = true
        history.pushState({ secretWordInstructions: true }, "")
    } else {
        if (!instructionsOpen) return
        instructionsOpen = false
        history.back()
    }
})

window.addEventListener("popstate", () => {
    if (instructionsOpen) {
        instructionsOpen = false
        app.ports?.backPressed?.send(null)
        return
    }
    if (!guarded) {
        // Avançar para uma entrada de instruções já fechadas
        if (history.state?.secretWordInstructions) history.back()
        return
    }
    pushGuard()
    app.ports?.backPressed?.send(null)
})
```

Fechar pelo botão remove a entrada, para que o próximo voltar saia da página como na preparação de hoje. O `history.back()` do avançar importa além da aparência: sem ele, começar a partida nesse ponto deixaria uma entrada a mais, e o `history.go(-2)` de Sair não sairia da página.

Alternativa considerada: não mexer no histórico e deixar o voltar sair da página. É o gesto mais natural para sair de uma tela de leitura no celular, e ele perderia os jogadores sem aviso.

Alternativa considerada: reaproveitar `protectGame`. Ligaria também o `beforeunload`, e recarregar nas instruções pediria confirmação, o que a spec não quer.

### 4. Botão no cabeçalho, empilhado abaixo da marca

O botão "Como jogar" fica abaixo do slogan, com `btn btn--secondary` e um novo `Icons.help`. No cabeçalho, ele fica longe de Começar e não compete com a ação principal.

Alternativa considerada: ao lado do título. "Palavra Secreta" em 1,75rem mais um botão com ícone e texto não cabem na mesma linha na maioria dos celulares, e um botão só com ícone é menos fácil de descobrir para quem nunca jogou.

Alternativa considerada: no rodapé, junto de Começar. Ocuparia espaço fixo em toda a preparação e ficaria sujeito a toques errados na ação principal.

### 5. Texto escrito na view

O texto fica em `viewInstructions`, como os demais textos do app, com uma lista de seções renderizada por `section.section` e `h2.section__title`. O corpo usa uma nova classe `.instructions__text`, com a cor de texto normal. O título é `h1.brand__title` com "Como jogar", e o rodapé segue o padrão `btn btn--primary btn--lg btn--block`.

Texto previsto:

| Seção | Texto |
| --- | --- |
| O jogo | Em duplas, um jogador dá dicas e o outro tenta adivinhar a palavra antes que o tempo acabe. Vocês jogam passando um único celular. |
| Preparação | Cadastrem pelo menos 2 jogadores e escolham o tempo da rodada. |
| Duplas | A cada rodada, o jogo mostra quem dá as dicas e quem adivinha. Cada jogador dá dicas para cada um dos outros, e a ordem recomeça depois da última dupla. Passem o celular para quem vai dar as dicas. |
| Rodada | Só quem dá as dicas olha a tela. Toque em Acertou para contar 1 acerto e trocar a palavra. Toque em Pular para trocar a palavra. Pular custa até 5 segundos do tempo restante: o custo cai 1 segundo a cada segundo com a palavra na tela, até zerar. |
| Pontos | Quem adivinha ganha 1 ponto por acerto. Quem dá as dicas ganha metade, arredondada para baixo: com 5 acertos, são 5 e 2 pontos. |
| Fim da partida | Joguem quantas rodadas quiserem. Quando quiserem parar, toquem em Encerrar partida. Vence quem tiver mais pontos. |

O app não impõe restrições sobre o que vale como dica, então o texto também não impõe.

Os valores "2 jogadores" e "5 segundos" ficam literais, como em "Pular (perde 5 segundos)" e "Adicione pelo menos 2 jogadores". Um teste confere o trecho de Pular, e mudanças de regra precisam atualizar o texto.

## Risks / Trade-offs

- [Depois de recarregar nas instruções, sobra uma entrada a mais no histórico] → O voltar precisa de um toque extra para sair do jogo, e o `history.go(-2)` de Sair numa partida iniciada em seguida para nessa entrada, numa preparação vazia. Nada é perdido além do que o próprio recarregar já descartou. Aceito: o caso é raro e corrigir exigiria mudar a guarda da partida.
- [`history.back()` é assíncrono] → Um toque em Começar antes de ele terminar faria a guarda ser empilhada e logo removida. `lockTaps` segura Começar por 500 ms, bem mais que uma navegação dentro do mesmo documento.
- [Texto e regras podem divergir] → Os cenários de conteúdo viram testes dos trechos que citam regras, e a spec exige que o texto reflita as regras em vigor.
