# Spec Delta

## Purpose

Define como o jogador chega às instruções do jogo a partir da preparação, o que elas explicam e como ele volta sem perder o que já tinha preenchido.

## ADDED Requirements

### Requirement: Acesso às instruções pela preparação
A tela de preparação SHALL mostrar um botão "Como jogar" no cabeçalho, sempre habilitado, com qualquer número de jogadores. Tocar nele SHALL abrir a tela de instruções, mostrada a partir do começo do texto. As instruções SHALL NOT ter botão de acesso nas telas da partida.

#### Scenario: Abrir as instruções sem jogadores
- **WHEN** a preparação não tem jogadores e o jogador toca em Como jogar
- **THEN** a tela de instruções aparece

#### Scenario: Partida em andamento
- **WHEN** a partida começou
- **THEN** nenhuma tela da partida mostra o botão Como jogar

### Requirement: Conteúdo das instruções
A tela de instruções SHALL explicar o objetivo do jogo e, nesta ordem: a preparação, as duplas, a rodada, a pontuação e como a partida termina. O texto SHALL usar os mesmos nomes dos botões do jogo e SHALL refletir as regras em vigor.

#### Scenario: Preparação e duplas
- **WHEN** a tela de instruções está aberta
- **THEN** o texto diz que são precisos pelo menos 2 jogadores e que o tempo da rodada é escolhido na preparação
- **AND** explica que cada jogador dá dicas para cada um dos outros, passando o celular a cada rodada

#### Scenario: Rodada
- **WHEN** a tela de instruções está aberta
- **THEN** o texto diz que Acertou conta 1 acerto e troca a palavra
- **AND** diz que Pular troca a palavra e custa 5 segundos do tempo restante

#### Scenario: Pontuação e fim da partida
- **WHEN** a tela de instruções está aberta
- **THEN** o texto diz que quem adivinha ganha 1 ponto por acerto e quem dá as dicas ganha metade, arredondada para baixo
- **AND** diz que a partida termina ao tocar em Encerrar partida e que vence quem tiver mais pontos

### Requirement: Voltar pelo botão da tela
A tela de instruções SHALL ter um botão Voltar como ação principal, fixo no rodapé. Tocar nele SHALL retornar à tela de preparação, mostrada a partir do topo. Depois disso, o voltar do navegador SHALL seguir o comportamento normal da preparação.

#### Scenario: Instruções mais compridas que a tela
- **WHEN** o texto passa da altura da tela
- **THEN** o texto rola e o botão Voltar continua visível na parte de baixo

#### Scenario: Voltar pelo botão
- **WHEN** a tela de instruções está aberta e o jogador toca em Voltar
- **THEN** a tela de preparação aparece, a partir do topo

#### Scenario: Voltar do navegador depois do botão
- **WHEN** o jogador chegou ao jogo vindo de outra página, abriu as instruções, tocou em Voltar e usa o voltar do navegador
- **THEN** o navegador volta para a página de onde ele veio, sem passar de novo pelas instruções

### Requirement: Voltar pelo navegador nas instruções
Com a tela de instruções aberta, o voltar do navegador (botão ou gesto) SHALL retornar à tela de preparação em vez de sair da página, sem pedir confirmação. O avançar do navegador SHALL NOT reabrir as instruções.

#### Scenario: Voltar pelo navegador
- **WHEN** a tela de instruções está aberta e o jogador usa o voltar do navegador
- **THEN** a tela de preparação aparece, sem diálogo, e a página não muda

#### Scenario: Voltar duas vezes
- **WHEN** o jogador chegou ao jogo vindo de outra página, abriu as instruções e usa o voltar do navegador duas vezes
- **THEN** o primeiro voltar mostra a preparação e o segundo leva à página de onde ele veio

#### Scenario: Avançar depois de voltar
- **WHEN** o jogador voltou das instruções pelo navegador e usa o avançar do navegador
- **THEN** a tela de preparação continua visível

#### Scenario: Partida depois de visitar as instruções
- **WHEN** o jogador voltou das instruções por qualquer caminho, começou a partida, usou o voltar do navegador e tocou em Sair
- **THEN** o navegador volta para a página de onde ele veio

### Requirement: Preparação preservada
Abrir as instruções e voltar SHALL manter os jogadores, a ordem deles, o tempo escolhido e o texto digitado no campo de nome. Enquanto as instruções estiverem abertas, o estado da preparação SHALL continuar válido para começar a partida depois de voltar.

#### Scenario: Voltar com a preparação preenchida
- **WHEN** a preparação tem Ana e Beto, 2min selecionado e "Cai" digitado no campo, e o jogador abre as instruções e volta
- **THEN** a preparação mostra Ana e Beto, 2min selecionado e "Cai" no campo

#### Scenario: Começar depois de voltar
- **WHEN** a preparação tem Ana e Beto, o jogador abre as instruções, volta e, passada a proteção contra toques, toca em Começar
- **THEN** o anúncio da primeira dupla aparece, com Ana dando dicas para Beto

### Requirement: Recarregar nas instruções
Recarregar a página com a tela de instruções aberta SHALL seguir o comportamento da preparação: a página recarrega sem pedir confirmação e mostra a tela de preparação vazia.

#### Scenario: Recarregar nas instruções
- **WHEN** a tela de instruções está aberta e o jogador recarrega a página
- **THEN** a página recarrega sem pedir confirmação e mostra a tela de preparação vazia

### Requirement: Proteção contra toques nas instruções
Depois de abrir ou fechar as instruções, os botões da nova tela SHALL ignorar toques e teclas nos primeiros 500 ms e responder normalmente depois. O voltar do navegador SHALL funcionar mesmo durante esse bloqueio. A aparência dos botões SHALL NOT mudar por causa dele.

#### Scenario: Toque duplo em Voltar
- **WHEN** com jogadores suficientes, o jogador toca duas vezes seguidas em Voltar nas instruções
- **THEN** a tela de preparação aparece e continua visível, e a partida não começa

#### Scenario: Toque depois do bloqueio
- **WHEN** a tela de instruções abriu há mais de 500 ms e o jogador toca em Voltar
- **THEN** a tela de preparação aparece

#### Scenario: Voltar do navegador logo depois de abrir
- **WHEN** a tela de instruções abriu há menos de 500 ms e o jogador usa o voltar do navegador
- **THEN** a tela de preparação aparece, e a página não muda

### Requirement: Instruções acessíveis
A tela de instruções SHALL ter um título de nível 1 e seções com títulos, para que leitores de tela possam navegar por ela. Em telas de 320px de largura, o botão Como jogar e o texto SHALL caber sem rolagem horizontal, com área de toque de pelo menos 48×48px no botão.

#### Scenario: Leitor de tela
- **WHEN** a tela de instruções está aberta e o jogador usa um leitor de tela
- **THEN** a tela tem um título "Como jogar" de nível 1 e um título para cada parte das regras

#### Scenario: Celular pequeno
- **WHEN** o jogo é aberto numa tela de 320px de largura
- **THEN** a preparação e as instruções cabem na largura sem rolagem horizontal
