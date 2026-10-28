# ui-theme Specification

## Purpose

Define a aparência e o comportamento visual comuns a todas as telas do jogo: tema, layout no celular, alvos de toque e a política de animações, incluindo movimento reduzido.

## Requirements

### Requirement: Tema claro e escuro automáticos
A interface SHALL seguir a preferência de cor do sistema operacional: tema claro quando o sistema está no modo claro e tema escuro quando está no modo escuro. Os dois temas SHALL manter contraste de pelo menos 4,5:1 para texto normal e 3:1 para texto grande e ícones.

#### Scenario: Sistema no modo escuro
- **WHEN** o aparelho está configurado no modo escuro e o jogador abre o jogo
- **THEN** o fundo, as superfícies e o texto usam a paleta escura
- **AND** a barra do navegador no celular acompanha a cor de fundo escura

#### Scenario: Troca de modo com o jogo aberto
- **WHEN** o sistema muda de claro para escuro enquanto o jogo está aberto
- **THEN** a interface passa para o tema escuro sem recarregar e sem perder o estado da partida

### Requirement: Layout mobile-first em coluna única
Todas as telas SHALL usar uma coluna única centralizada, com largura máxima confortável para leitura, ocupando a altura visível da tela. Em telas de 320px de largura ou mais, nenhum conteúdo SHALL exigir rolagem horizontal.

#### Scenario: Celular pequeno
- **WHEN** o jogo é aberto numa tela de 320px de largura
- **THEN** todo o conteúdo cabe na largura sem rolagem horizontal

#### Scenario: Desktop
- **WHEN** o jogo é aberto numa tela larga
- **THEN** o conteúdo fica numa coluna centralizada de largura limitada, e não esticado de ponta a ponta

### Requirement: Ação principal ao alcance do polegar
Em cada tela, a ação principal (Começar, Acertou/Pular, Continuar) SHALL ficar fixa na parte de baixo da área visível, acima da safe area do aparelho, e SHALL continuar visível quando o teclado virtual não está aberto.

#### Scenario: iPhone com barra de gestos
- **WHEN** o jogo é aberto num iPhone com indicador de início na parte de baixo
- **THEN** os botões principais ficam acima da safe area inferior, sem sobreposição com o indicador

#### Scenario: Lista longa de jogadores
- **WHEN** há jogadores suficientes para a lista passar da altura da tela
- **THEN** a lista rola e o botão principal continua visível na parte de baixo

### Requirement: Alvos de toque confortáveis
Todo elemento interativo SHALL ter área de toque de pelo menos 48×48px. Toques rápidos repetidos SHALL NOT provocar zoom da página, e focar um campo de texto SHALL NOT provocar zoom automático.

#### Scenario: Toques rápidos no botão Acertou
- **WHEN** o jogador toca duas vezes rapidamente no botão Acertou
- **THEN** a página não dá zoom e cada toque conta como uma ação

#### Scenario: Foco no campo de nome no iOS
- **WHEN** o jogador toca no campo de nome do jogador no Safari do iOS
- **THEN** a página não dá zoom

### Requirement: Resposta visual a toque e foco
Todo elemento interativo SHALL mostrar resposta visual imediata ao ser pressionado e um indicador de foco visível ao ser focado pelo teclado. Elementos desabilitados SHALL ser visualmente distintos dos habilitados e SHALL NOT reagir ao toque.

#### Scenario: Navegação por teclado
- **WHEN** o jogador navega com a tecla Tab
- **THEN** o elemento focado exibe um contorno de foco visível

#### Scenario: Botão desabilitado
- **WHEN** um botão está desabilitado e é tocado
- **THEN** ele não muda de aparência como se fosse pressionado e nenhuma ação acontece

### Requirement: Animações de transição
A entrada de cada tela, a troca de palavra, o aumento de pontuação e a entrada e saída de jogadores na lista SHALL ser animados. Nenhuma animação SHALL durar mais de 400ms nem bloquear a interação do jogador.

#### Scenario: Nova palavra
- **WHEN** o jogador toca em Acertou ou Pular
- **THEN** a nova palavra aparece com uma animação de entrada
- **AND** o jogador já pode tocar de novo durante a animação

#### Scenario: Troca de tela
- **WHEN** o jogo passa da tela de preparação para o anúncio da dupla
- **THEN** o conteúdo da nova tela entra com uma animação curta

### Requirement: Movimento reduzido
Quando o sistema indica preferência por movimento reduzido, a interface SHALL substituir deslocamentos, escalas e pulsações por mudanças instantâneas ou esmaecimentos curtos, sem perder nenhuma informação.

#### Scenario: Preferência ativada
- **WHEN** o aparelho tem "reduzir movimento" ativado e uma nova palavra é exibida
- **THEN** a palavra aparece sem deslizar nem mudar de escala

#### Scenario: Aviso de tempo com movimento reduzido
- **WHEN** o aparelho tem "reduzir movimento" ativado e faltam 10 segundos ou menos
- **THEN** o aviso de tempo é indicado por cor, sem pulsar
