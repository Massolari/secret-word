# Spec Delta

## Purpose

Define a partida como um todo, do toque em Começar até ela deixar de existir, e como o jogo evita que ela seja perdida sem querer ao recarregar, fechar ou voltar no navegador.

## ADDED Requirements

### Requirement: Confirmação ao recarregar ou fechar
Do toque em Começar na preparação em diante, recarregar a página, fechar a aba ou navegar para outro endereço SHALL pedir a confirmação do próprio navegador antes de descartar a partida, nos navegadores que oferecem essa confirmação. Na tela de preparação, o jogo SHALL NOT pedir confirmação.

#### Scenario: Recarregar durante a partida
- **WHEN** a partida começou e o jogador recarrega a página
- **THEN** o navegador pergunta se ele quer mesmo sair da página antes de recarregar

#### Scenario: Desistir de recarregar
- **WHEN** o navegador pergunta se o jogador quer sair e ele cancela
- **THEN** a partida continua com os mesmos jogadores, o mesmo placar e a mesma dupla da vez

#### Scenario: Confirmar o recarregamento
- **WHEN** o navegador pergunta se o jogador quer sair e ele confirma
- **THEN** a página recarrega e mostra a tela de preparação vazia

#### Scenario: Recarregar na preparação
- **WHEN** a partida não começou e o jogador recarrega a página
- **THEN** a página recarrega sem pedir confirmação

### Requirement: Confirmação ao voltar durante a partida
Do toque em Começar em diante, o voltar do navegador (botão ou gesto) SHALL NOT sair da página. O jogo SHALL mostrar sobre a tela atual um diálogo "Sair da partida?" que avisa que o placar será perdido e oferece Continuar partida e Sair. Na tela de preparação, o voltar SHALL seguir o comportamento normal do navegador.

#### Scenario: Voltar no anúncio da dupla
- **WHEN** a tela mostra o anúncio da dupla e o jogador usa o voltar do navegador
- **THEN** o jogo continua na página e mostra o diálogo "Sair da partida?" sobre o anúncio

#### Scenario: Voltar no fim da rodada
- **WHEN** a tela mostra o fim da rodada e o jogador usa o voltar do navegador
- **THEN** o jogo continua na página, mostra o diálogo e os pontos da rodada ainda não foram somados ao placar

#### Scenario: Voltar na preparação
- **WHEN** a partida não começou e o jogador usa o voltar do navegador
- **THEN** o navegador volta para a página anterior, sem diálogo

### Requirement: Rodada pausa ao pedir para sair
Se o voltar acontecer com a rodada em andamento, a rodada SHALL pausar antes de o diálogo aparecer, e o tempo SHALL NOT correr enquanto o diálogo estiver aberto.

#### Scenario: Voltar no meio da rodada
- **WHEN** faltam 00:30 na rodada e o jogador usa o voltar do navegador
- **THEN** a rodada pausa com 00:30 e o diálogo "Sair da partida?" aparece sobre a tela de rodada pausada

#### Scenario: Diálogo aberto por um tempo
- **WHEN** a rodada pausou com 00:30 pelo voltar e o diálogo fica aberto por 1 minuto
- **THEN** o cronômetro continua mostrando 00:30

### Requirement: Continuar partida
Continuar partida SHALL fechar o diálogo e deixar o jogo na tela em que estava, com o mesmo estado. Uma rodada pausada pelo voltar SHALL continuar pausada até o toque em Continuar rodada. Usar o voltar do navegador de novo com o diálogo aberto SHALL ter o mesmo efeito de Continuar partida. Depois de fechar o diálogo, o próximo voltar SHALL abrir o diálogo de novo.

#### Scenario: Continuar depois de voltar na rodada
- **WHEN** a rodada pausou com 00:30 pelo voltar e o jogador toca em Continuar partida
- **THEN** o diálogo fecha e a tela mostra "Rodada pausada" com 00:30
- **AND** a rodada só volta a contar com o toque em Continuar rodada

#### Scenario: Voltar com o diálogo aberto
- **WHEN** o diálogo está aberto e o jogador usa o voltar do navegador de novo
- **THEN** o diálogo fecha, o jogo continua na mesma tela e a página não muda

#### Scenario: Proteção continua ativa
- **WHEN** o jogador fechou o diálogo com Continuar partida e usa o voltar do navegador outra vez
- **THEN** o diálogo "Sair da partida?" aparece de novo

### Requirement: Sair da partida
Sair SHALL desligar a proteção e concluir o voltar, deixando a página como o voltar do navegador faria. A saída SHALL NOT pedir uma segunda confirmação.

#### Scenario: Sair pelo diálogo
- **WHEN** o jogador chegou ao jogo vindo de outra página, a partida está em andamento, ele usa o voltar e toca em Sair
- **THEN** o navegador volta para a página de onde ele veio, sem outra pergunta

### Requirement: Diálogo de saída bloqueia a tela de trás
Enquanto o diálogo estiver aberto, os botões da tela de trás SHALL NOT responder a toques nem a teclas. O diálogo SHALL ser anunciado como diálogo por leitores de tela e SHALL receber o foco ao abrir, em Continuar partida. A tecla Esc SHALL ter o mesmo efeito de Continuar partida. Continuar partida SHALL ser a ação de destaque.

#### Scenario: Toque fora do diálogo
- **WHEN** o diálogo está aberto sobre o anúncio da dupla e o jogador toca onde fica Começar rodada
- **THEN** a rodada não começa e o diálogo continua aberto

#### Scenario: Teclado
- **WHEN** o diálogo abre
- **THEN** o foco está em Continuar partida
- **AND** apertar Esc fecha o diálogo e mantém a tela de antes
