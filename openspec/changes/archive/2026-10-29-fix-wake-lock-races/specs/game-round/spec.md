# Spec Delta

## MODIFIED Requirements

### Requirement: Tela acesa durante a rodada
Enquanto uma rodada está em andamento, o jogo SHALL pedir ao navegador que mantenha a tela acesa, quando houver suporte, e SHALL liberar esse pedido quando a rodada termina. Em nenhum momento SHALL haver mais de um pedido de tela acesa ativo, e depois do fim da rodada nenhum pedido SHALL continuar ativo. Se o pedido falhar ou não houver suporte, o jogo SHALL continuar normalmente.

#### Scenario: Rodada longa
- **WHEN** uma rodada de 3min está em andamento num navegador com suporte
- **THEN** a tela não apaga sozinha durante a rodada

#### Scenario: Fim da rodada
- **WHEN** a rodada termina
- **THEN** o jogo libera o pedido e a tela volta a seguir o tempo de bloqueio do aparelho

#### Scenario: Volta ao app logo depois de começar a rodada
- **WHEN** a rodada começa e o app volta a ficar visível antes de o navegador responder ao pedido de tela acesa
- **THEN** existe um único pedido de tela acesa ativo
- **AND** quando a rodada termina, a tela volta a seguir o tempo de bloqueio do aparelho

#### Scenario: Rodada termina antes de o pedido ser atendido
- **WHEN** a rodada termina antes de o navegador responder ao pedido de tela acesa
- **THEN** assim que o navegador responde, o pedido é liberado e a tela volta a seguir o tempo de bloqueio do aparelho

#### Scenario: Nova rodada logo depois do fim
- **WHEN** uma rodada termina e outra começa antes de o navegador concluir a liberação do pedido anterior
- **THEN** a tela continua acesa durante a nova rodada
- **AND** quando a nova rodada termina, nenhum pedido continua ativo e a tela volta a seguir o tempo de bloqueio do aparelho
