# Spec Delta

## ADDED Requirements

### Requirement: Histórico de palavras da rodada
A tela de fim da rodada SHALL mostrar, junto com os pontos, todas as palavras exibidas na rodada, na ordem em que apareceram, cada uma marcada como acerto ou erro. É acerto a palavra em que se tocou Acertou; é erro a palavra pulada ou a que estava na tela quando o tempo acabou. A marcação SHALL usar ícone e cor, não só cor. O histórico SHALL conter apenas as palavras da rodada que terminou.

#### Scenario: Acertos e pulos
- **WHEN** na rodada apareceram Cacto (acertou), Ducha (pulou) e Ferro (acertou), e o tempo acabou com Gaivota na tela
- **THEN** a tela de fim mostra Cacto, Ducha, Ferro e Gaivota, nessa ordem
- **AND** Cacto e Ferro aparecem como acerto, Ducha e Gaivota como erro

#### Scenario: Contagem coerente com os pontos
- **WHEN** a rodada terminou com 5 acertos
- **THEN** o histórico tem exatamente 5 palavras marcadas como acerto

#### Scenario: Rodada seguinte começa limpa
- **WHEN** o jogador toca em Continuar e a próxima rodada termina
- **THEN** o histórico mostra apenas as palavras dessa nova rodada

#### Scenario: Leitor de tela
- **WHEN** um leitor de tela percorre o histórico
- **THEN** cada palavra é anunciada junto com o resultado (acertou ou errou)
