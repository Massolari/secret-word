# Spec Delta

## ADDED Requirements

### Requirement: Sorteio de palavras
Sempre que a rodada precisa de uma nova palavra (início da rodada, Acertou ou Pular), o jogo SHALL sortear, com chances iguais, uma palavra da lista que ainda não saiu na partida. Toda palavra da lista SHALL poder ser sorteada. O sorteio SHALL terminar sem travar o app, por mais palavras que já tenham saído.

#### Scenario: Sem repetição na partida
- **WHEN** uma palavra já saiu em qualquer rodada da partida e ainda há palavras da lista que não saíram
- **THEN** essa palavra não é sorteada de novo

#### Scenario: Todas as palavras podem sair
- **WHEN** a partida sorteia palavras até passar pela lista inteira
- **THEN** cada palavra da lista, incluindo a primeira e a última, sai exatamente uma vez antes de qualquer repetição

#### Scenario: Poucas palavras restantes
- **WHEN** resta só uma palavra da lista que ainda não saiu e o jogador toca em Acertou
- **THEN** essa palavra aparece na hora, sem o app congelar

### Requirement: Lista esgotada
Quando todas as palavras da lista já saíram na partida, o jogo SHALL começar um novo ciclo de sorteio com a lista inteira, sem aviso e sem interromper a rodada. A palavra que está na tela SHALL ficar de fora do primeiro sorteio do novo ciclo, a menos que ela seja a única palavra da lista.

#### Scenario: Última palavra acertada
- **WHEN** a palavra na tela é a última da lista que ainda não tinha saído e o jogador toca em Acertou
- **THEN** o contador soma 1 e aparece uma nova palavra, diferente da que estava na tela
- **AND** a rodada e o cronômetro continuam normalmente

#### Scenario: Novo ciclo sem repetição
- **WHEN** um novo ciclo começou
- **THEN** nenhuma palavra se repete até que a lista inteira tenha saído de novo

## MODIFIED Requirements

### Requirement: Acertar
Durante a rodada, tocar em Acertou SHALL somar 1 ao contador de acertos da rodada, exibido na tela, e sortear uma nova palavra conforme o requisito "Sorteio de palavras".

#### Scenario: Acerto
- **WHEN** o contador está em 2 e o jogador toca em Acertou
- **THEN** o contador mostra 3 com uma animação de incremento
- **AND** uma nova palavra é exibida
