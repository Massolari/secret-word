module RoundTest exposing (suite)

{-| Cenários do spec `game-round` dirigidos pela interface.

O sorteio simulado pega o primeiro índice livre, então as palavras saem na
ordem da lista: Abacate, Abacateiro, Abacaxi, Ábaco, Abafado, …

Os cenários de relógio, visibilidade e sorteio que dependem de mensagens
que a interface não dispara estão em `UpdateTest`.

Ficam manuais, porque dependem do navegador, de CSS ou do aparelho:

  - "Palavra longa em tela estreita": largura de 320px e quebra de linha.
  - "Acerto": a animação de incremento do contador.
  - "Acerto num Android" e "Navegador sem vibração": a vibração real. Aqui
    se confere o padrão enviado pela porta `vibrate`.
  - "Tela acesa durante a rodada": "Rodada longa", "Fim da rodada", "Rodada
    pausada", "Volta ao app logo depois de começar a rodada", "Rodada termina
    antes de o pedido ser atendido" e "Nova rodada logo depois do fim". O
    wake lock vive em `src/index.js`; aqui se confere a sequência enviada
    pela porta `keepAwake`.
  - "Leitor de tela" do histórico: o anúncio em si. Aqui se confere o texto
    escondido que ele lê.

-}

import Browser.Events
import Dict
import Expect
import Harness exposing (..)
import Html.Attributes as Attr
import Json.Decode as Decode
import Main exposing (..)
import ProgramTest
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (attribute, class, containing, disabled, tag, text)


suite : Test
suite =
    describe "game-round"
        [ orderTests
        , announceTests
        , wordTests
        , correctTests
        , skipTests
        , timerTests
        , endTests
        , historyTests
        , scoreboardTests
        , pausedTests
        , guardTests
        ]



-------------
-- Atalhos --
-------------


{-| Partida de 1min com Ana e Beto, já na rodada.
-}
inRound : Session
inRound =
    start
        |> addPlayers [ "Ana", "Beto" ]
        |> startGame
        |> startRound


{-| Acerto respeitando o bloqueio de 500 ms do botão.
-}
correctAndWait : Session -> Session
correctAndWait =
    correct >> ProgramTest.advanceTime 500


{-| Leva a rodada de 1min até restarem `seconds` segundos.
-}
remaining : Int -> Session -> Session
remaining seconds =
    ticks 1000 ((60 - seconds) * 1000)


{-| Termina a rodada de 1min e espera o bloqueio da tela de fim.
-}
endRound : Session -> Session
endRound =
    ticks 1000 60000 >> waitGuard


expectTimer : String -> Session -> Session
expectTimer shown =
    ProgramTest.ensureView (Query.find [ class "timer__time" ] >> Query.has [ text shown ])


expectWord : String -> Session -> Session
expectWord word =
    ProgramTest.ensureView (Query.find [ class "word" ] >> Query.has [ text word ])


expectCounter : Int -> Session -> Session
expectCounter count =
    ProgramTest.ensureView (Query.find [ class "counter__num" ] >> Query.has [ text (String.fromInt count) ])


expectPair : String -> String -> Session -> Session
expectPair questioner guesser =
    ProgramTest.ensureView
        (Query.findAll [ class "pair__name" ]
            >> Expect.all
                [ Query.count (Expect.equal 2)
                , Query.index 0 >> Query.has [ text questioner ]
                , Query.index 1 >> Query.has [ text guesser ]
                ]
        )


button : String -> List Selector.Selector
button label =
    [ tag "button", containing [ text label ] ]


skipButton : List Selector.Selector
skipButton =
    [ tag "button", attribute (Attr.attribute "aria-label" "Pular (perde 5 segundos)") ]


expectEnabled : List Selector.Selector -> Bool -> Session -> Session
expectEnabled selectors enabled =
    ProgramTest.ensureView
        (Query.find selectors
            >> (if enabled then
                    Query.hasNot [ disabled True ]

                else
                    Query.has [ disabled True ]
               )
        )


history : Session -> (List ( String, WordResult ) -> Expect.Expectation) -> Expect.Expectation
history session expectation =
    ProgramTest.expectModel (.gameModel >> .history >> List.reverse >> expectation) session


hide : Session -> Session
hide =
    ProgramTest.update (GameMsg (VisibilityChanged Browser.Events.Hidden))


done : Session -> Expect.Expectation
done =
    ProgramTest.done



------------------------
-- Ordem e anúncio --
------------------------


orderTests : Test
orderTests =
    describe "Ordem das duplas"
        [ test "Três jogadores" <|
            \_ ->
                let
                    playRound =
                        startRound >> endRound >> continue
                in
                start
                    |> addPlayers [ "Ana", "Beto", "Caio" ]
                    |> startGame
                    |> expectPair "Ana" "Beto"
                    |> playRound
                    |> expectPair "Ana" "Caio"
                    |> playRound
                    |> expectPair "Beto" "Ana"
                    |> playRound
                    |> expectPair "Beto" "Caio"
                    |> playRound
                    |> expectPair "Caio" "Ana"
                    |> playRound
                    |> expectPair "Caio" "Beto"
                    |> playRound
                    |> expectPair "Ana" "Beto"
                    |> done
        ]


announceTests : Test
announceTests =
    describe "Anúncio da dupla"
        [ test "Início da rodada" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> startGame
                    |> ProgramTest.ensureViewHas [ text "Dá as dicas" ]
                    |> ProgramTest.ensureViewHas [ text "Adivinha" ]
                    |> expectPair "Ana" "Beto"
                    |> ProgramTest.advanceTime 5000
                    |> ProgramTest.ensureView (Query.find [ class "time-chip" ] >> Query.has [ text "01:00" ])
                    |> ProgramTest.ensureViewHas [ class "scoreboard" ]
                    |> ProgramTest.expectViewHas (button "Começar rodada")
        ]


wordTests : Test
wordTests =
    describe "Exibição da palavra"
        [ test "Placar oculto" <|
            \_ ->
                inRound
                    |> ProgramTest.ensureViewHas [ class "word" ]
                    |> ProgramTest.expectViewHasNot [ class "scoreboard" ]
        ]



-------------
-- Acertar --
-------------


correctTests : Test
correctTests =
    describe "Acertar"
        [ test "Acerto" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> correctAndWait
                    |> expectCounter 2
                    |> expectWord "Abacaxi"
                    |> correct
                    |> expectCounter 3
                    |> expectWord "Ábaco"
                    |> done
        , test "Toque duplo em Acertou" <|
            \_ ->
                inRound
                    |> correct
                    |> correct
                    |> expectCounter 1
                    |> expectWord "Abacateiro"
                    |> (\session -> history session (Expect.equal [ ( "Abacate", Guessed ) ]))
        , test "Acertou logo depois de Pular" <|
            \_ ->
                inRound
                    |> skip
                    |> correct
                    |> expectCounter 0
                    |> expectWord "Abacateiro"
                    |> done
        , test "Acertos em sequência" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> correct
                    |> expectCounter 2
                    |> expectWord "Abacaxi"
                    |> done
        , test "Aparência durante o bloqueio" <|
            \_ ->
                inRound
                    |> correct
                    |> expectEnabled (button "Acertou") True
                    |> done
        , test "vibra brevemente a cada acerto" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> correctAndWait
                    |> ProgramTest.expectOutgoingPortValues "vibrate"
                        (Decode.list Decode.int)
                        (Expect.equal [ [ 30 ], [ 30 ] ])
        ]



-----------
-- Pular --
-----------


progressOf : Float -> Selector.Selector
progressOf fraction =
    attribute (Attr.attribute "style" ("--progress: " ++ String.fromFloat fraction))


skipTests : Test
skipTests =
    describe "Pular custa tempo"
        [ test "Pular logo que a palavra aparece" <|
            \_ ->
                inRound
                    |> remaining 40
                    |> expectTimer "00:40"
                    |> skip
                    |> expectTimer "00:35"
                    |> ProgramTest.ensureViewHas [ class "timer__fill", progressOf (35000 / 60000) ]
                    |> expectWord "Abacateiro"
                    |> expectCounter 0
                    |> done
        , test "Pulos seguidos" <|
            \_ ->
                inRound
                    |> remaining 30
                    |> skip
                    |> skip
                    |> expectTimer "00:20"
                    |> done
        , test "Custo visível" <|
            \_ ->
                inRound
                    |> ProgramTest.ensureView (Query.find skipButton >> Query.has [ text "−5s" ])
                    |> ProgramTest.expectViewHas skipButton
        , test "Tempo insuficiente" <|
            \_ ->
                inRound
                    |> remaining 5
                    |> expectEnabled skipButton False
                    |> ProgramTest.update (GameMsg Jump)
                    |> expectTimer "00:05"
                    |> expectWord "Abacate"
                    |> done
        , test "Limite exato" <|
            \_ ->
                inRound
                    |> remaining 6
                    |> expectEnabled skipButton True
                    |> skip
                    |> expectTimer "00:01"
                    |> expectEnabled skipButton False
                    |> done
        , test "Pulo leva aos últimos segundos" <|
            \_ ->
                inRound
                    |> remaining 14
                    |> ProgramTest.ensureViewHasNot [ class "timer--alert" ]
                    |> skip
                    |> expectTimer "00:09"
                    |> ProgramTest.expectViewHas [ class "timer--alert" ]
        , test "Pular não vibra" <|
            \_ ->
                inRound
                    |> skip
                    |> ProgramTest.expectOutgoingPortValues "vibrate" (Decode.list Decode.int) (Expect.equal [])
        ]



----------------
-- Cronômetro --
----------------


timerTests : Test
timerTests =
    describe "Cronômetro com progresso"
        [ test "Contagem normal" <|
            \_ ->
                inRound
                    |> expectTimer "01:00"
                    |> ProgramTest.ensureViewHas [ class "timer__fill", progressOf 1 ]
                    |> tick 1000
                    |> expectTimer "00:59"
                    |> tick 2000
                    |> expectTimer "00:58"
                    |> ProgramTest.ensureViewHas [ class "timer__fill", progressOf (58000 / 60000) ]
                    |> done
        , test "Últimos segundos" <|
            \_ ->
                inRound
                    |> remaining 11
                    |> ProgramTest.ensureViewHasNot [ class "timer--alert" ]
                    |> tick 50000
                    |> expectTimer "00:10"
                    |> ProgramTest.expectViewHas [ class "timer--alert" ]
        ]



------------------
-- Fim e pontos --
------------------


endTests : Test
endTests =
    describe "Fim da rodada e pontuação"
        [ test "Cinco acertos" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> correctAndWait
                    |> correctAndWait
                    |> correctAndWait
                    |> correctAndWait
                    |> endRound
                    |> ProgramTest.ensureViewHas [ text "Tempo esgotado!" ]
                    |> ProgramTest.ensureView
                        (Query.findAll [ class "result__row" ]
                            >> Expect.all
                                [ Query.index 0 >> Query.has [ text "Beto", text "+5" ]
                                , Query.index 1 >> Query.has [ text "Ana", text "+2" ]
                                ]
                        )
                    |> continue
                    |> ProgramTest.ensureView
                        (Query.findAll [ class "scoreboard__row" ]
                            >> Expect.all
                                [ Query.index 0 >> Query.has [ text "Beto", text "5 pts" ]
                                , Query.index 1 >> Query.has [ text "Ana", text "2 pts" ]
                                ]
                        )
                    |> done
        , test "Sem ações depois do fim" <|
            \_ ->
                inRound
                    |> endRound
                    |> ProgramTest.ensureViewHasNot (button "Acertou")
                    |> ProgramTest.ensureViewHasNot skipButton
                    |> ProgramTest.update (GameMsg Correct)
                    |> ProgramTest.update (GameMsg Jump)
                    |> ProgramTest.expectModel (.gameModel >> .score >> Expect.equal 0)
        , test "o fim vibra com o padrão longo e libera a tela" <|
            \_ ->
                inRound
                    |> endRound
                    |> ProgramTest.ensureOutgoingPortValues "vibrate"
                        (Decode.list Decode.int)
                        (Expect.equal [ [ 120, 80, 120, 80, 240 ] ])
                    |> ProgramTest.expectOutgoingPortValues "keepAwake" Decode.bool (Expect.equal [ True, False ])
        ]


historyTests : Test
historyTests =
    describe "Histórico de palavras da rodada"
        [ test "Acertos e pulos" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> skip
                    |> ProgramTest.advanceTime 500
                    |> correctAndWait
                    |> endRound
                    |> ProgramTest.expectView
                        (Query.findAll [ class "history__row" ]
                            >> Expect.all
                                [ Query.count (Expect.equal 4)
                                , Query.index 0 >> Query.has [ text "Abacate", class "history__row--guessed" ]
                                , Query.index 1 >> Query.has [ text "Abacateiro", class "history__row--missed" ]
                                , Query.index 2 >> Query.has [ text "Abacaxi", class "history__row--guessed" ]
                                , Query.index 3 >> Query.has [ text "Ábaco", class "history__row--missed" ]
                                ]
                        )
        , test "Contagem coerente com os pontos" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> correctAndWait
                    |> correctAndWait
                    |> correctAndWait
                    |> correctAndWait
                    |> endRound
                    |> ProgramTest.ensureView (Query.find [ class "section__count" ] >> Query.has [ text "5/6" ])
                    |> ProgramTest.expectView
                        (Query.findAll [ class "history__row--guessed" ] >> Query.count (Expect.equal 5))
        , test "Rodada seguinte começa limpa" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> endRound
                    |> continue
                    |> startRound
                    |> endRound
                    |> ProgramTest.expectView
                        (Query.findAll [ class "history__row" ]
                            >> Expect.all
                                [ Query.count (Expect.equal 1)
                                , Query.index 0 >> Query.has [ text "Abacaxi", class "history__row--missed" ]
                                ]
                        )
        , test "Leitor de tela" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> endRound
                    |> ProgramTest.expectView
                        (Query.findAll [ class "history__row" ]
                            >> Expect.all
                                [ Query.index 0 >> Query.has [ class "visually-hidden", text ", acertou" ]
                                , Query.index 1 >> Query.has [ text ", errou" ]
                                ]
                        )
        ]



------------
-- Placar --
------------


{-| Anúncio da primeira dupla com os jogadores e pontos dados.
-}
announcing : List ( String, Int ) -> Session
announcing players =
    let
        model =
            playingModel
                |> withPlayers (List.map Tuple.first players)
                |> withScores (List.map Tuple.second players)

        game =
            model.gameModel
    in
    fromModel { model | status = Playing, gameModel = { game | status = GameShowingPlayers } }


{-| Confere as linhas do placar: nome, posição mostrada (ou troféu) e destaque.
-}
expectScoreboard : List ( String, Maybe Int ) -> Session -> Expect.Expectation
expectScoreboard rows =
    ProgramTest.expectView
        (Query.findAll [ class "scoreboard__row" ]
            >> Expect.all
                (Query.count (Expect.equal (List.length rows))
                    :: List.indexedMap
                        (\index ( name, position ) ->
                            Query.index index
                                >> (case position of
                                        Just number ->
                                            Expect.all
                                                [ Query.has [ text name ]
                                                , Query.hasNot [ class "scoreboard__row--leader" ]
                                                , Query.find [ class "scoreboard__pos" ] >> Query.has [ text (String.fromInt number) ]
                                                ]

                                        Nothing ->
                                            Expect.all
                                                [ Query.has [ text name, class "scoreboard__row--leader" ]
                                                , Query.find [ class "scoreboard__pos" ] >> Query.has [ tag "svg" ]
                                                ]
                                   )
                        )
                        rows
                )
        )


scoreboardTests : Test
scoreboardTests =
    describe "Placar"
        [ test "Ordenação" <|
            \_ ->
                announcing [ ( "Ana", 3 ), ( "Beto", 7 ), ( "Caio", 5 ) ]
                    |> expectScoreboard [ ( "Beto", Nothing ), ( "Caio", Just 2 ), ( "Ana", Just 3 ) ]
        , test "Empate fora da liderança" <|
            \_ ->
                announcing [ ( "Ana", 5 ), ( "Beto", 3 ), ( "Caio", 3 ) ]
                    |> expectScoreboard [ ( "Ana", Nothing ), ( "Beto", Just 2 ), ( "Caio", Just 2 ) ]
        , test "Posição depois de um empate" <|
            \_ ->
                announcing [ ( "Ana", 5 ), ( "Beto", 3 ), ( "Caio", 3 ), ( "Davi", 1 ) ]
                    |> expectScoreboard [ ( "Ana", Nothing ), ( "Beto", Just 2 ), ( "Caio", Just 2 ), ( "Davi", Just 4 ) ]
        , test "Empate na liderança" <|
            \_ ->
                announcing [ ( "Ana", 5 ), ( "Beto", 5 ), ( "Caio", 3 ) ]
                    |> expectScoreboard [ ( "Ana", Nothing ), ( "Beto", Nothing ), ( "Caio", Just 3 ) ]
        , test "Ninguém pontuou" <|
            \_ ->
                announcing [ ( "Ana", 0 ), ( "Beto", 0 ), ( "Caio", 0 ) ]
                    |> expectScoreboard [ ( "Ana", Just 1 ), ( "Beto", Just 1 ), ( "Caio", Just 1 ) ]
        , test "aparece também no fim da rodada" <|
            \_ ->
                inRound
                    |> endRound
                    |> ProgramTest.expectViewHas [ class "scoreboard" ]
        ]



-----------
-- Pausa --
-----------


pausedTests : Test
pausedTests =
    describe "Retomar rodada pausada"
        [ test "Volta para o jogo" <|
            \_ ->
                inRound
                    |> remaining 40
                    |> hide
                    |> ProgramTest.ensureViewHas [ text "Rodada pausada" ]
                    |> ProgramTest.ensureViewHas [ text "Ana dá dicas para Beto" ]
                    |> expectTimer "00:40"
                    |> ProgramTest.ensureViewHasNot [ class "word" ]
                    |> ProgramTest.ensureViewHasNot (button "Acertou")
                    |> ProgramTest.ensureViewHasNot skipButton
                    |> ProgramTest.advanceTime 10000
                    |> ProgramTest.expectViewHas [ text "Rodada pausada" ]
        , test "Retomar" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> correctAndWait
                    |> correctAndWait
                    |> remaining 25
                    |> hide
                    |> waitGuard
                    |> ProgramTest.clickButton "Continuar rodada"
                    |> waitGuard
                    |> expectWord "Ábaco"
                    |> expectCounter 3
                    |> expectTimer "00:25"
                    |> tick 1000
                    |> expectTimer "00:24"
                    |> done
        , test "pausar libera a tela e retomar pede de novo" <|
            \_ ->
                inRound
                    |> hide
                    |> waitGuard
                    |> ProgramTest.clickButton "Continuar rodada"
                    |> ProgramTest.expectOutgoingPortValues "keepAwake" Decode.bool (Expect.equal [ True, False, True ])
        ]



-------------------------------
-- Proteção na troca de tela --
-------------------------------


guardTests : Test
guardTests =
    describe "Proteção contra toques na troca de tela"
        [ test "Acertou no fim do tempo" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> ticks 1000 60000
                    |> ProgramTest.update (GameMsg Correct)
                    |> ProgramTest.clickButton "Continuar"
                    |> ProgramTest.ensureViewHas [ text "Tempo esgotado!" ]
                    |> ProgramTest.ensureView (Query.findAll [ class "history__row--guessed" ] >> Query.count (Expect.equal 1))
                    |> ProgramTest.expectModel
                        (.players >> Dict.values >> List.map .score >> Expect.equal [ 0, 0 ])
        , test "Toque duplo em Continuar" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> correctAndWait
                    |> endRound
                    |> ProgramTest.clickButton "Continuar"
                    |> ProgramTest.clickButton "Começar rodada"
                    |> ProgramTest.ensureViewHas (button "Começar rodada")
                    |> ProgramTest.advanceTime 1000
                    |> ProgramTest.ensureViewHas [ text "Dá as dicas" ]
                    |> ProgramTest.expectModel
                        (.players >> Dict.values >> List.map .score >> Expect.equal [ 1, 2 ])
        , test "Toque duplo em Começar rodada" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> startGame
                    |> ProgramTest.clickButton "Começar rodada"
                    |> correct
                    |> expectCounter 0
                    |> (\session -> history session (Expect.equal []))
        , test "Toque duplo em Começar" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> ProgramTest.clickButton "Começar"
                    |> ProgramTest.clickButton "Começar rodada"
                    |> ProgramTest.advanceTime 1000
                    |> ProgramTest.expectViewHas [ text "Dá as dicas" ]
        , test "Toque duplo em Continuar rodada" <|
            \_ ->
                inRound
                    |> correctAndWait
                    |> remaining 30
                    |> hide
                    |> waitGuard
                    |> ProgramTest.clickButton "Continuar rodada"
                    |> correct
                    |> expectWord "Abacateiro"
                    |> expectCounter 1
                    |> expectTimer "00:30"
                    |> done
        , test "Botão liberado depois do bloqueio" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> ProgramTest.clickButton "Começar"
                    |> ProgramTest.advanceTime 501
                    |> ProgramTest.clickButton "Começar rodada"
                    |> ProgramTest.expectViewHas [ class "word" ]
        , test "Aparência durante o bloqueio" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> ProgramTest.clickButton "Começar"
                    |> expectEnabled (button "Começar rodada") True
                    |> ProgramTest.clickButton "Começar rodada"
                    |> expectEnabled (button "Acertou") True
                    |> expectEnabled skipButton True
                    |> done
        ]
