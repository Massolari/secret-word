module HarnessTest exposing (suite)

import Harness exposing (..)
import ProgramTest
import Test exposing (Test, describe, test)
import Test.Html.Selector exposing (text)


suite : Test
suite =
    describe "Harness"
        [ test "começa a primeira rodada com a primeira palavra da lista" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> startGame
                    |> startRound
                    |> ProgramTest.ensureViewHas [ text "Ana dá dicas para Beto" ]
                    |> ProgramTest.expectViewHas [ text "Abacate" ]
        , test "ticks de um minuto encerram a rodada de 1min" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> startGame
                    |> startRound
                    |> ticks 1000 60000
                    |> ProgramTest.expectViewHas [ text "Tempo esgotado!" ]
        ]
