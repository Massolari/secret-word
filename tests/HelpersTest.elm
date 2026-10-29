module HelpersTest exposing (suite)

import Array
import Expect
import Fuzz
import Harness exposing (playingModel, withPlayers, withRemaining)
import Main exposing (..)
import Set
import Test exposing (Test, describe, fuzz, test)
import Time


suite : Test
suite =
    describe "Funções auxiliares"
        [ timeTests
        , nameTests
        , wordPoolTests
        , skipTests
        , currentPlayersTests
        ]


timeTests : Test
timeTests =
    describe "getTimeString e secondsLeft"
        [ test "zero mostra 00:00" <|
            \_ -> getTimeString (Time.millisToPosix 0) |> Expect.equal "00:00"
        , test "1 ms restante ainda mostra 00:01" <|
            \_ -> getTimeString (Time.millisToPosix 1) |> Expect.equal "00:01"
        , test "um minuto mostra 01:00" <|
            \_ -> getTimeString (Time.millisToPosix 60000) |> Expect.equal "01:00"
        , test "três minutos mostram 03:00" <|
            \_ -> getTimeString (Time.millisToPosix 180000) |> Expect.equal "03:00"
        , test "arredonda para cima" <|
            \_ -> getTimeString (Time.millisToPosix 39001) |> Expect.equal "00:40"
        , test "secondsLeft arredonda para cima" <|
            \_ ->
                List.map (Time.millisToPosix >> secondsLeft) [ 0, 1, 999, 1000, 1001 ]
                    |> Expect.equal [ 0, 1, 1, 1, 2 ]
        , fuzz (Fuzz.intRange 0 (100 * 60 * 1000 - 1000)) "sempre no formato MM:SS" <|
            \millis ->
                let
                    shown =
                        getTimeString (Time.millisToPosix millis)
                in
                Expect.all
                    [ String.length >> Expect.equal 5
                    , String.slice 2 3 >> Expect.equal ":"
                    , String.toList >> List.filter Char.isDigit >> List.length >> Expect.equal 4
                    ]
                    shown
        , fuzz (Fuzz.intRange 1 (10 * 60 * 1000)) "nunca mostra 00:00 com tempo restante" <|
            \millis ->
                getTimeString (Time.millisToPosix millis) |> Expect.notEqual "00:00"
        ]


typing : String -> List String -> Model
typing text names =
    let
        model =
            Tuple.first init |> withPlayers names

        preparing =
            model.preparingModel
    in
    { model | preparingModel = { preparing | playerName = text } }


nameTests : Test
nameTests =
    describe "normalizeName e duplicatePlayer"
        [ test "remove espaços das pontas e junta os internos" <|
            \_ -> normalizeName "  Ana    Maria " |> Expect.equal "ana maria"
        , test "ignora maiúsculas e mantém acentos" <|
            \_ -> normalizeName "ÁNA" |> Expect.equal "ána"
        , fuzz Fuzz.string "é idempotente" <|
            \name -> normalizeName (normalizeName name) |> Expect.equal (normalizeName name)
        , test "acha \"Ana\" para \"  ANA \"" <|
            \_ ->
                duplicatePlayer (typing "  ANA " [ "Ana" ])
                    |> Maybe.map .name
                    |> Expect.equal (Just "Ana")
        , test "acha \"Ana Maria\" para \"ana   maria\"" <|
            \_ ->
                duplicatePlayer (typing "ana   maria" [ "Ana Maria" ])
                    |> Maybe.map .name
                    |> Expect.equal (Just "Ana Maria")
        , test "acento diferencia nomes" <|
            \_ -> duplicatePlayer (typing "Aná" [ "Ana" ]) |> Expect.equal Nothing
        , test "texto vazio nunca repete" <|
            \_ -> duplicatePlayer (typing "   " [ "Ana" ]) |> Expect.equal Nothing
        , test "ignora jogadores sendo removidos" <|
            \_ ->
                let
                    model =
                        typing "ana" [ "Ana" ]

                    preparing =
                        model.preparingModel
                in
                duplicatePlayer { model | preparingModel = { preparing | removing = Set.singleton 0 } }
                    |> Expect.equal Nothing
        ]


wordPoolTests : Test
wordPoolTests =
    describe "wordPool"
        [ test "devolve só os índices que ainda não saíram" <|
            \_ -> wordPool 5 (Set.fromList [ 0, 2 ]) (Just 2) |> Expect.equal [ 1, 3, 4 ]
        , test "com a lista esgotada, começa um ciclo sem a palavra atual" <|
            \_ -> wordPool 3 (Set.fromList [ 0, 1, 2 ]) (Just 1) |> Expect.equal [ 0, 2 ]
        , test "com uma palavra só, ela pode repetir" <|
            \_ -> wordPool 1 (Set.singleton 0) (Just 0) |> Expect.equal [ 0 ]
        , test "nada sorteado: a lista inteira" <|
            \_ -> wordPool 4 Set.empty Nothing |> Expect.equal [ 0, 1, 2, 3 ]
        , fuzz (Fuzz.list (Fuzz.intRange 0 19)) "nunca devolve índice já sorteado enquanto há restantes" <|
            \drawnList ->
                let
                    drawn =
                        Set.fromList drawnList

                    pool =
                        wordPool 20 drawn (List.head drawnList)
                in
                if Set.size drawn < 20 then
                    pool
                        |> List.filter (\index -> Set.member index drawn)
                        |> Expect.equal []

                else
                    List.length pool |> Expect.equal 19
        ]


skipTests : Test
skipTests =
    describe "canSkip"
        [ test "com 5001 ms pode pular" <|
            \_ -> canSkip (withRemaining 5001 playingModel).gameModel |> Expect.equal True
        , test "com 5000 ms não pode" <|
            \_ -> canSkip (withRemaining 5000 playingModel).gameModel |> Expect.equal False
        ]


currentPlayersTests : Test
currentPlayersTests =
    describe "getCurrentPlayers"
        [ test "devolve a dupla do índice" <|
            \_ ->
                getCurrentPlayers 1 (Array.fromList [ ( ( 0, "Ana" ), ( 1, "Beto" ) ), ( ( 1, "Beto" ), ( 0, "Ana" ) ) ])
                    |> Expect.equal ( ( 1, "Beto" ), ( 0, "Ana" ) )
        , test "índice fora do array devolve a dupla de erro" <|
            \_ ->
                getCurrentPlayers 5 Array.empty
                    |> Expect.equal ( ( 0, "Error" ), ( 0, "Error" ) )
        ]
