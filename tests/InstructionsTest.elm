module InstructionsTest exposing (suite)

{-| Cenários do spec `game-instructions`.

Ficam manuais, porque o `elm-test` não roda o navegador:

  - "Voltar do navegador depois do botão", "Voltar duas vezes", "Avançar depois
    de voltar", "Partida depois de visitar as instruções" e "Recarregar nas
    instruções": o histórico real do navegador, que fica em `index.js`. Aqui só
    se confere o efeito `InstructionsEntry` e o `BackPressed` mandado à mão.
  - "Instruções mais compridas que a tela" e "Celular pequeno": o layout real.
  - O começo do texto e o topo da preparação: a rolagem real. Aqui só se confere
    que abrir e fechar pedem o efeito `ScrollToTop`.

-}

import Expect
import Harness exposing (..)
import Html.Attributes as Attr
import Main exposing (..)
import ProgramTest
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, checked, id, tag, text)


suite : Test
suite =
    describe "game-instructions"
        [ accessTests
        , contentTests
        , backButtonTests
        , browserBackTests
        , preservedTests
        , guardTests
        , effectTests
        ]



-------------
-- Atalhos --
-------------


preparing : Model
preparing =
    Tuple.first init


inInstructions : Model
inInstructions =
    { preparing | status = Instructions }


locked : Model -> Model
locked model =
    { model | tapGuard = { locked = True, token = 0 } }


releaseTaps : Int -> Effect
releaseTaps token =
    After 500 (ReleaseTaps token)


openInstructions : Session -> Session
openInstructions =
    ProgramTest.clickButton "Como jogar" >> waitGuard


expectOnInstructions : Session -> Expect.Expectation
expectOnInstructions =
    ProgramTest.expectViewHas [ tag "h1", Test.Html.Selector.containing [ text "Como jogar" ] ]


expectOnPreparing : Session -> Expect.Expectation
expectOnPreparing =
    ProgramTest.expectViewHas [ tag "h1", Test.Html.Selector.containing [ text "Palavra Secreta" ] ]


expectText : List String -> Expect.Expectation
expectText snippets =
    start
        |> openInstructions
        |> ProgramTest.expectView
            (Expect.all
                (List.map (\snippet -> Query.has [ text snippet ]) snippets)
            )



--------------
-- Cenários --
--------------


accessTests : Test
accessTests =
    describe "Acesso às instruções pela preparação"
        [ test "Abrir as instruções sem jogadores" <|
            \_ ->
                start
                    |> openInstructions
                    |> expectOnInstructions
        , describe "Partida em andamento" <|
            List.map
                (\( screen, status ) ->
                    test screen <|
                        \_ ->
                            let
                                game =
                                    playingModel.gameModel
                            in
                            fromModel { playingModel | gameModel = { game | status = status } }
                                |> ProgramTest.expectViewHasNot [ tag "button", Test.Html.Selector.containing [ text "Como jogar" ] ]
                )
                [ ( "anúncio da dupla", GameShowingPlayers )
                , ( "rodada", GamePlaying )
                , ( "pausa", GamePaused )
                , ( "fim da rodada", GameOver )
                , ( "confirmar encerramento", GameConfirmingEnd GameOver )
                , ( "fim de jogo", GameFinished )
                ]
        ]


contentTests : Test
contentTests =
    describe "Conteúdo das instruções"
        [ test "Preparação e duplas" <|
            \_ ->
                expectText
                    [ "pelo menos 2 jogadores"
                    , "escolham o tempo da rodada"
                    , "Cada jogador dá dicas para cada um dos outros"
                    , "Passem o celular"
                    ]
        , test "Rodada" <|
            \_ ->
                expectText
                    [ "Acertou para contar 1 acerto e trocar a palavra"
                    , "Pular custa até 5 segundos do tempo restante"
                    , "o custo cai 1 segundo a cada segundo com a palavra na tela"
                    ]
        , test "Pontuação e fim da partida" <|
            \_ ->
                expectText
                    [ "Quem adivinha ganha 1 ponto por acerto"
                    , "Quem dá as dicas ganha metade, arredondada para baixo"
                    , "toquem em Encerrar partida"
                    , "Vence quem tiver mais pontos"
                    ]
        , describe "Leitor de tela"
            [ test "título de nível 1" <|
                \_ ->
                    start
                        |> openInstructions
                        |> expectOnInstructions
            , test "um título para cada parte das regras, na ordem" <|
                \_ ->
                    let
                        titles =
                            [ "O jogo", "Preparação", "Duplas", "Rodada", "Pontos", "Fim da partida" ]
                    in
                    start
                        |> openInstructions
                        |> ProgramTest.expectView
                            (Query.findAll [ tag "h2" ]
                                >> Expect.all
                                    (Query.count (Expect.equal (List.length titles))
                                        :: List.indexedMap
                                            (\position title ->
                                                Query.index position >> Query.has [ text title ]
                                            )
                                            titles
                                    )
                            )
            ]
        ]


backButtonTests : Test
backButtonTests =
    describe "Voltar pelo botão da tela"
        [ test "Voltar pelo botão" <|
            \_ ->
                start
                    |> openInstructions
                    |> ProgramTest.clickButton "Voltar"
                    |> expectOnPreparing
        ]


browserBackTests : Test
browserBackTests =
    describe "Voltar pelo navegador nas instruções"
        [ test "Voltar pelo navegador" <|
            \_ ->
                start
                    |> openInstructions
                    |> ProgramTest.update BackPressed
                    |> ProgramTest.ensureViewHasNot [ Test.Html.Selector.class "dialog" ]
                    |> expectOnPreparing
        ]


preservedTests : Test
preservedTests =
    describe "Preparação preservada"
        [ test "Voltar com a preparação preenchida" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> ProgramTest.check "" "2min" True
                    |> typeName "Cai"
                    |> openInstructions
                    |> ProgramTest.clickButton "Voltar"
                    |> ProgramTest.ensureViewHas [ text "Ana" ]
                    |> ProgramTest.ensureViewHas [ text "Beto" ]
                    |> ProgramTest.ensureView
                        (Query.find [ tag "input", checked True ]
                            >> Query.has [ attribute (Attr.value "120") ]
                        )
                    |> ProgramTest.expectView
                        (Query.find [ id playerNameInputId ]
                            >> Query.has [ attribute (Attr.value "Cai") ]
                        )
        , test "Começar depois de voltar" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> openInstructions
                    |> ProgramTest.clickButton "Voltar"
                    |> waitGuard
                    |> startGame
                    |> ProgramTest.ensureViewHas [ text "Dá as dicas" ]
                    |> ProgramTest.expectModel
                        (.gameModel
                            >> (\game -> ( game.questioner, game.guesser ))
                            >> Expect.equal ( ( 0, "Ana" ), ( 1, "Beto" ) )
                        )
        ]


guardTests : Test
guardTests =
    describe "Proteção contra toques nas instruções"
        [ test "Toque duplo em Voltar" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> openInstructions
                    |> ProgramTest.clickButton "Voltar"
                    |> ProgramTest.clickButton "Começar"
                    |> ProgramTest.advanceTime 1000
                    |> expectOnPreparing
        , test "Toque depois do bloqueio" <|
            \_ ->
                start
                    |> openInstructions
                    |> ProgramTest.clickButton "Voltar"
                    |> expectOnPreparing
        , test "Toque antes do fim do bloqueio é ignorado" <|
            \_ ->
                start
                    |> ProgramTest.clickButton "Como jogar"
                    |> ProgramTest.clickButton "Voltar"
                    |> expectOnInstructions
        , test "Voltar do navegador logo depois de abrir" <|
            \_ ->
                start
                    |> ProgramTest.clickButton "Como jogar"
                    |> ProgramTest.update BackPressed
                    |> expectOnPreparing
        ]



------------
-- Update --
------------


effectTests : Test
effectTests =
    describe "efeito de cada mensagem"
        [ test "OpenInstructions empilha a entrada, rola para o topo e trava os toques" <|
            \_ ->
                update OpenInstructions preparing
                    |> Expect.all
                        [ Tuple.first >> .status >> Expect.equal Instructions
                        , Tuple.second
                            >> Expect.equal
                                (Batch [ Batch [ InstructionsEntry True, ScrollToTop ], releaseTaps 1 ])
                        ]
        , test "CloseInstructions tira a entrada, rola para o topo e trava os toques" <|
            \_ ->
                update CloseInstructions inInstructions
                    |> Expect.all
                        [ Tuple.first >> .status >> Expect.equal Preparing
                        , Tuple.second
                            >> Expect.equal
                                (Batch [ Batch [ InstructionsEntry False, ScrollToTop ], releaseTaps 1 ])
                        ]
        , test "BackPressed nas instruções não mexe na entrada, que o navegador já tirou" <|
            \_ ->
                update BackPressed inInstructions
                    |> Expect.all
                        [ Tuple.first >> .status >> Expect.equal Preparing
                        , Tuple.second >> Expect.equal (Batch [ ScrollToTop, releaseTaps 1 ])
                        ]
        , test "OpenInstructions com os toques travados não faz nada" <|
            \_ ->
                update OpenInstructions (locked preparing)
                    |> Expect.equal ( locked preparing, NoEffect )
        , test "CloseInstructions com os toques travados não faz nada" <|
            \_ ->
                update CloseInstructions (locked inInstructions)
                    |> Expect.equal ( locked inInstructions, NoEffect )
        , test "BackPressed nas instruções vale com os toques travados" <|
            \_ ->
                update BackPressed (locked inInstructions)
                    |> Tuple.first
                    |> .status
                    |> Expect.equal Preparing
        , test "BackPressed na preparação não faz nada" <|
            \_ ->
                update BackPressed preparing
                    |> Expect.equal ( preparing, NoEffect )
        , test "OpenInstructions durante a partida não faz nada" <|
            \_ ->
                update OpenInstructions playingModel
                    |> Expect.equal ( playingModel, NoEffect )
        , test "CloseInstructions fora das instruções não faz nada" <|
            \_ ->
                update CloseInstructions preparing
                    |> Expect.equal ( preparing, NoEffect )
        , test "Start nas instruções não faz nada" <|
            \_ ->
                let
                    model =
                        withPlayers [ "Ana", "Beto" ] inInstructions
                in
                update Start model
                    |> Expect.equal ( model, NoEffect )
        ]
