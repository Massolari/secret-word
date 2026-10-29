module SetupTest exposing (suite)

{-| Cenários do spec `game-setup`.

Ficam manuais, porque o `elm-test` não roda o navegador:

  - "Adicionar pelo teclado": o foco real no campo depois de adicionar. Aqui
    só se confere que `AddUser` pede o efeito `FocusPlayerName`.
  - "Leitor de tela" (nos dois requisitos): o anúncio feito pelo leitor de
    tela. Aqui se conferem os atributos que ele lê.

-}

import Dict
import Expect
import Harness exposing (..)
import Html.Attributes as Attr
import Main exposing (..)
import ProgramTest
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, checked, class, disabled, id, tag, text)


suite : Test
suite =
    describe "game-setup"
        [ timeTests
        , addTests
        , duplicateTests
        , removeTests
        , emptyTests
        , startRuleTests
        ]


addButton : List Test.Html.Selector.Selector
addButton =
    [ tag "button", attribute (Attr.attribute "aria-label" "Adicionar jogador") ]


startButton : List Test.Html.Selector.Selector
startButton =
    [ tag "button", class "btn--primary", Test.Html.Selector.containing [ text "Começar" ] ]


expectButton : List Test.Html.Selector.Selector -> Bool -> Session -> Expect.Expectation
expectButton selectors isDisabled =
    ProgramTest.expectView (Query.find selectors >> Query.has [ disabled isDisabled ])


timeTests : Test
timeTests =
    describe "Escolha do tempo por presets"
        [ test "Valor inicial" <|
            \_ ->
                start
                    |> ProgramTest.expectView
                        (Query.find [ tag "input", checked True ]
                            >> Query.has [ attribute (Attr.value "60") ]
                        )
        , test "Trocar o tempo" <|
            \_ ->
                start
                    |> ProgramTest.check "" "2min" True
                    |> ProgramTest.ensureView
                        (Query.find [ tag "input", checked True ]
                            >> Query.has [ attribute (Attr.value "120") ]
                        )
                    |> addPlayers [ "Ana", "Beto" ]
                    |> startGame
                    |> ProgramTest.expectViewHas [ class "time-chip", Test.Html.Selector.containing [ text "02:00" ] ]
        ]


addTests : Test
addTests =
    describe "Adicionar jogador"
        [ test "Adicionar pelo teclado" <|
            \_ ->
                start
                    |> addPlayer "Beto"
                    |> addPlayer "  Ana "
                    |> ProgramTest.ensureView
                        (Query.findAll [ class "player__name" ]
                            >> Query.index -1
                            >> Query.has [ text "Ana" ]
                        )
                    |> ProgramTest.expectView
                        (Query.find [ id playerNameInputId ]
                            >> Query.has [ attribute (Attr.value "") ]
                        )
        , test "Adicionar pelo teclado: guarda o nome sem espaços das pontas" <|
            \_ ->
                start
                    |> addPlayer "  Ana "
                    |> ProgramTest.expectModel
                        (.players >> Dict.values >> List.map .name >> Expect.equal [ "Ana" ])
        , test "Adicionar pelo teclado: pede foco no campo" <|
            \_ ->
                typed "Ana"
                    |> update (PreparingMsg AddUser)
                    |> Tuple.second
                    |> Expect.equal FocusPlayerName
        , test "Nome vazio: botão desabilitado" <|
            \_ ->
                start
                    |> typeName "   "
                    |> expectButton addButton True
        , test "Nome vazio: confirmar pelo teclado não adiciona ninguém" <|
            \_ ->
                start
                    |> typeName "   "
                    |> submitPlayer
                    |> ProgramTest.expectModel (.players >> Dict.isEmpty >> Expect.equal True)
        ]


typed : String -> Model
typed text_ =
    let
        model =
            Tuple.first init

        preparing =
            model.preparingModel
    in
    { model | preparingModel = { preparing | playerName = text_ } }


duplicateTests : Test
duplicateTests =
    describe "Nome de jogador repetido"
        [ test "Mesmo nome com outra caixa e espaços" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> typeName "  ANA "
                    |> ProgramTest.ensureViewHas [ id playerNameErrorId, Test.Html.Selector.containing [ text "Já tem um jogador chamado Ana." ] ]
                    |> expectButton addButton True
        , test "Espaços repetidos no meio" <|
            \_ ->
                start
                    |> addPlayer "Ana Maria"
                    |> typeName "ana   maria"
                    |> ProgramTest.ensureViewHas [ text "Já tem um jogador chamado Ana Maria." ]
                    |> expectButton addButton True
        , test "Confirmar pelo teclado com nome repetido" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> typeName "ana"
                    |> submitPlayer
                    |> ProgramTest.ensureView
                        (Query.find [ id playerNameInputId ]
                            >> Query.has [ attribute (Attr.value "ana") ]
                        )
                    |> ProgramTest.ensureViewHas [ text "Já tem um jogador chamado Ana." ]
                    |> ProgramTest.expectModel (.players >> Dict.size >> Expect.equal 1)
        , test "Acento diferencia nomes" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> typeName "Aná"
                    |> ProgramTest.ensureViewHasNot [ text "Já tem um jogador" ]
                    |> expectButton addButton False
        , test "Aviso some ao corrigir o nome" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> typeName "ana"
                    |> ProgramTest.ensureViewHas [ text "Já tem um jogador chamado Ana." ]
                    |> typeName "Bia"
                    |> ProgramTest.ensureViewHasNot [ text "Já tem um jogador" ]
                    |> expectButton addButton False
        , test "Remover o jogador de mesmo nome" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> typeName "ana"
                    |> ProgramTest.clickButton "Remover Ana"
                    |> ProgramTest.ensureViewHasNot [ text "Já tem um jogador" ]
                    |> ProgramTest.advanceTime 200
                    |> submitPlayer
                    |> ProgramTest.expectModel
                        (.players >> Dict.values >> List.map .name >> Expect.equal [ "ana" ])
        , test "Leitor de tela" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> typeName "ana"
                    |> ProgramTest.expectView
                        (Query.find [ id playerNameInputId ]
                            >> Query.has
                                [ attribute (Attr.attribute "aria-invalid" "true")
                                , attribute (Attr.attribute "aria-describedby" playerNameErrorId)
                                ]
                        )
        , test "Sem repetição, o campo não fica inválido" <|
            \_ ->
                start
                    |> typeName "Ana"
                    |> ProgramTest.expectView
                        (Query.find [ id playerNameInputId ]
                            >> Query.hasNot [ attribute (Attr.attribute "aria-invalid" "true") ]
                        )
        ]


removeTests : Test
removeTests =
    describe "Remover jogador"
        [ test "Remover um jogador" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> ProgramTest.clickButton "Remover Ana"
                    |> ProgramTest.ensureViewHas [ class "player--leaving" ]
                    |> ProgramTest.advanceTime 200
                    |> ProgramTest.ensureViewHasNot [ class "player__name", Test.Html.Selector.containing [ text "Ana" ] ]
                    |> ProgramTest.expectModel
                        (.players >> Dict.values >> List.map .name >> Expect.equal [ "Beto" ])
        , test "Leitor de tela" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> ProgramTest.expectViewHas
                        [ tag "button", attribute (Attr.attribute "aria-label" "Remover Ana") ]
        ]


emptyTests : Test
emptyTests =
    describe "Estado vazio da lista"
        [ test "Sem jogadores" <|
            \_ ->
                start
                    |> ProgramTest.expectViewHas [ text "Adicione pelo menos 2 jogadores para começar." ]
        , test "Com jogadores, a mensagem some" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> ProgramTest.expectViewHasNot [ text "Adicione pelo menos 2 jogadores para começar." ]
        ]


startRuleTests : Test
startRuleTests =
    describe "Regra para começar"
        [ test "Sem jogadores" <|
            \_ ->
                start
                    |> ProgramTest.ensureViewHas [ text "Faltam 2 jogadores" ]
                    |> expectButton startButton True
        , test "Um jogador" <|
            \_ ->
                start
                    |> addPlayer "Ana"
                    |> ProgramTest.ensureViewHas [ text "Falta 1 jogador" ]
                    |> expectButton startButton True
        , test "Dois jogadores" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> ProgramTest.ensureView (Query.find startButton >> Query.has [ disabled False ])
                    |> startGame
                    |> ProgramTest.ensureViewHas [ text "Dá as dicas" ]
                    |> ProgramTest.expectViewHas [ class "pair__name", Test.Html.Selector.containing [ text "Ana" ] ]
        , test "Jogador sendo removido não conta" <|
            \_ ->
                start
                    |> addPlayers [ "Ana", "Beto" ]
                    |> ProgramTest.clickButton "Remover Beto"
                    |> ProgramTest.ensureViewHas [ text "Falta 1 jogador" ]
                    |> expectButton startButton True
        ]
