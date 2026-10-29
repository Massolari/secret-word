module Harness exposing
    ( Session
    , addPlayer
    , addPlayers
    , correct
    , continue
    , fromModel
    , playingModel
    , simulate
    , simulateWith
    , skip
    , start
    , startGame
    , startRound
    , startWith
    , submitPlayer
    , tick
    , ticks
    , typeName
    , waitGuard
    , withPlayers
    , withRemaining
    , withScores
    )

{-| Simulação dos efeitos de `Main` e atalhos para dirigir o app nos testes.

O sorteio pega o primeiro índice do pool (as palavras saem na ordem da
lista) e `GetTime` responde sempre 0: cada rodada começa e retoma com o
relógio dos ticks em 0, e os testes mandam os ticks seguintes à mão.

-}

import Array
import Dict
import Json.Encode as Encode
import Main exposing (..)
import ProgramTest exposing (ProgramTest)
import Set
import SimulatedEffect.Cmd
import SimulatedEffect.Ports
import SimulatedEffect.Process
import SimulatedEffect.Task
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time


type alias Session =
    ProgramTest Model Msg Effect



-------------
-- Efeitos --
-------------


simulate : Effect -> ProgramTest.SimulatedEffect Msg
simulate =
    simulateWith List.head


simulateWith : (List Int -> Maybe Int) -> Effect -> ProgramTest.SimulatedEffect Msg
simulateWith pick effect =
    case effect of
        NoEffect ->
            SimulatedEffect.Cmd.none

        Batch effects ->
            SimulatedEffect.Cmd.batch (List.map (simulateWith pick) effects)

        Vibrate pattern ->
            SimulatedEffect.Ports.send "vibrate" (Encode.list Encode.int pattern)

        KeepAwake keep ->
            SimulatedEffect.Ports.send "keepAwake" (Encode.bool keep)

        FocusPlayerName ->
            SimulatedEffect.Cmd.none

        GetTime ->
            SimulatedEffect.Task.perform (GameMsg << Tick) (SimulatedEffect.Task.succeed (Time.millisToPosix 0))

        DrawWord pool ->
            SimulatedEffect.Task.perform (GameMsg << NewWordIndex)
                (SimulatedEffect.Task.succeed (pick pool |> Maybe.withDefault -1))

        After millis msg ->
            SimulatedEffect.Process.sleep millis
                |> SimulatedEffect.Task.perform (\_ -> msg)



-------------
-- Começar --
-------------


start : Session
start =
    startWith List.head


startWith : (List Int -> Maybe Int) -> Session
startWith pick =
    ProgramTest.createElement
        { init = \_ -> init
        , update = update
        , view = view
        }
        |> ProgramTest.withSimulatedEffects (simulateWith pick)
        |> ProgramTest.start ()


{-| Começa o app num `Model` pronto, sem efeitos iniciais.
-}
fromModel : Model -> Session
fromModel model =
    ProgramTest.createElement
        { init = \_ -> ( model, NoEffect )
        , update = update
        , view = view
        }
        |> ProgramTest.withSimulatedEffects simulate
        |> ProgramTest.start ()



-------------
-- Atalhos --
-------------


typeName : String -> Session -> Session
typeName =
    ProgramTest.fillIn playerNameInputId "Nome do jogador"


{-| Envia o formulário, como o Enter ou o toque no botão de adicionar.
-}
submitPlayer : Session -> Session
submitPlayer =
    ProgramTest.simulateDomEvent (Query.find [ Selector.tag "form" ]) Event.submit


addPlayer : String -> Session -> Session
addPlayer name =
    typeName name >> submitPlayer


addPlayers : List String -> Session -> Session
addPlayers names session =
    List.foldl addPlayer session names


{-| Espera o bloqueio de toques da troca de tela passar.
-}
waitGuard : Session -> Session
waitGuard =
    ProgramTest.advanceTime 500


startGame : Session -> Session
startGame =
    ProgramTest.clickButton "Começar" >> waitGuard


startRound : Session -> Session
startRound =
    ProgramTest.clickButton "Começar rodada" >> waitGuard


correct : Session -> Session
correct =
    ProgramTest.clickButton "Acertou"


skip : Session -> Session
skip =
    ProgramTest.clickButton "Pular (perde 5 segundos)"


continue : Session -> Session
continue =
    ProgramTest.clickButton "Continuar" >> waitGuard


tick : Int -> Session -> Session
tick millis =
    ProgramTest.update (GameMsg (Tick (Time.millisToPosix millis)))


{-| Um tick por segundo de `from` até `to`, inclusive.
-}
ticks : Int -> Int -> Session -> Session
ticks from to session =
    if from > to then
        session

    else
        ticks (from + 1000) to (tick from session)



---------------------
-- Modelos prontos --
---------------------


withPlayers : List String -> Model -> Model
withPlayers names model =
    let
        players =
            List.indexedMap (\id name -> ( id, { name = name, score = 0 } )) names
    in
    { model
        | players = Dict.fromList players
        , preparingModel =
            { initPreparing | currentId = List.length names }
    }


withScores : List Int -> Model -> Model
withScores scores model =
    { model
        | players =
            List.foldl
                (\( id, score ) -> Dict.update id (Maybe.map (\player -> { player | score = score })))
                model.players
                (List.indexedMap Tuple.pair scores)
    }


{-| Rodada em andamento entre os dois primeiros jogadores, com a palavra de
índice 0 na tela e `lastTick` em 0.
-}
playingModel : Model
playingModel =
    let
        model =
            Tuple.first init |> withPlayers [ "Ana", "Beto" ]

        game =
            model.gameModel
    in
    { model
        | status = Playing
        , gameModel =
            { game
                | time = Time.millisToPosix 60000
                , currentTime = Time.millisToPosix 60000
                , playersOrder = Array.fromList [ ( ( 0, "Ana" ), ( 1, "Beto" ) ), ( ( 1, "Beto" ), ( 0, "Ana" ) ) ]
                , questioner = ( 0, "Ana" )
                , guesser = ( 1, "Beto" )
                , status = GamePlaying
                , word = "Abacate"
                , currentIndex = Just 0
                , drawnIndex = Set.singleton 0
                , drawCount = 1
                , lastTick = Just (Time.millisToPosix 0)
            }
    }


withRemaining : Int -> Model -> Model
withRemaining millis ({ gameModel } as model) =
    { model | gameModel = { gameModel | currentTime = Time.millisToPosix millis } }
