module UpdateTest exposing (suite)

{-| `update` mensagem a mensagem: o efeito exato de cada mensagem e os
cenários de `game-round` que dependem de ticks, da visibilidade da página ou
de respostas do sorteio, que o `elm-program-test` não dispara.
-}

import Array
import Browser.Events exposing (Visibility(..))
import Expect
import Fuzz
import Harness exposing (playingModel, withPlayers, withRemaining)
import Main exposing (..)
import Set
import Test exposing (Test, describe, fuzz, test)
import Time
import Words


suite : Test
suite =
    describe "update"
        [ effectTests
        , guardTokenTests
        , clockTests
        , visibilityTests
        , drawTests
        , exhaustedTests
        ]



-------------
-- Atalhos --
-------------


wordsCount : Int
wordsCount =
    Array.length Words.words


allIndexes : List Int
allIndexes =
    List.range 0 (wordsCount - 1)


withGame : (GameModel -> GameModel) -> Model -> Model
withGame f model =
    { model | gameModel = f model.gameModel }


withStatus : GameStatus -> Model -> Model
withStatus status =
    withGame (\game -> { game | status = status })


withDrawn : List Int -> Maybe Int -> Model -> Model
withDrawn drawn current =
    withGame
        (\game ->
            { game
                | drawnIndex = Set.fromList drawn
                , currentIndex = current
                , word = current |> Maybe.andThen (\index -> Array.get index Words.words) |> Maybe.withDefault game.word
            }
        )


withRoundSeconds : Int -> Model -> Model
withRoundSeconds seconds =
    withGame (\game -> { game | time = Time.millisToPosix (seconds * 1000), currentTime = Time.millisToPosix (seconds * 1000) })


step : Msg -> Model -> Model
step msg model =
    update msg model |> Tuple.first


tickAt : Int -> Model -> Model
tickAt millis =
    step (GameMsg (Tick (Time.millisToPosix millis)))


remainingMillis : Model -> Int
remainingMillis model =
    Time.posixToMillis model.gameModel.currentTime


preparingWith : List String -> Model
preparingWith names =
    Tuple.first init |> withPlayers names


releaseTaps : Int -> Effect
releaseTaps token =
    After 500 (ReleaseTaps token)



-------------
-- Efeitos --
-------------


effectTests : Test
effectTests =
    describe "efeito de cada mensagem"
        [ test "Start trava os toques" <|
            \_ ->
                update Start (preparingWith [ "Ana", "Beto" ])
                    |> Tuple.second
                    |> Expect.equal (Batch [ NoEffect, releaseTaps 1 ])
        , test "Start fora da preparação não faz nada" <|
            \_ ->
                update Start playingModel
                    |> Expect.equal ( playingModel, NoEffect )
        , test "Play sorteia, lê o relógio, pede tela acesa e trava os toques" <|
            \_ ->
                update (GameMsg Play) (playingModel |> withStatus GameShowingPlayers |> withDrawn [] Nothing)
                    |> Tuple.second
                    |> Expect.equal (Batch [ Batch [ DrawWord allIndexes, GetTime, KeepAwake True ], releaseTaps 1 ])
        , test "Correct sorteia sem a palavra atual e vibra" <|
            \_ ->
                update (GameMsg Correct) playingModel
                    |> Tuple.second
                    |> Expect.equal (Batch [ DrawWord (List.drop 1 allIndexes), Vibrate [ 30 ] ])
        , test "Correct trava o próprio botão até a nova palavra" <|
            \_ ->
                step (GameMsg Correct) playingModel
                    |> .correctGuard
                    |> Expect.equal { locked = True, token = 1 }
        , test "Jump só sorteia" <|
            \_ ->
                update (GameMsg Jump) playingModel
                    |> Tuple.second
                    |> Expect.equal (DrawWord (List.drop 1 allIndexes))
        , test "NewWordIndex libera o Acertou depois de 500 ms" <|
            \_ ->
                update (GameMsg (NewWordIndex 1)) playingModel
                    |> Tuple.second
                    |> Expect.equal (Batch [ NoEffect, After 500 (ReleaseCorrect 1) ])
        , test "Restart trava os toques" <|
            \_ ->
                update (GameMsg Restart) (playingModel |> withStatus GameOver)
                    |> Tuple.second
                    |> Expect.equal (Batch [ NoEffect, releaseTaps 1 ])
        , test "Resume lê o relógio, pede tela acesa e trava os toques" <|
            \_ ->
                update (GameMsg Resume) (playingModel |> withStatus GamePaused)
                    |> Tuple.second
                    |> Expect.equal (Batch [ Batch [ GetTime, KeepAwake True ], releaseTaps 1 ])
        , test "fim do tempo vibra, libera a tela e trava os toques" <|
            \_ ->
                update (GameMsg (Tick (Time.millisToPosix 1000))) (playingModel |> withRemaining 1000)
                    |> Tuple.second
                    |> Expect.equal (Batch [ Batch [ Vibrate [ 120, 80, 120, 80, 240 ], KeepAwake False ], releaseTaps 1 ])
        , test "AddUser pede foco no campo" <|
            \_ ->
                let
                    model =
                        Tuple.first init

                    preparing =
                        model.preparingModel
                in
                update (PreparingMsg AddUser) { model | preparingModel = { preparing | playerName = "Ana" } }
                    |> Tuple.second
                    |> Expect.equal FocusPlayerName
        , test "DeletePlayer remove depois de 200 ms" <|
            \_ ->
                update (PreparingMsg (DeletePlayer 0)) (preparingWith [ "Ana" ])
                    |> Tuple.second
                    |> Expect.equal (After 200 (PreparingMsg (RemovePlayer 0)))
        , test "ticks no meio da rodada não têm efeito" <|
            \_ ->
                update (GameMsg (Tick (Time.millisToPosix 1000))) playingModel
                    |> Tuple.second
                    |> Expect.equal NoEffect
        ]


guardTokenTests : Test
guardTokenTests =
    describe "liberação dos bloqueios"
        [ test "ReleaseTaps com o token atual destrava" <|
            \_ ->
                let
                    locked =
                        step Start (preparingWith [ "Ana", "Beto" ])
                in
                step (ReleaseTaps locked.tapGuard.token) locked
                    |> .tapGuard
                    |> .locked
                    |> Expect.equal False
        , test "ReleaseTaps com token velho não destrava" <|
            \_ ->
                let
                    locked =
                        step Start (preparingWith [ "Ana", "Beto" ])
                in
                step (ReleaseTaps (locked.tapGuard.token - 1)) locked
                    |> Expect.equal locked
        , test "ReleaseCorrect com token velho não destrava" <|
            \_ ->
                let
                    locked =
                        playingModel |> step (GameMsg Correct) |> step (GameMsg (NewWordIndex 1))
                in
                step (ReleaseCorrect 1) locked
                    |> Expect.equal locked
        , test "mensagens de tela ficam bloqueadas enquanto os toques estão travados" <|
            \_ ->
                let
                    locked =
                        { playingModel | tapGuard = { locked = True, token = 3 } }
                in
                update (GameMsg Jump) locked
                    |> Expect.equal ( locked, NoEffect )
        ]



----------------
-- Cronômetro --
----------------


clockTests : Test
clockTests =
    describe "Cronômetro com progresso"
        [ fuzz (Fuzz.list (Fuzz.intRange 1 2000)) "Navegador lento" <|
            \intervals ->
                let
                    instants =
                        List.foldl (\interval acc -> (Maybe.withDefault 0 (List.head acc) + interval) :: acc) [ 0 ] intervals
                            |> List.reverse
                            |> List.drop 1

                    final =
                        List.foldl tickAt (playingModel |> withRoundSeconds 30) instants

                    total =
                        List.sum intervals
                in
                if total >= 30000 then
                    final.gameModel.status |> Expect.equal GameOver

                else
                    Expect.all
                        [ .gameModel >> .status >> Expect.equal GamePlaying
                        , remainingMillis >> Expect.equal (30000 - total)
                        ]
                        final
        , test "termina assim que o tempo real passa de 30 s" <|
            \_ ->
                let
                    model =
                        List.foldl tickAt (playingModel |> withRoundSeconds 30) (List.map ((*) 1500) (List.range 1 19))
                in
                Expect.all
                    [ \m -> m.gameModel.status |> Expect.equal GamePlaying
                    , \m -> tickAt 30000 m |> .gameModel |> .status |> Expect.equal GameOver
                    ]
                    model
        , test "Página congelada sem aviso" <|
            \_ ->
                playingModel
                    |> withRemaining 20000
                    |> tickAt 60000
                    |> Expect.all
                        [ .gameModel >> .status >> Expect.equal GamePaused
                        , remainingMillis >> Expect.equal 20000
                        ]
        , test "o primeiro tick depois de Resume não desconta tempo" <|
            \_ ->
                playingModel
                    |> withRemaining 25000
                    |> withStatus GamePaused
                    |> step (GameMsg Resume)
                    |> tickAt 999999
                    |> Expect.all
                        [ .gameModel >> .status >> Expect.equal GamePlaying
                        , remainingMillis >> Expect.equal 25000
                        ]
        , test "o fim do tempo marca a palavra na tela como erro" <|
            \_ ->
                playingModel
                    |> withRemaining 500
                    |> tickAt 1000
                    |> .gameModel
                    |> .history
                    |> Expect.equal [ ( "Abacate", Missed ) ]
        , test "o tempo não fica negativo" <|
            \_ ->
                playingModel
                    |> withRemaining 500
                    |> tickAt 1500
                    |> remainingMillis
                    |> Expect.equal 0
        , test "ticks fora da rodada são ignorados" <|
            \_ ->
                let
                    over =
                        playingModel |> withStatus GameOver
                in
                tickAt 1000 over |> Expect.equal over
        ]


visibilityTests : Test
visibilityTests =
    describe "Pausa ao sair da tela"
        [ test "Tela bloqueada" <|
            \_ ->
                let
                    ( paused, effect ) =
                        playingModel
                            |> withRemaining 40000
                            |> update (GameMsg (VisibilityChanged Hidden))
                in
                Expect.all
                    [ \_ -> paused.gameModel.status |> Expect.equal GamePaused
                    , \_ -> remainingMillis paused |> Expect.equal 40000
                    , \_ -> effect |> Expect.equal (Batch [ KeepAwake False, releaseTaps 1 ])
                    , \_ ->
                        paused
                            |> tickAt 120000
                            |> step (GameMsg (VisibilityChanged Visible))
                            |> Expect.equal paused
                    ]
                    ()
        , test "Troca de aba" <|
            \_ ->
                playingModel
                    |> withRemaining 15000
                    |> step (GameMsg (VisibilityChanged Hidden))
                    |> tickAt 30000
                    |> Expect.all
                        [ .gameModel >> .status >> Expect.equal GamePaused
                        , remainingMillis >> Expect.equal 15000
                        ]
        , test "Fora da rodada: anúncio" <|
            \_ ->
                let
                    announcing =
                        playingModel |> withStatus GameShowingPlayers
                in
                update (GameMsg (VisibilityChanged Hidden)) announcing
                    |> Expect.equal ( announcing, NoEffect )
        , test "Fora da rodada: fim" <|
            \_ ->
                let
                    over =
                        playingModel |> withStatus GameOver
                in
                update (GameMsg (VisibilityChanged Hidden)) over
                    |> Expect.equal ( over, NoEffect )
        , test "Rodada pausada: continua pausada ao ficar visível" <|
            \_ ->
                let
                    paused =
                        playingModel |> withStatus GamePaused
                in
                update (GameMsg (VisibilityChanged Visible)) paused
                    |> Expect.equal ( paused, NoEffect )
        ]



-------------
-- Sorteio --
-------------


drawTests : Test
drawTests =
    describe "Sorteio de palavras"
        [ test "Sem repetição na partida" <|
            \_ ->
                update (GameMsg Correct) (playingModel |> withDrawn [ 0, 1, 2 ] (Just 2))
                    |> Tuple.second
                    |> Expect.equal (Batch [ DrawWord (List.drop 3 allIndexes), Vibrate [ 30 ] ])
        , test "Todas as palavras podem sair: a primeira" <|
            \_ ->
                playingModel
                    |> withDrawn [ 5 ] (Just 5)
                    |> step (GameMsg (NewWordIndex 0))
                    |> .gameModel
                    |> .word
                    |> Expect.equal "Abacate"
        , test "Todas as palavras podem sair: a última" <|
            \_ ->
                playingModel
                    |> step (GameMsg (NewWordIndex (wordsCount - 1)))
                    |> .gameModel
                    |> .word
                    |> Expect.equal "Zumbir"
        , test "Todas as palavras podem sair: o pool inicial tem a lista inteira" <|
            \_ ->
                wordPool wordsCount Set.empty Nothing
                    |> Expect.equal allIndexes
        , test "Poucas palavras restantes" <|
            \_ ->
                let
                    missing =
                        1234

                    model =
                        playingModel |> withDrawn (List.filter ((/=) missing) allIndexes) (Just 0)
                in
                Expect.all
                    [ \m -> update (GameMsg Correct) m |> Tuple.second |> Expect.equal (Batch [ DrawWord [ missing ], Vibrate [ 30 ] ])
                    , \m ->
                        m
                            |> step (GameMsg Correct)
                            |> step (GameMsg (NewWordIndex missing))
                            |> .gameModel
                            |> .currentIndex
                            |> Expect.equal (Just missing)
                    ]
                    model
        , test "resposta atrasada de um sorteio pede outro" <|
            \_ ->
                let
                    model =
                        playingModel |> withDrawn [ 0, 1 ] (Just 1)
                in
                update (GameMsg (NewWordIndex 0)) model
                    |> Expect.equal ( model, DrawWord (List.drop 2 allIndexes) )
        , test "índice inválido é ignorado" <|
            \_ ->
                update (GameMsg (NewWordIndex -1)) playingModel
                    |> Expect.equal ( playingModel, NoEffect )
        , test "Sorteio usa a lista curada" <|
            \_ ->
                playingModel
                    |> step (GameMsg (NewWordIndex 42))
                    |> .gameModel
                    |> .word
                    |> Just
                    |> Expect.equal (Array.get 42 Words.words)
        ]


exhaustedTests : Test
exhaustedTests =
    describe "Lista esgotada"
        [ test "Última palavra acertada" <|
            \_ ->
                let
                    last =
                        777

                    model =
                        playingModel |> withDrawn allIndexes (Just last)

                    ( afterCorrect, effect ) =
                        update (GameMsg Correct) model

                    renewed =
                        step (GameMsg (NewWordIndex 3)) afterCorrect
                in
                Expect.all
                    [ \_ -> effect |> Expect.equal (Batch [ DrawWord (List.filter ((/=) last) allIndexes), Vibrate [ 30 ] ])
                    , \_ -> renewed.gameModel.score |> Expect.equal 1
                    , \_ -> renewed.gameModel.word |> Expect.equal "Ábaco"
                    , \_ -> renewed.gameModel.status |> Expect.equal GamePlaying
                    , \_ -> renewed.gameModel.drawnIndex |> Expect.equal (Set.singleton 3)
                    ]
                    ()
        , test "Novo ciclo sem repetição" <|
            \_ ->
                playingModel
                    |> withDrawn allIndexes (Just 777)
                    |> step (GameMsg (NewWordIndex 3))
                    |> step (ReleaseCorrect 1)
                    |> update (GameMsg Correct)
                    |> Tuple.second
                    |> Expect.equal (Batch [ DrawWord (List.filter ((/=) 3) allIndexes), Vibrate [ 30 ] ])
        , test "com a lista esgotada, a palavra da tela não volta logo" <|
            \_ ->
                let
                    model =
                        playingModel |> withDrawn allIndexes (Just 777)
                in
                update (GameMsg (NewWordIndex 777)) model
                    |> Expect.equal ( model, DrawWord (List.filter ((/=) 777) allIndexes) )
        ]
