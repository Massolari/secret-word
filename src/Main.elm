port module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, fieldset, form, h1, h2, input, label, legend, li, ol, p, section, span, text, ul)
import Html.Attributes exposing (attribute, autocomplete, checked, class, classList, disabled, id, maxlength, name, novalidate, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Icons
import Process
import Random
import Set exposing (Set)
import Task
import Time
import Words



-----------
-- Model --
-----------


type alias Model =
    { status : Status
    , players : Dict Int Player
    , preparingModel : PreparingModel
    , gameModel : GameModel
    , tapGuard : TapGuard
    , correctGuard : TapGuard
    }


type alias PreparingModel =
    { roundSeconds : Int
    , currentId : Int
    , playerName : String
    , removing : Set Int
    }


type alias GameModel =
    { time : Time.Posix
    , currentTime : Time.Posix
    , score : Int
    , word : String
    , playersOrder : Array ( ( Int, String ), ( Int, String ) )
    , currentPlayersIndex : Int
    , questioner : ( Int, String )
    , guesser : ( Int, String )
    , status : GameStatus
    , drawnIndex : Set Int
    , currentIndex : Maybe Int
    , drawCount : Int
    , history : List ( String, WordResult )
    , lastTick : Maybe Time.Posix
    }


type alias TapGuard =
    { locked : Bool
    , token : Int
    }


type alias Player =
    { name : String
    , score : Int
    }


type Status
    = Preparing
    | Playing


type GameStatus
    = GameShowingPlayers
    | GamePlaying
    | GamePaused
    | GameOver


type WordResult
    = Guessed
    | Missed


init : ( Model, Effect )
init =
    ( { status = Preparing
      , players = Dict.empty
      , preparingModel = initPreparing
      , gameModel = initGame
      , tapGuard = { locked = False, token = 0 }
      , correctGuard = { locked = False, token = 0 }
      }
    , NoEffect
    )


initPreparing : PreparingModel
initPreparing =
    { roundSeconds = 60
    , currentId = 0
    , playerName = ""
    , removing = Set.empty
    }


initGame : GameModel
initGame =
    { time = Time.millisToPosix 0
    , currentTime = Time.millisToPosix 0
    , score = 0
    , word = ""
    , playersOrder = Array.empty
    , currentPlayersIndex = 0
    , questioner = ( 0, "Não encontrado" )
    , guesser = ( 0, "Não encontrado" )
    , status = GameShowingPlayers
    , drawnIndex = Set.empty
    , currentIndex = Nothing
    , drawCount = 0
    , history = []
    , lastTick = Nothing
    }


skipCost : Int
skipCost =
    5000


canSkip : GameModel -> Bool
canSkip gameModel =
    Time.posixToMillis gameModel.currentTime > skipCost


roundPresets : List ( Int, String )
roundPresets =
    [ ( 30, "30s" )
    , ( 60, "1min" )
    , ( 90, "1min30" )
    , ( 120, "2min" )
    , ( 180, "3min" )
    ]


tapGuardMillis : Float
tapGuardMillis =
    500


playerNameInputId : String
playerNameInputId =
    "player-name"


playerNameErrorId : String
playerNameErrorId =
    "player-name-error"



-----------
-- Ports --
-----------


port vibrate : List Int -> Cmd msg


port keepAwake : Bool -> Cmd msg



---------
-- Msg --
---------


type Msg
    = Start
    | NoOp
    | PreparingMsg PreparingMsg
    | GameMsg GameMsg
    | ReleaseTaps Int
    | ReleaseCorrect Int


type PreparingMsg
    = SelectTime Int
    | InputPlayer String
    | DeletePlayer Int
    | RemovePlayer Int
    | AddUser


type GameMsg
    = Play
    | Tick Time.Posix
    | Restart
    | Correct
    | Jump
    | NewWordIndex Int
    | Resume
    | VisibilityChanged Browser.Events.Visibility



------------
-- Effect --
------------


{-| Efeitos que `update` pede, como dados. `perform` os converte em `Cmd`;
nos testes, o `elm-program-test` os simula.
-}
type Effect
    = NoEffect
    | Batch (List Effect)
    | Vibrate (List Int)
    | KeepAwake Bool
    | FocusPlayerName
    | GetTime
    | DrawWord (List Int)
    | After Float Msg


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map perform effects)

        Vibrate pattern ->
            vibrate pattern

        KeepAwake keep ->
            keepAwake keep

        FocusPlayerName ->
            Task.attempt (always NoOp) (Browser.Dom.focus playerNameInputId)

        GetTime ->
            Task.perform (GameMsg << Tick) Time.now

        DrawWord pool ->
            let
                indexes =
                    Array.fromList pool
            in
            Random.int 0 (Array.length indexes - 1)
                |> Random.map (\position -> Array.get position indexes |> Maybe.withDefault -1)
                |> Random.generate (GameMsg << NewWordIndex)

        After millis msg ->
            Process.sleep millis
                |> Task.perform (\_ -> msg)



------------
-- Update --
------------


update : Msg -> Model -> ( Model, Effect )
update msg ({ preparingModel, gameModel } as model) =
    case msg of
        Start ->
            if model.status /= Preparing then
                ( model, NoEffect )

            else
                let
                    players =
                        activePlayers model

                    playersList =
                        players
                            |> Dict.toList

                    playersOrder =
                        playersList
                            |> Array.fromList
                            |> Array.foldl
                                (\( id, player ) arr ->
                                    let
                                        othersPlayers =
                                            List.filter (Tuple.first >> (/=) id) playersList

                                        playerOrder =
                                            List.map (\( i, p ) -> ( ( id, player.name ), ( i, p.name ) )) othersPlayers
                                                |> Array.fromList
                                    in
                                    Array.append arr playerOrder
                                )
                                Array.empty

                    time =
                        Time.millisToPosix (preparingModel.roundSeconds * 1000)

                    ( questioner, guesser ) =
                        getCurrentPlayers gameModel.currentPlayersIndex playersOrder
                in
                ( { model
                    | gameModel =
                        { gameModel
                            | time = time
                            , currentTime = time
                            , playersOrder = playersOrder
                            , questioner = questioner
                            , guesser = guesser
                        }
                    , players = players
                    , preparingModel = { preparingModel | removing = Set.empty }
                    , status = Playing
                  }
                , NoEffect
                )
                    |> lockTaps

        NoOp ->
            ( model, NoEffect )

        PreparingMsg preparingMsg ->
            updatePreparing preparingMsg model

        GameMsg gameMsg ->
            if acceptsGameMsg gameMsg model then
                updateGame gameMsg model

            else
                ( model, NoEffect )

        ReleaseTaps token ->
            if token == model.tapGuard.token then
                ( { model | tapGuard = { locked = False, token = token } }, NoEffect )

            else
                ( model, NoEffect )

        ReleaseCorrect token ->
            if token == model.correctGuard.token then
                ( { model | correctGuard = { locked = False, token = token } }, NoEffect )

            else
                ( model, NoEffect )


updatePreparing : PreparingMsg -> Model -> ( Model, Effect )
updatePreparing msg ({ preparingModel } as model) =
    case msg of
        SelectTime seconds ->
            ( { model
                | preparingModel =
                    { preparingModel | roundSeconds = seconds }
              }
            , NoEffect
            )

        InputPlayer playerName ->
            ( { model
                | preparingModel =
                    { preparingModel | playerName = playerName }
              }
            , NoEffect
            )

        DeletePlayer id ->
            ( { model
                | preparingModel =
                    { preparingModel | removing = Set.insert id preparingModel.removing }
              }
            , After 200 (PreparingMsg (RemovePlayer id))
            )

        RemovePlayer id ->
            ( { model
                | players = Dict.remove id model.players
                , preparingModel =
                    { preparingModel | removing = Set.remove id preparingModel.removing }
              }
            , NoEffect
            )

        AddUser ->
            let
                playerName =
                    String.trim preparingModel.playerName
            in
            if String.isEmpty playerName || duplicatePlayer model /= Nothing then
                ( model, NoEffect )

            else
                ( { model
                    | players = Dict.insert preparingModel.currentId (newPlayer playerName) model.players
                    , preparingModel =
                        { preparingModel
                            | playerName = ""
                            , currentId = preparingModel.currentId + 1
                        }
                  }
                , FocusPlayerName
                )


acceptsGameMsg : GameMsg -> Model -> Bool
acceptsGameMsg msg model =
    let
        screen =
            case msg of
                Play ->
                    Just GameShowingPlayers

                Correct ->
                    Just GamePlaying

                Jump ->
                    Just GamePlaying

                Restart ->
                    Just GameOver

                Resume ->
                    Just GamePaused

                Tick _ ->
                    Nothing

                NewWordIndex _ ->
                    Nothing

                VisibilityChanged _ ->
                    Nothing
    in
    case screen of
        Just status ->
            not model.tapGuard.locked
                && not (msg == Correct && model.correctGuard.locked)
                && model.status == Playing
                && model.gameModel.status == status

        Nothing ->
            True


updateGame : GameMsg -> Model -> ( Model, Effect )
updateGame msg ({ gameModel } as model) =
    case msg of
        Play ->
            ( { model
                | gameModel =
                    { gameModel | status = GamePlaying, lastTick = Nothing }
              }
            , Batch [ newWordIndex gameModel, GetTime, KeepAwake True ]
            )
                |> lockTaps

        Tick now ->
            case ( gameModel.status, gameModel.lastTick ) of
                ( GamePlaying, Nothing ) ->
                    ( { model | gameModel = { gameModel | lastTick = Just now } }, NoEffect )

                ( GamePlaying, Just previous ) ->
                    let
                        elapsed =
                            max 0 (Time.posixToMillis now - Time.posixToMillis previous)
                    in
                    if elapsed > interruptionMillis then
                        pauseRound model

                    else
                        advanceRound now elapsed model

                _ ->
                    ( model, NoEffect )

        Resume ->
            ( { model | gameModel = { gameModel | status = GamePlaying, lastTick = Nothing } }
            , Batch [ GetTime, KeepAwake True ]
            )
                |> lockTaps

        VisibilityChanged visibility ->
            if gameModel.status == GamePlaying && visibility == Browser.Events.Hidden then
                pauseRound model

            else
                ( model, NoEffect )

        Restart ->
            let
                playersUpdated =
                    Dict.update
                        (Tuple.first gameModel.questioner)
                        (Maybe.map
                            (\player -> { player | score = player.score + (gameModel.score // 2) })
                        )
                        model.players
                        |> Dict.update
                            (Tuple.first gameModel.guesser)
                            (Maybe.map
                                (\player -> { player | score = player.score + gameModel.score })
                            )

                newCurrentIndex =
                    if gameModel.currentPlayersIndex >= (Array.length gameModel.playersOrder - 1) then
                        0

                    else
                        gameModel.currentPlayersIndex + 1

                ( questioner, guesser ) =
                    getCurrentPlayers newCurrentIndex gameModel.playersOrder
            in
            ( { model
                | gameModel =
                    { gameModel
                        | currentPlayersIndex = newCurrentIndex
                        , questioner = questioner
                        , guesser = guesser
                        , score = 0
                        , currentTime = gameModel.time
                        , status = GameShowingPlayers
                        , history = []
                    }
                , players = playersUpdated
              }
            , NoEffect
            )
                |> lockTaps

        Correct ->
            ( holdCorrect
                { model
                    | gameModel =
                        { gameModel
                            | score = gameModel.score + 1
                            , history = ( gameModel.word, Guessed ) :: gameModel.history
                        }
                }
            , Batch [ newWordIndex gameModel, Vibrate [ 30 ] ]
            )

        Jump ->
            if canSkip gameModel then
                ( holdCorrect
                    { model
                        | gameModel =
                            { gameModel
                                | currentTime = Time.millisToPosix (Time.posixToMillis gameModel.currentTime - skipCost)
                                , history = ( gameModel.word, Missed ) :: gameModel.history
                            }
                    }
                , newWordIndex gameModel
                )

            else
                ( model, NoEffect )

        NewWordIndex index ->
            let
                wordsCount =
                    Array.length Words.words

                cycleDone =
                    Set.size gameModel.drawnIndex >= wordsCount

                -- Resposta atrasada de um sorteio pedido antes do último aceito
                stale =
                    if cycleDone then
                        wordsCount > 1 && gameModel.currentIndex == Just index

                    else
                        Set.member index gameModel.drawnIndex
            in
            if stale then
                ( model, newWordIndex gameModel )

            else
                case Array.get index Words.words of
                    Just word ->
                        ( { model
                            | gameModel =
                                { gameModel
                                    | word = word
                                    , currentIndex = Just index
                                    , drawnIndex =
                                        if cycleDone then
                                            Set.singleton index

                                        else
                                            Set.insert index gameModel.drawnIndex
                                    , drawCount = gameModel.drawCount + 1
                                }
                          }
                        , NoEffect
                        )
                            |> lockCorrect

                    Nothing ->
                        ( model, NoEffect )


{-| Intervalo entre ticks acima do qual a página foi congelada ou suspensa
sem avisar, e a rodada deve pausar em vez de descontar esse tempo.
-}
interruptionMillis : Int
interruptionMillis =
    2000


advanceRound : Time.Posix -> Int -> Model -> ( Model, Effect )
advanceRound now elapsed ({ gameModel } as model) =
    let
        newTime =
            max 0 (Time.posixToMillis gameModel.currentTime - elapsed)

        newCurrent =
            Time.millisToPosix newTime

        justEnded =
            newTime <= 0

        newHistory =
            if justEnded && Maybe.map Tuple.first (List.head gameModel.history) /= Just gameModel.word then
                ( gameModel.word, Missed ) :: gameModel.history

            else
                gameModel.history

        newModel =
            { model
                | gameModel =
                    { gameModel
                        | currentTime = newCurrent
                        , status =
                            if justEnded then
                                GameOver

                            else
                                gameModel.status
                        , history = newHistory
                        , lastTick = Just now
                    }
            }
    in
    if justEnded then
        ( newModel, Batch [ Vibrate [ 120, 80, 120, 80, 240 ], KeepAwake False ] )
            |> lockTaps

    else
        ( newModel, NoEffect )


pauseRound : Model -> ( Model, Effect )
pauseRound ({ gameModel } as model) =
    ( { model | gameModel = { gameModel | status = GamePaused, lastTick = Nothing } }
    , KeepAwake False
    )
        |> lockTaps



----------
-- View --
----------


type alias Screen =
    { key : String
    , header : Html Msg
    , main : List (Html Msg)
    , footer : List (Html Msg)
    }


view : Model -> Html Msg
view model =
    let
        screen =
            case model.status of
                Preparing ->
                    viewPreparing model

                Playing ->
                    viewGame model
    in
    Keyed.node "div"
        [ class "app" ]
        [ ( screen.key ++ "-header", Html.header [ class "app__header" ] [ screen.header ] )
        , ( screen.key ++ "-main", Html.main_ [ class "app__main" ] screen.main )
        , ( screen.key ++ "-footer", Html.footer [ class "app__footer" ] screen.footer )
        ]


viewPreparing : Model -> Screen
viewPreparing model =
    let
        playersCount =
            Dict.size (activePlayers model)

        missing =
            2 - playersCount
    in
    { key = "setup"
    , header =
        div [ class "brand" ]
            [ h1 [ class "brand__title" ] [ text "Palavra Secreta" ]
            , p [ class "brand__tagline" ] [ text "Dê dicas, adivinhe, pontue." ]
            ]
    , main =
        [ viewPrepareTime model
        , viewPreparePlayers model
        ]
    , footer =
        [ button
            [ type_ "button"
            , class "btn btn--primary btn--lg btn--block"
            , disabled (missing > 0)
            , onClick Start
            ]
            [ text "Começar", Icons.play ]
        , p [ class "hint", attribute "aria-live" "polite" ]
            [ if missing == 1 then
                text "Falta 1 jogador"

              else if missing > 1 then
                text <| "Faltam " ++ String.fromInt missing ++ " jogadores"

              else
                text ""
            ]
        ]
    }


viewPrepareTime : Model -> Html Msg
viewPrepareTime model =
    fieldset [ class "segmented" ]
        [ legend [ class "section__title" ] [ text "Tempo da rodada" ]
        , div [ class "segmented__options" ] <|
            List.map
                (\( seconds, labelText ) ->
                    label [ class "segmented__option" ]
                        [ input
                            [ type_ "radio"
                            , name "round-time"
                            , class "visually-hidden"
                            , value (String.fromInt seconds)
                            , checked (seconds == model.preparingModel.roundSeconds)
                            , onCheck (\_ -> PreparingMsg (SelectTime seconds))
                            ]
                            []
                        , span [] [ text labelText ]
                        ]
                )
                roundPresets
        ]


viewPreparePlayers : Model -> Html Msg
viewPreparePlayers ({ preparingModel } as model) =
    let
        players =
            Dict.toList model.players

        duplicate =
            duplicatePlayer model

        invalidAttributes =
            case duplicate of
                Just _ ->
                    [ attribute "aria-invalid" "true"
                    , attribute "aria-describedby" playerNameErrorId
                    ]

                Nothing ->
                    []
    in
    section [ class "section" ]
        [ h2 [ class "section__title" ]
            [ text "Jogadores"
            , span [ class "section__count" ] [ text <| String.fromInt (Dict.size (activePlayers model)) ]
            ]
        , div [ class "add-player-group" ]
            [ form [ class "add-player", onSubmit (PreparingMsg AddUser), novalidate True ]
                [ input
                    ([ id playerNameInputId
                     , class "field"
                     , type_ "text"
                     , placeholder "Nome do jogador"
                     , attribute "aria-label" "Nome do jogador"
                     , attribute "enterkeyhint" "done"
                     , attribute "autocapitalize" "words"
                     , autocomplete False
                     , maxlength 24
                     , value preparingModel.playerName
                     , onInput (PreparingMsg << InputPlayer)
                     ]
                        ++ invalidAttributes
                    )
                    []
                , button
                    [ type_ "submit"
                    , class "btn btn--primary btn--icon"
                    , attribute "aria-label" "Adicionar jogador"
                    , disabled (String.isEmpty (String.trim preparingModel.playerName) || duplicate /= Nothing)
                    ]
                    [ Icons.plus ]
                ]
            , p [ id playerNameErrorId, class "field-error", attribute "aria-live" "polite" ]
                (case duplicate of
                    Just player ->
                        [ text ("Já tem um jogador chamado " ++ player.name ++ ".") ]

                    Nothing ->
                        []
                )
            ]
        , if List.isEmpty players then
            p [ class "empty" ]
                [ Icons.users
                , text "Adicione pelo menos 2 jogadores para começar."
                ]

          else
            Keyed.ul [ class "players" ] <|
                List.map (viewPlayer preparingModel.removing) players
        ]


viewPlayer : Set Int -> ( Int, Player ) -> ( String, Html Msg )
viewPlayer removing ( id, player ) =
    let
        leaving =
            Set.member id removing
    in
    ( String.fromInt id
    , li
        [ classList [ ( "player", True ), ( "player--leaving", leaving ) ]
        , avatarHue id
        ]
        [ viewAvatar player.name
        , span [ class "player__name" ] [ text player.name ]
        , button
            [ type_ "button"
            , class "btn btn--ghost btn--icon"
            , attribute "aria-label" ("Remover " ++ player.name)
            , disabled leaving
            , onClick (PreparingMsg <| DeletePlayer id)
            ]
            [ Icons.trash ]
        ]
    )


viewGame : Model -> Screen
viewGame ({ gameModel } as model) =
    case gameModel.status of
        GameShowingPlayers ->
            viewGameShowingPlayers model

        GamePlaying ->
            viewPlaying model

        GamePaused ->
            viewPaused model

        GameOver ->
            viewGameOver model


viewGameShowingPlayers : Model -> Screen
viewGameShowingPlayers ({ gameModel } as model) =
    let
        ( questioner, guesser ) =
            ( Tuple.second gameModel.questioner
            , Tuple.second gameModel.guesser
            )
    in
    { key = "pair-" ++ String.fromInt gameModel.currentPlayersIndex
    , header =
        div [ class "topbar" ]
            [ span [ class "topbar__title" ] [ text "Palavra Secreta" ]
            , span [ class "time-chip", attribute "aria-label" "Tempo da rodada" ]
                [ Icons.clock, text <| getTimeString gameModel.currentTime ]
            ]
    , main =
        [ div [ class "card pair" ]
            [ p [ class "pair__role" ] [ text "Dá as dicas" ]
            , p [ class "pair__name" ] [ text questioner ]
            , span [ class "pair__arrow" ] [ Icons.arrowDown ]
            , p [ class "pair__role" ] [ text "Adivinha" ]
            , p [ class "pair__name" ] [ text guesser ]
            ]
        , viewScoreboard model
        ]
    , footer =
        [ button
            [ type_ "button"
            , class "btn btn--primary btn--lg btn--block"
            , onClick (GameMsg Play)
            ]
            [ text "Começar rodada", Icons.play ]
        ]
    }


viewPlaying : Model -> Screen
viewPlaying { gameModel } =
    { key = "round"
    , header = viewTimer gameModel
    , main =
        [ div [ class "round" ]
            [ p [ class "round__pair" ]
                [ text <| Tuple.second gameModel.questioner ++ " dá dicas para " ++ Tuple.second gameModel.guesser ]
            , Keyed.node "div"
                [ class "word-stage", attribute "aria-live" "polite" ]
                [ ( String.fromInt gameModel.drawCount
                  , p [ class "word" ] [ text gameModel.word ]
                  )
                ]
            , div [ class "counter" ]
                [ Keyed.node "span"
                    []
                    [ ( String.fromInt gameModel.score
                      , span
                            [ classList [ ( "counter__num", True ), ( "counter__num--pop", gameModel.score > 0 ) ] ]
                            [ text <| String.fromInt gameModel.score ]
                      )
                    ]
                , span []
                    [ text <|
                        if gameModel.score == 1 then
                            "acerto"

                        else
                            "acertos"
                    ]
                ]
            ]
        ]
    , footer =
        [ div [ class "actions" ]
            [ viewSkipButton (canSkip gameModel)
            , button
                [ type_ "button"
                , class "btn btn--success btn--lg"
                , onClick (GameMsg Correct)
                ]
                [ Icons.check, text "Acertou" ]
            ]
        ]
    }


viewPaused : Model -> Screen
viewPaused { gameModel } =
    { key = "paused"
    , header = viewTimer gameModel
    , main =
        [ div [ class "round" ]
            [ p [ class "round__pair" ]
                [ text <| Tuple.second gameModel.questioner ++ " dá dicas para " ++ Tuple.second gameModel.guesser ]
            , div [ class "card paused" ]
                [ span [ class "paused__icon" ] [ Icons.clock ]
                , h2 [ class "paused__title" ] [ text "Rodada pausada" ]
                , p [ class "paused__text" ] [ text "O tempo para quando o jogo sai da tela." ]
                ]
            ]
        ]
    , footer =
        [ button
            [ type_ "button"
            , class "btn btn--primary btn--lg btn--block"
            , onClick (GameMsg Resume)
            ]
            [ text "Continuar rodada", Icons.play ]
        ]
    }


viewTimer : GameModel -> Html Msg
viewTimer gameModel =
    let
        remaining =
            Time.posixToMillis gameModel.currentTime

        total =
            Time.posixToMillis gameModel.time

        progress =
            if total > 0 then
                toFloat remaining / toFloat total

            else
                0
    in
    div
        [ classList [ ( "timer", True ), ( "timer--alert", remaining <= 10000 ) ]
        , attribute "role" "timer"
        , attribute "aria-label" "Tempo restante"
        ]
        [ span [ class "timer__time" ] [ text <| getTimeString gameModel.currentTime ]
        , div [ class "timer__bar" ]
            [ div [ class "timer__fill", progressStyle progress ] [] ]
        ]


viewSkipButton : Bool -> Html Msg
viewSkipButton enabled =
    button
        [ type_ "button"
        , class "btn btn--secondary btn--lg"
        , disabled (not enabled)
        , attribute "aria-label" "Pular (perde 5 segundos)"
        , onClick (GameMsg Jump)
        ]
        [ Icons.skip
        , text "Pular"
        , span [ class "skip-cost" ] [ text "−5s" ]
        ]


viewGameOver : Model -> Screen
viewGameOver ({ gameModel } as model) =
    let
        ( ( questionerId, questioner ), ( guesserId, guesser ) ) =
            ( gameModel.questioner, gameModel.guesser )

        questionerScore =
            gameModel.score // 2
    in
    { key = "over"
    , header =
        div []
            [ h1 [ class "over__title" ] [ text "Tempo esgotado!" ]
            , p [ class "over__subtitle" ] [ text "Pontos desta rodada" ]
            ]
    , main =
        [ div [ class "card result" ]
            [ viewResultRow guesserId guesser "adivinhou" gameModel.score
            , viewResultRow questionerId questioner "deu as dicas" questionerScore
            ]
        , viewHistory gameModel.history
        , viewScoreboard model
        ]
    , footer =
        [ button
            [ type_ "button"
            , class "btn btn--primary btn--lg btn--block"
            , onClick (GameMsg Restart)
            ]
            [ text "Continuar" ]
        ]
    }


viewResultRow : Int -> String -> String -> Int -> Html Msg
viewResultRow id playerName role points =
    div [ class "result__row", avatarHue id ]
        [ viewAvatar playerName
        , div [ class "result__who" ]
            [ p [ class "result__name" ] [ text playerName ]
            , p [ class "result__role" ] [ text role ]
            ]
        , span [ class "result__points" ] [ text <| "+" ++ String.fromInt points ]
        ]


viewHistory : List ( String, WordResult ) -> Html Msg
viewHistory history =
    let
        guessed =
            List.length (List.filter (Tuple.second >> (==) Guessed) history)
    in
    section [ class "section" ]
        [ h2 [ class "section__title" ]
            [ text "Palavras"
            , span [ class "section__count" ]
                [ text <| String.fromInt guessed ++ "/" ++ String.fromInt (List.length history) ]
            ]
        , ol [ class "card history" ] <|
            List.map
                (\( word, result ) ->
                    let
                        ( modifier, icon, resultText ) =
                            case result of
                                Guessed ->
                                    ( "history__row--guessed", Icons.check, "acertou" )

                                Missed ->
                                    ( "history__row--missed", Icons.close, "errou" )
                    in
                    li [ class ("history__row " ++ modifier) ]
                        [ span [ class "history__icon" ] [ icon ]
                        , span [ class "history__word" ] [ text word ]
                        , span [ class "visually-hidden" ] [ text <| ", " ++ resultText ]
                        ]
                )
                (List.reverse history)
        ]


viewScoreboard : Model -> Html Msg
viewScoreboard model =
    let
        players =
            Dict.toList model.players
                |> List.sortBy (\( _, player ) -> negate player.score)

        scores =
            List.map (Tuple.second >> .score) players

        topScore =
            scores
                |> List.maximum
                |> Maybe.withDefault 0
    in
    section [ class "section" ]
        [ h2 [ class "section__title" ] [ text "Placar" ]
        , ol [ class "card scoreboard" ] <|
            List.map
                (\( id, player ) ->
                    let
                        leader =
                            topScore > 0 && player.score == topScore

                        position =
                            1 + List.length (List.filter (\score -> score > player.score) scores)
                    in
                    li
                        [ classList [ ( "scoreboard__row", True ), ( "scoreboard__row--leader", leader ) ]
                        , avatarHue id
                        ]
                        [ span [ class "scoreboard__pos" ]
                            [ if leader then
                                Icons.trophy

                              else
                                text <| String.fromInt position
                            ]
                        , viewAvatar player.name
                        , span [ class "scoreboard__name" ] [ text player.name ]
                        , span [ class "scoreboard__score" ] [ text <| String.fromInt player.score ++ " pts" ]
                        ]
                )
                players
        ]


viewAvatar : String -> Html msg
viewAvatar playerName =
    span [ class "avatar", attribute "aria-hidden" "true" ]
        [ text <|
            case String.uncons playerName of
                Just ( initial, _ ) ->
                    String.toUpper (String.fromChar initial)

                Nothing ->
                    ""
        ]



------------
-- Helper --
------------


lockTaps : ( Model, Effect ) -> ( Model, Effect )
lockTaps ( { tapGuard } as model, cmd ) =
    let
        token =
            tapGuard.token + 1
    in
    ( { model | tapGuard = { locked = True, token = token } }
    , Batch
        [ cmd
        , After tapGuardMillis (ReleaseTaps token)
        ]
    )


holdCorrect : Model -> Model
holdCorrect ({ correctGuard } as model) =
    { model | correctGuard = { locked = True, token = correctGuard.token + 1 } }


lockCorrect : ( Model, Effect ) -> ( Model, Effect )
lockCorrect ( model, cmd ) =
    let
        ({ correctGuard } as heldModel) =
            holdCorrect model
    in
    ( heldModel
    , Batch
        [ cmd
        , After tapGuardMillis (ReleaseCorrect correctGuard.token)
        ]
    )


activePlayers : Model -> Dict Int Player
activePlayers model =
    Dict.filter (\id _ -> not (Set.member id model.preparingModel.removing)) model.players


avatarHue : Int -> Html.Attribute msg
avatarHue id =
    attribute "style" ("--hue: " ++ String.fromInt (modBy 360 (230 + id * 67)))


progressStyle : Float -> Html.Attribute msg
progressStyle progress =
    attribute "style" ("--progress: " ++ String.fromFloat progress)


normalizeName : String -> String
normalizeName =
    String.words >> String.join " " >> String.toLower


duplicatePlayer : Model -> Maybe Player
duplicatePlayer model =
    let
        name =
            normalizeName model.preparingModel.playerName
    in
    if String.isEmpty name then
        Nothing

    else
        activePlayers model
            |> Dict.values
            |> List.filter (\player -> normalizeName player.name == name)
            |> List.head


newPlayer : String -> Player
newPlayer name =
    { name = name
    , score = 0
    }


getTimeString : Time.Posix -> String
getTimeString time =
    let
        pad =
            String.fromInt
                >> String.padLeft 2 '0'

        totalSeconds =
            secondsLeft time
    in
    pad (totalSeconds // 60) ++ ":" ++ pad (modBy 60 totalSeconds)


{-| Segundos inteiros que faltam, arredondados para cima: o cronômetro só
mostra 00:00 quando o tempo acabou.
-}
secondsLeft : Time.Posix -> Int
secondsLeft time =
    (Time.posixToMillis time + 999) // 1000


getCurrentPlayers : Int -> Array ( ( Int, String ), ( Int, String ) ) -> ( ( Int, String ), ( Int, String ) )
getCurrentPlayers index players =
    Array.get index players
        |> Maybe.withDefault ( ( 0, "Error" ), ( 0, "Error" ) )


{-| Índices que podem ser sorteados: os que ainda não saíram na partida ou,
com a lista esgotada, todos menos a palavra atual (novo ciclo).
-}
wordPool : Int -> Set Int -> Maybe Int -> List Int
wordPool wordsCount drawn current =
    let
        allIndexes =
            List.range 0 (wordsCount - 1)

        remaining =
            List.filter (\index -> not (Set.member index drawn)) allIndexes

        newCycle =
            List.filter (\index -> Just index /= current) allIndexes
    in
    if not (List.isEmpty remaining) then
        remaining

    else if not (List.isEmpty newCycle) then
        newCycle

    else
        allIndexes


newWordIndex : GameModel -> Effect
newWordIndex gameModel =
    DrawWord (wordPool (Array.length Words.words) gameModel.drawnIndex gameModel.currentIndex)



------------------
-- Subscription --
------------------


subscriptions : Model -> Sub Msg
subscriptions ({ gameModel } as model) =
    case gameModel.status of
        GamePlaying ->
            Sub.batch
                [ Time.every 200 (GameMsg << Tick)
                , Browser.Events.onVisibilityChange (GameMsg << VisibilityChanged)
                ]

        _ ->
            Sub.none



----------
-- Main --
----------


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> Tuple.mapSecond perform init
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond perform
        , subscriptions = subscriptions
        }
