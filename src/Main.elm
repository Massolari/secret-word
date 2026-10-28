port module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom
import Dict exposing (Dict)
import Html exposing (Html, button, div, fieldset, form, h1, h2, input, label, legend, li, ol, p, section, span, text, ul)
import Html.Attributes exposing (attribute, autocomplete, checked, class, classList, disabled, id, maxlength, name, novalidate, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Icons
import Process
import Random exposing (Generator)
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
    , generator : Generator Int
    , drawnIndex : List Int
    , history : List ( String, WordResult )
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
    | GameOver


type WordResult
    = Guessed
    | Missed


init : ( Model, Cmd Msg )
init =
    ( { status = Preparing
      , players = Dict.empty
      , preparingModel = initPreparing
      , gameModel = initGame
      }
    , Cmd.none
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
    , generator = randomWord <| Array.length Words.words
    , drawnIndex = []
    , history = []
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


playerNameInputId : String
playerNameInputId =
    "player-name"



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



------------
-- Update --
------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ preparingModel, gameModel } as model) =
    case msg of
        Start ->
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
                        , generator = randomWord <| Array.length Words.words
                    }
                , players = players
                , preparingModel = { preparingModel | removing = Set.empty }
                , status = Playing
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        PreparingMsg preparingMsg ->
            updatePreparing preparingMsg model

        GameMsg gameMsg ->
            updateGame gameMsg model


updatePreparing : PreparingMsg -> Model -> ( Model, Cmd Msg )
updatePreparing msg ({ preparingModel } as model) =
    case msg of
        SelectTime seconds ->
            ( { model
                | preparingModel =
                    { preparingModel | roundSeconds = seconds }
              }
            , Cmd.none
            )

        InputPlayer playerName ->
            ( { model
                | preparingModel =
                    { preparingModel | playerName = playerName }
              }
            , Cmd.none
            )

        DeletePlayer id ->
            ( { model
                | preparingModel =
                    { preparingModel | removing = Set.insert id preparingModel.removing }
              }
            , Process.sleep 200
                |> Task.perform (\_ -> PreparingMsg (RemovePlayer id))
            )

        RemovePlayer id ->
            ( { model
                | players = Dict.remove id model.players
                , preparingModel =
                    { preparingModel | removing = Set.remove id preparingModel.removing }
              }
            , Cmd.none
            )

        AddUser ->
            let
                playerName =
                    String.trim preparingModel.playerName
            in
            if String.isEmpty playerName then
                ( model, Cmd.none )

            else
                ( { model
                    | players = Dict.insert preparingModel.currentId (newPlayer playerName) model.players
                    , preparingModel =
                        { preparingModel
                            | playerName = ""
                            , currentId = preparingModel.currentId + 1
                        }
                  }
                , Task.attempt (always NoOp) (Browser.Dom.focus playerNameInputId)
                )


updateGame : GameMsg -> Model -> ( Model, Cmd Msg )
updateGame msg ({ gameModel } as model) =
    case msg of
        Play ->
            ( { model
                | gameModel =
                    { gameModel | status = GamePlaying }
              }
            , Cmd.batch [ newWordIndex gameModel.generator, keepAwake True ]
            )

        Tick _ ->
            let
                newTime =
                    gameModel.currentTime
                        |> Time.posixToMillis
                        |> (\t -> t - 1000)

                ( newCurrent, newStatus ) =
                    if newTime <= 0 then
                        ( Time.millisToPosix 0, GameOver )

                    else
                        ( Time.millisToPosix newTime, gameModel.status )

                justEnded =
                    newStatus == GameOver && gameModel.status /= GameOver

                newHistory =
                    if justEnded && Maybe.map Tuple.first (List.head gameModel.history) /= Just gameModel.word then
                        ( gameModel.word, Missed ) :: gameModel.history

                    else
                        gameModel.history
            in
            ( { model
                | gameModel =
                    { gameModel
                        | currentTime = newCurrent
                        , status = newStatus
                        , history = newHistory
                    }
              }
            , if justEnded then
                Cmd.batch [ vibrate [ 120, 80, 120, 80, 240 ], keepAwake False ]

              else
                Cmd.none
            )

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
            , Cmd.none
            )

        Correct ->
            ( { model
                | gameModel =
                    { gameModel
                        | score = gameModel.score + 1
                        , history = ( gameModel.word, Guessed ) :: gameModel.history
                    }
              }
            , Cmd.batch [ newWordIndex gameModel.generator, vibrate [ 30 ] ]
            )

        Jump ->
            if canSkip gameModel then
                ( { model
                    | gameModel =
                        { gameModel
                            | currentTime = Time.millisToPosix (Time.posixToMillis gameModel.currentTime - skipCost)
                            , history = ( gameModel.word, Missed ) :: gameModel.history
                        }
                  }
                , newWordIndex gameModel.generator
                )

            else
                ( model, Cmd.none )

        NewWordIndex index ->
            if List.member index gameModel.drawnIndex then
                ( model, newWordIndex gameModel.generator )

            else
                let
                    newDrawnIndex =
                        index :: gameModel.drawnIndex

                    newGameModel =
                        { gameModel | drawnIndex = newDrawnIndex }
                in
                case Array.get index Words.words of
                    Just word ->
                        ( { model
                            | gameModel =
                                { newGameModel | word = word }
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( { model | gameModel = newGameModel }, newWordIndex gameModel.generator )



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
    in
    section [ class "section" ]
        [ h2 [ class "section__title" ]
            [ text "Jogadores"
            , span [ class "section__count" ] [ text <| String.fromInt (Dict.size (activePlayers model)) ]
            ]
        , form [ class "add-player", onSubmit (PreparingMsg AddUser), novalidate True ]
            [ input
                [ id playerNameInputId
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
                []
            , button
                [ type_ "submit"
                , class "btn btn--primary btn--icon"
                , attribute "aria-label" "Adicionar jogador"
                , disabled (String.isEmpty (String.trim preparingModel.playerName))
                ]
                [ Icons.plus ]
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
                [ ( String.fromInt (List.length gameModel.drawnIndex)
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

        topScore =
            players
                |> List.map (Tuple.second >> .score)
                |> List.maximum
                |> Maybe.withDefault 0
    in
    section [ class "section" ]
        [ h2 [ class "section__title" ] [ text "Placar" ]
        , ol [ class "card scoreboard" ] <|
            List.indexedMap
                (\index ( id, player ) ->
                    let
                        leader =
                            topScore > 0 && player.score == topScore
                    in
                    li
                        [ classList [ ( "scoreboard__row", True ), ( "scoreboard__row--leader", leader ) ]
                        , avatarHue id
                        ]
                        [ span [ class "scoreboard__pos" ]
                            [ if leader then
                                Icons.trophy

                              else
                                text <| String.fromInt (index + 1)
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


activePlayers : Model -> Dict Int Player
activePlayers model =
    Dict.filter (\id _ -> not (Set.member id model.preparingModel.removing)) model.players


avatarHue : Int -> Html.Attribute msg
avatarHue id =
    attribute "style" ("--hue: " ++ String.fromInt (modBy 360 (230 + id * 67)))


progressStyle : Float -> Html.Attribute msg
progressStyle progress =
    attribute "style" ("--progress: " ++ String.fromFloat progress)


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
            Time.posixToMillis time // 1000
    in
    pad (totalSeconds // 60) ++ ":" ++ pad (modBy 60 totalSeconds)


getCurrentPlayers : Int -> Array ( ( Int, String ), ( Int, String ) ) -> ( ( Int, String ), ( Int, String ) )
getCurrentPlayers index players =
    Array.get index players
        |> Maybe.withDefault ( ( 0, "Error" ), ( 0, "Error" ) )


randomWord : Int -> Generator Int
randomWord arrayLength =
    Random.int 0 arrayLength


newWordIndex : Generator Int -> Cmd Msg
newWordIndex generator =
    Random.generate (GameMsg << NewWordIndex) generator



------------------
-- Subscription --
------------------


subscriptions : Model -> Sub Msg
subscriptions ({ gameModel } as model) =
    case gameModel.status of
        GamePlaying ->
            Time.every 1000 (GameMsg << Tick)

        _ ->
            Sub.none



----------
-- Main --
----------


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
