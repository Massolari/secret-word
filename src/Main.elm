module Main exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, form, h4, h5, p, span, text)
import Html.Attributes exposing (attribute, class, novalidate, style, type_)
import Html.Events exposing (onSubmit)
import Material.Button as Button
import Material.Fab as Fab
import Material.IconButton as IconButton
import Material.LayoutGrid as LayoutGrid
import Material.List as MList
import Material.List.Item as MListItem exposing (ListItem)
import Material.TextField as TextField
import Material.Typography exposing (headline4, headline5)
import Random exposing (Generator)
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
    , zone : Time.Zone
    }


type alias PreparingModel =
    { time : String
    , currentId : Int
    , playerName : String
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
    , toShowJump : Int
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


init : ( Model, Cmd Msg )
init =
    ( { status = Preparing
      , players = Dict.empty
      , preparingModel = initPreparing
      , gameModel = initGame
      , zone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )


initPreparing : PreparingModel
initPreparing =
    { time = ""
    , currentId = 0
    , playerName = ""
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
    , toShowJump = 5
    }



---------
-- Msg --
---------


type Msg
    = Start
    | AdjustTimeZone Time.Zone
    | PreparingMsg PreparingMsg
    | GameMsg GameMsg


type PreparingMsg
    = InputTime String
    | InputPlayer String
    | DeletePlayer Int
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
                playersList =
                    model.players
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

                timeSplitted =
                    String.split ":" preparingModel.time

                minutes =
                    timeSplitted
                        |> List.head
                        |> Maybe.andThen String.toInt
                        |> Maybe.withDefault 0

                seconds =
                    timeSplitted
                        |> List.reverse
                        |> List.head
                        |> Maybe.andThen String.toInt
                        |> Maybe.withDefault 0

                time =
                    ((minutes * 60 * 1000) + (seconds * 1000))
                        |> Time.millisToPosix

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
                , status = Playing
              }
            , Cmd.none
            )

        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        PreparingMsg preparingMsg ->
            updatePreparing preparingMsg model

        GameMsg gameMsg ->
            updateGame gameMsg model


updatePreparing : PreparingMsg -> Model -> ( Model, Cmd Msg )
updatePreparing msg ({ preparingModel } as model) =
    case msg of
        InputTime time ->
            ( { model
                | preparingModel =
                    { preparingModel | time = time }
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
                | players = Dict.remove id model.players
              }
            , Cmd.none
            )

        AddUser ->
            ( { model
                | players = Dict.insert preparingModel.currentId (newPlayer preparingModel.playerName) model.players
                , preparingModel =
                    { preparingModel
                        | playerName = ""
                        , currentId = preparingModel.currentId + 1
                    }
              }
            , Cmd.none
            )


updateGame : GameMsg -> Model -> ( Model, Cmd Msg )
updateGame msg ({ gameModel } as model) =
    case msg of
        Play ->
            ( { model
                | gameModel =
                    { gameModel | status = GamePlaying }
              }
            , newWordIndex gameModel.generator
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

                newToShowJump =
                    if gameModel.toShowJump > 0 then
                        gameModel.toShowJump - 1

                    else
                        gameModel.toShowJump
            in
            ( { model
                | gameModel =
                    { gameModel
                        | currentTime = newCurrent
                        , status = newStatus
                        , toShowJump = newToShowJump
                    }
              }
            , Cmd.none
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
                    }
                , players = playersUpdated
              }
            , Cmd.none
            )

        Correct ->
            ( { model
                | gameModel = { gameModel | score = gameModel.score + 1 }
              }
            , newWordIndex gameModel.generator
            )

        Jump ->
            ( model, newWordIndex gameModel.generator )

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
                                { newGameModel
                                    | word = word
                                    , toShowJump = 5
                                }
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( { model | gameModel = newGameModel }, newWordIndex gameModel.generator )



----------
-- View --
----------


view : Model -> Html Msg
view model =
    case model.status of
        Preparing ->
            viewPreparing model

        Playing ->
            viewGame model


viewPreparing : Model -> Html Msg
viewPreparing ({ preparingModel } as model) =
    LayoutGrid.layoutGrid [ class "center" ]
        [ LayoutGrid.inner []
            [ LayoutGrid.cell [ LayoutGrid.span12 ] [ viewPrepareTime model ]
            , LayoutGrid.cell [ LayoutGrid.span12 ] [ viewPreparePlayers model ]
            , LayoutGrid.cell [ LayoutGrid.span12 ]
                [ Button.raised
                    (Button.config
                        |> Button.setIcon (Just (Button.icon "play_arrow"))
                        |> Button.setTrailingIcon True
                        |> Button.setDisabled ((Dict.size model.players < 2) || String.isEmpty preparingModel.time)
                        |> Button.setOnClick Start
                    )
                    "Começar"
                ]
            ]
        ]


viewPrepareTime : Model -> Html Msg
viewPrepareTime model =
    TextField.filled
        (TextField.config
            |> TextField.setLabel (Just "Tempo")
            |> TextField.setType (Just "time")
            |> TextField.setOnInput (PreparingMsg << InputTime)
            |> TextField.setValue (Just model.preparingModel.time)
        )


viewPreparePlayers : Model -> Html Msg
viewPreparePlayers ({ preparingModel } as model) =
    let
        players =
            Dict.toList model.players
    in
    LayoutGrid.cell []
        [ form [ onSubmit (PreparingMsg AddUser), novalidate True ]
            [ TextField.filled
                (TextField.config
                    |> TextField.setType (Just "text")
                    |> TextField.setLabel (Just "Jogador")
                    |> TextField.setOnInput (PreparingMsg << InputPlayer)
                    |> TextField.setValue (Just preparingModel.playerName)
                )
            , Fab.fab
                (Fab.config
                    |> Fab.setOnClick (PreparingMsg AddUser)
                    |> Fab.setExited (String.isEmpty preparingModel.playerName)
                )
                (Fab.icon "add")
            ]
        , LayoutGrid.inner []
            [ LayoutGrid.cell [ LayoutGrid.span12, class "center", class "list-inline-block" ]
                [ viewList (MList.config |> MList.setInteractive False) <|
                    List.map
                        (\( id, player ) ->
                            MListItem.listItem MListItem.config
                                [ text player.name
                                , MListItem.meta []
                                    [ IconButton.iconButton
                                        (IconButton.config |> IconButton.setOnClick (PreparingMsg <| DeletePlayer id))
                                        (IconButton.icon "delete")
                                    ]
                                ]
                        )
                        players
                ]
            ]
        ]


viewGame : Model -> Html Msg
viewGame ({ gameModel } as model) =
    LayoutGrid.layoutGrid [ class "center" ]
        [ h5 [ headline5 ] [ text <| getTimeString gameModel.currentTime model.zone ]
        , viewGameContent model
        , LayoutGrid.inner []
            [ LayoutGrid.cell [ LayoutGrid.span12, class "center", class "list-inline-block" ]
                [ viewList (MList.config |> MList.setInteractive False |> MList.setTwoLine True) <|
                    List.map
                        (\player ->
                            MListItem.listItem MListItem.config
                                [ MListItem.text []
                                    { primary = [ text player.name ]
                                    , secondary = [ text <| "Pontuação: " ++ String.fromInt player.score ]
                                    }
                                ]
                        )
                        (Dict.values model.players |> List.sortBy .score |> List.reverse)
                ]
            ]
        ]


viewGameContent : Model -> Html Msg
viewGameContent ({ gameModel } as model) =
    case gameModel.status of
        GameShowingPlayers ->
            viewGameShowingPlayers model

        GamePlaying ->
            viewPlaying model

        GameOver ->
            viewGameOver model


viewGameShowingPlayers : Model -> Html Msg
viewGameShowingPlayers { gameModel } =
    let
        ( questioner, guesser ) =
            ( Tuple.second gameModel.questioner
            , Tuple.second gameModel.guesser
            )
    in
    LayoutGrid.cell []
        [ h5 [ headline5 ]
            [ text <| questioner ++ " dá dicas para " ++ guesser ++ " adivinhar a palavra" ]
        , Button.raised
            (Button.config
                |> Button.setIcon (Just (Button.icon "play_arrow"))
                |> Button.setTrailingIcon True
                |> Button.setOnClick (GameMsg Play)
            )
            "Começar"
        ]


viewPlaying : Model -> Html Msg
viewPlaying { gameModel } =
    LayoutGrid.cell []
        [ h4 [ headline4 ]
            [ text gameModel.word ]
        , LayoutGrid.inner []
            [ LayoutGrid.cell [ LayoutGrid.span12 ]
                [ span [ class "action-button" ] [ viewPlayingButton "done" Correct ]
                , span [ class "action-button", class "action-button--skip" ]
                    [ if gameModel.toShowJump == 0 then
                        viewPlayingButton "redo" Jump

                      else
                        Fab.fab
                            (Fab.config |> Fab.setAttributes [ class "fab--disabled", attribute "aria-disabled" "true" ])
                            (Fab.customIcon span [] [ text <| String.fromInt gameModel.toShowJump ])
                    ]
                ]
            ]
        , h5 [ headline5 ] [ text <| "Acertou " ++ String.fromInt gameModel.score ]
        ]


viewPlayingButton : String -> GameMsg -> Html Msg
viewPlayingButton icon msg =
    Fab.fab
        (Fab.config |> Fab.setOnClick (GameMsg msg))
        (Fab.icon icon)


viewGameOver : Model -> Html Msg
viewGameOver { gameModel } =
    let
        ( questioner, guesser ) =
            ( Tuple.second gameModel.questioner
            , Tuple.second gameModel.guesser
            )

        questionerScore =
            gameModel.score // 2
    in
    LayoutGrid.cell []
        [ h5 [ headline5 ]
            [ p [] [ text <| guesser ++ " fez " ++ String.fromInt gameModel.score ++ " pontos" ]
            , p [] [ text <| questioner ++ " fez " ++ String.fromInt questionerScore ++ " pontos" ]
            ]
        , Button.raised
            (Button.config |> Button.setOnClick (GameMsg Restart))
            "Continuar"
        ]


------------
-- Helper --
------------


viewList : MList.Config msg -> List (ListItem msg) -> Html msg
viewList config items =
    case items of
        [] ->
            text ""

        first :: rest ->
            MList.list config first rest


newPlayer : String -> Player
newPlayer name =
    { name = name
    , score = 0
    }


getTimeString : Time.Posix -> Time.Zone -> String
getTimeString time zone =
    let
        pad =
            String.fromInt
                >> String.padLeft 2 '0'

        minutes =
            Time.toMinute zone time
                |> pad

        seconds =
            Time.toSecond zone time
                |> pad
    in
    minutes ++ ":" ++ seconds


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
