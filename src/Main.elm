module Main exposing (..)

import Adaline exposing (..)
import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Maybe.Extra
import Random exposing (Generator)


main : Program Flags Model Msg
main =
    Browser.document
        { init =
            \{ initialSeed } ->
                ( { lifecycle = Welcome, seed = Random.initialSeed initialSeed }
                , Cmd.none
                )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Flags =
    { initialSeed : Int }


type alias Model =
    { lifecycle : Lifecycle
    , seed : Random.Seed
    }


type Lifecycle
    = Welcome
    | Setup SetupData
    | Training TrainingData
    | Execute ExecuteData


type Msg
    = NoOp
    | WelcomeMsg WelcomeMsg
    | SetupMsg SetupMsg
    | TrainingMsg TrainingMsg
    | ExecuteMsg ExecuteMsg


type WelcomeMsg
    = BeginTraining


type SetupMsg
    = AddEntry
    | AddInput
    | UpdateSetup SetupData
    | AdvanceFromSetup


type TrainingMsg
    = TrainSteps Int
    | AdvanceFromTraining


type ExecuteMsg
    = UpdateExecute ExecuteData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WelcomeMsg welcomeMsg ->
            case model.lifecycle of
                Welcome ->
                    case welcomeMsg of
                        BeginTraining ->
                            let
                                initialData =
                                    { entries = []
                                    , μ = 0
                                    }
                            in
                            ( { model | lifecycle = Setup initialData }
                            , Cmd.none
                            )

                _ ->
                    -- TODO: error feedback
                    ( model
                    , Cmd.none
                    )

        SetupMsg setupMsg ->
            case model.lifecycle of
                Setup currentSetupData ->
                    case setupMsg of
                        AddEntry ->
                            let
                                newEntry =
                                    { inputs = List.repeat (numberOfInputs currentSetupData) Nothing
                                    , desired = Nothing
                                    }

                                newSetupData =
                                    { currentSetupData
                                        | entries = List.append currentSetupData.entries [ newEntry ]
                                    }
                            in
                            ( { model | lifecycle = Setup newSetupData }
                            , Cmd.none
                            )

                        AddInput ->
                            let
                                newSetupData =
                                    { currentSetupData
                                        | entries =
                                            List.map
                                                (\e -> { e | inputs = List.append e.inputs [ Nothing ] })
                                                currentSetupData.entries
                                    }
                            in
                            ( { model | lifecycle = Setup newSetupData }
                            , Cmd.none
                            )

                        AdvanceFromSetup ->
                            let
                                maybeTrainingData =
                                    initialTrainingData currentSetupData
                            in
                            case maybeTrainingData of
                                Just genTrainingData ->
                                    let
                                        ( trainingData, newSeed ) =
                                            Random.step genTrainingData model.seed
                                    in
                                    ( { model
                                        | seed = newSeed
                                        , lifecycle = Training trainingData
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    -- TODO: error feedback
                                    ( model
                                    , Cmd.none
                                    )

                        UpdateSetup newSetupData ->
                            ( { model | lifecycle = Setup newSetupData }
                            , Cmd.none
                            )

                _ ->
                    -- TODO: report error
                    ( model, Cmd.none )

        TrainingMsg trainingMsg ->
            case model.lifecycle of
                Training currentTrainingData ->
                    case trainingMsg of
                        TrainSteps iterations ->
                            let
                                newTrainingData =
                                    advanceTrainingTimes iterations currentTrainingData
                            in
                            ( { model
                                | lifecycle = Training newTrainingData
                              }
                            , Cmd.none
                            )

                        AdvanceFromTraining ->
                            if currentTrainingData.finished then
                                let
                                    executeData =
                                        finishTraining currentTrainingData
                                in
                                ( { model
                                    | lifecycle = Execute executeData
                                  }
                                , Cmd.none
                                )

                            else
                                -- TODO: report error
                                ( model, Cmd.none )

                _ ->
                    -- TODO: report error
                    ( model, Cmd.none )

        ExecuteMsg executeMsg ->
            case model.lifecycle of
                Execute currentExecuteData ->
                    case executeMsg of
                        UpdateExecute newExecuteData ->
                            ( { model | lifecycle = Execute newExecuteData }
                            , Cmd.none
                            )

                _ ->
                    -- TODO: report error
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title =
        case model.lifecycle of
            Welcome ->
                "Adaline"

            Setup _ ->
                "Adaline | setup"

            Training trainingData ->
                "Adaline | training"

            Execute executeData ->
                "Adaline | execute"
    , body =
        [ Element.layout [] <| ui model ]
    }


ui : Model -> Element Msg
ui model =
    let
        content =
            case model.lifecycle of
                Welcome ->
                    welcomeScreen model

                Setup setupData ->
                    setupScreen model setupData

                Training trainingData ->
                    trainingScreen model trainingData

                Execute executeData ->
                    executeScreen model executeData
    in
    column
        [ width fill
        , spacing 16
        ]
        [ el
            [ width fill
            , padding 16
            , Background.color <| rgb255 180 180 180
            , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , Border.color <| rgb255 50 50 50
            ]
          <|
            text "Adaline"
        , el
            [ width (fill |> maximum 900)
            , centerX
            ]
            content
        ]


welcomeScreen : Model -> Element Msg
welcomeScreen model =
    column
        []
        [ Input.button
            [ padding 8
            , Background.color <| rgb255 220 220 220
            , Border.width 2
            , Border.color <| rgb255 50 50 50
            ]
            { onPress = Just <| WelcomeMsg BeginTraining
            , label =
                text "Begin training"
            }
        ]


setupScreen : Model -> SetupData -> Element Msg
setupScreen model setupData =
    column
        [ width fill
        , spacing 16
        ]
        [ row
            []
            [ Input.text
                []
                { onChange =
                    \value ->
                        case String.toFloat value of
                            Just newμ ->
                                SetupMsg <| UpdateSetup { setupData | μ = newμ }

                            Nothing ->
                                NoOp
                , text = String.fromFloat setupData.μ
                , placeholder = Nothing
                , label =
                    Input.labelLeft [] <| text "μ ="
                }
            ]
        , row
            [ spacing 8 ]
            [ Input.button
                (buttonAttrs False)
                { onPress = Just <| SetupMsg AddEntry
                , label = text "Add entry"
                }
            , Input.button
                (buttonAttrs False)
                { onPress = Just <| SetupMsg AddInput
                , label = text "Add input"
                }
            ]
        , column [ spacing 4 ] <| List.map setupEntryUi setupData.entries
        ]


setupEntryUi : SetupDataEntry -> Element Msg
setupEntryUi setupDataEntry =
    row
        [ spacing 8 ]
        [ row
            [ spacing 4 ]
            (List.map (boolUi <| always NoOp) setupDataEntry.inputs)
        , boolUi (always NoOp) setupDataEntry.desired
        ]


trainingScreen : Model -> TrainingData -> Element Msg
trainingScreen model trainingData =
    Element.text "training"


executeScreen : Model -> ExecuteData -> Element Msg
executeScreen model executeData =
    Element.text "execute"


boolUi : (Bool -> Msg) -> Maybe Bool -> Element Msg
boolUi msgFn maybeBool =
    row
        []
        [ Input.button
            ((buttonAttrs <| maybeContains False maybeBool)
                ++ [ width <| px 40
                   , height <| px 50
                   ]
            )
            { onPress = Just <| msgFn False
            , label = el [ centerX, centerY ] <| text "-1"
            }
        , Input.button
            ((buttonAttrs <| maybeContains True maybeBool)
                ++ [ width <| px 40
                   , height <| px 50
                   ]
            )
            { onPress = Just <| msgFn True
            , label = el [ centerX, centerY ] <| text "1"
            }
        ]


maybeContains : a -> Maybe a -> Bool
maybeContains a maybeA =
    case maybeA of
        Just value ->
            a == value

        Nothing ->
            False


buttonAttrs : Bool -> List (Attribute Msg)
buttonAttrs selected =
    [ padding 4
    , Border.width 2
    , Border.color <| rgb255 50 50 50
    , Background.color
        (if selected then
            rgb255 180 180 180

         else
            rgba255 180 180 180 0
        )
    ]
