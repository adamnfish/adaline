module Main exposing (..)

import Adaline exposing (..)
import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Random exposing (Generator)
import Svg
import Svg.Attributes
import Svg.Events


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


type UiMode
    = InputsMode
    | GridMode


type Lifecycle
    = Welcome
    | Setup String UiMode Config SetupData
    | Training Bool UiMode Config SetupData TrainingData
    | Execute UiMode Config SetupData ExecuteData


type Msg
    = NoOp
    | WelcomeMsg WelcomeMsg
    | SetupMsg SetupMsg
    | TrainingMsg TrainingMsg
    | ExecuteMsg ExecuteMsg
    | SetInputMode UiMode


type WelcomeMsg
    = BeginTraining


type SetupMsg
    = AddEntry
    | AddInput
    | UpdateSetup SetupData
    | Updateμ String
    | AdvanceFromSetup


type TrainingMsg
    = TrainSteps Int
    | ToggleShowWorking
    | AdvanceFromTraining


type ExecuteMsg
    = UpdateExecute ExecuteData
    | ReturnToSetup


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
                                μ =
                                    0.2

                                initialData =
                                    { entries =
                                        [ { desired = Nothing, inputs = [ Nothing ] } ]
                                    , μ = 0.2
                                    }

                                initialConfig =
                                    { trainingCompleteThreshold = 0.00000001
                                    }

                                initialUiMode =
                                    InputsMode
                            in
                            ( { model | lifecycle = Setup (String.fromFloat μ) initialUiMode initialConfig initialData }
                            , Cmd.none
                            )

                _ ->
                    -- TODO: error feedback
                    ( model
                    , Cmd.none
                    )

        SetupMsg setupMsg ->
            case model.lifecycle of
                Setup μInput uiMode config currentSetupData ->
                    case setupMsg of
                        AddEntry ->
                            let
                                emptyElement =
                                    case uiMode of
                                        InputsMode ->
                                            Nothing

                                        GridMode ->
                                            Just False

                                newEntry =
                                    { inputs = List.repeat (numberOfInputs currentSetupData) emptyElement
                                    , desired = Nothing
                                    }

                                newSetupData =
                                    { currentSetupData
                                        | entries = List.append currentSetupData.entries [ newEntry ]
                                    }
                            in
                            ( { model | lifecycle = Setup μInput uiMode config newSetupData }
                            , Cmd.none
                            )

                        AddInput ->
                            let
                                topUpInputsToNextDimension : Int -> List (Maybe Bool)
                                topUpInputsToNextDimension currentCount =
                                    let
                                        nextSquareDimension =
                                            1 + (floor <| sqrt <| toFloat currentCount)

                                        newCount =
                                            nextSquareDimension ^ 2
                                    in
                                    List.repeat (newCount - currentCount) (Just False)

                                -- in grid mode, adding an input means extending the grid size (multiple inputs added)
                                newSetupData =
                                    { currentSetupData
                                        | entries =
                                            List.map
                                                (\e ->
                                                    { e
                                                        | inputs =
                                                            List.append e.inputs <|
                                                                case uiMode of
                                                                    InputsMode ->
                                                                        [ Nothing ]

                                                                    GridMode ->
                                                                        -- TODO insert them more cleverly, to preserve inputs
                                                                        topUpInputsToNextDimension <| List.length e.inputs
                                                    }
                                                )
                                                currentSetupData.entries
                                    }
                            in
                            ( { model | lifecycle = Setup μInput uiMode config newSetupData }
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
                                        , lifecycle = Training False uiMode config currentSetupData trainingData
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    -- TODO: error feedback
                                    ( model
                                    , Cmd.none
                                    )

                        UpdateSetup newSetupData ->
                            ( { model | lifecycle = Setup μInput uiMode config newSetupData }
                            , Cmd.none
                            )

                        Updateμ newμ ->
                            let
                                newSetupData =
                                    case String.toFloat newμ of
                                        Just μ ->
                                            { currentSetupData | μ = μ }

                                        Nothing ->
                                            currentSetupData
                            in
                            ( { model | lifecycle = Setup newμ uiMode config newSetupData }
                            , Cmd.none
                            )

                _ ->
                    -- TODO: report error
                    ( model, Cmd.none )

        TrainingMsg trainingMsg ->
            case model.lifecycle of
                Training showWorking uiMode config setupData currentTrainingData ->
                    case trainingMsg of
                        TrainSteps iterations ->
                            let
                                newTrainingData =
                                    advanceTrainingTimes iterations config currentTrainingData
                            in
                            ( { model
                                | lifecycle = Training showWorking uiMode config setupData newTrainingData
                              }
                            , Cmd.none
                            )

                        ToggleShowWorking ->
                            ( { model
                                | lifecycle = Training (not showWorking) uiMode config setupData currentTrainingData
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
                                    | lifecycle = Execute uiMode config setupData executeData
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
                Execute uiMode config setupData currentExecuteData ->
                    case executeMsg of
                        UpdateExecute newExecuteData ->
                            ( { model | lifecycle = Execute uiMode config setupData newExecuteData }
                            , Cmd.none
                            )

                        ReturnToSetup ->
                            ( { model | lifecycle = Setup (String.fromFloat setupData.μ) uiMode config setupData }
                            , Cmd.none
                            )

                _ ->
                    -- TODO: report error
                    ( model, Cmd.none )

        SetInputMode uiMode ->
            case model.lifecycle of
                Welcome ->
                    -- TODO: report error, this is unexpected
                    ( model, Cmd.none )

                Setup μInput _ config setupData ->
                    let
                        updatedSetupData =
                            if uiMode == GridMode then
                                -- we need to set all the maybe input values to false for the grid UI
                                { setupData
                                    | entries =
                                        List.map
                                            (\e -> { e | inputs = List.map (\_ -> Just False) e.inputs })
                                            setupData.entries
                                }

                            else
                                setupData
                    in
                    ( { model | lifecycle = Setup μInput uiMode config updatedSetupData }
                    , Cmd.none
                    )

                Training showWorking _ config setupData trainingData ->
                    -- TODO: report error, this is unexpected
                    ( model, Cmd.none )

                Execute _ config setupData executeData ->
                    -- TODO: report error, this is unexpected
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title =
        case model.lifecycle of
            Welcome ->
                "Adaline"

            Setup _ _ _ _ ->
                "Adaline | setup"

            Training _ _ _ _ _ ->
                "Adaline | training"

            Execute _ _ _ _ ->
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

                Setup μInput uiMode config setupData ->
                    setupScreen model uiMode μInput setupData

                Training showWorking uiMode config setupData trainingData ->
                    trainingScreen model uiMode showWorking trainingData

                Execute uiMode config setupData executeData ->
                    executeScreen model uiMode setupData executeData
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
            , paddingXY 16 0
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


setupScreen : Model -> UiMode -> String -> SetupData -> Element Msg
setupScreen model uiMode μInput setupData =
    column
        [ width fill
        , spacing 16
        ]
        [ row
            [ width fill
            , spacing 12
            ]
            [ Input.text
                [ Background.color <|
                    if String.fromFloat setupData.μ /= μInput then
                        rgb255 230 180 180

                    else
                        rgba255 255 255 255 0
                , width <| px 200
                ]
                { onChange = \value -> SetupMsg <| Updateμ value
                , text = μInput
                , placeholder = Nothing
                , label =
                    Input.labelLeft
                        []
                    <|
                        text "μ ="
                }
            , Input.button
                ([ alignRight ] ++ buttonAttrs False)
                { onPress =
                    Just <|
                        SetInputMode <|
                            case uiMode of
                                InputsMode ->
                                    GridMode

                                GridMode ->
                                    InputsMode
                , label =
                    text <|
                        case uiMode of
                            InputsMode ->
                                "Switch to grid"

                            GridMode ->
                                "Switch to inputs"
                }
            , Input.button
                (List.append
                    (buttonAttrs False)
                    [ alignRight ]
                )
                { onPress = Just <| SetupMsg AdvanceFromSetup
                , label = text "Finish setup"
                }
            ]
        , row
            [ spacing 12
            , width fill
            ]
            [ el
                [ width <| px 100
                , Border.widthEach
                    { bottom = 6, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 210 210 210
                ]
              <|
                text "desired"
            , el
                [ width fill
                , Border.widthEach
                    { bottom = 6, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 210 210 210
                ]
              <|
                text "inputs"
            ]
        , column
            [ spacing 4
            , width fill
            ]
          <|
            case uiMode of
                InputsMode ->
                    List.indexedMap (setupInputEntryUi setupData) setupData.entries

                GridMode ->
                    List.indexedMap (setupGridEntryUI setupData) setupData.entries
        , row
            [ spacing 16
            , width fill
            ]
            [ Input.button
                (buttonAttrs False)
                { onPress = Just <| SetupMsg AddEntry
                , label = text "Add entry"
                }
            ]
        ]


setupGridEntryUI : SetupData -> Int -> SetupDataEntry -> Element Msg
setupGridEntryUI setupData entryIndex setupDataEntry =
    let
        dimensions =
            ceiling <| sqrt <| toFloat <| List.length setupDataEntry.inputs

        indexedInputs =
            List.Extra.greedyGroupsOf dimensions <| List.indexedMap Tuple.pair setupDataEntry.inputs
    in
    row
        [ spacing 12
        , width fill
        ]
        [ boolInputUi
            (\desired ->
                case List.Extra.getAt entryIndex setupData.entries of
                    Just entry ->
                        let
                            newEntry =
                                { entry | desired = Just desired }
                        in
                        SetupMsg <|
                            UpdateSetup { setupData | entries = List.Extra.setAt entryIndex newEntry setupData.entries }

                    Nothing ->
                        NoOp
            )
            setupDataEntry.desired
        , column
            []
          <|
            List.map
                (\inputRow ->
                    row
                        []
                    <|
                        List.map
                            (\( inputIndex, maybeCurrentValue ) ->
                                let
                                    msgFn =
                                        \newValue ->
                                            case List.Extra.getAt entryIndex setupData.entries of
                                                Just entry ->
                                                    let
                                                        newEntry =
                                                            { entry | inputs = List.Extra.setAt inputIndex (Just newValue) entry.inputs }
                                                    in
                                                    SetupMsg <|
                                                        UpdateSetup { setupData | entries = List.Extra.setAt entryIndex newEntry setupData.entries }

                                                Nothing ->
                                                    NoOp
                                in
                                Input.button
                                    [ width <| px 16
                                    , height <| px 16
                                    , Border.width 1
                                    , Background.color <|
                                        case maybeCurrentValue of
                                            Just True ->
                                                rgb255 180 180 180

                                            Just False ->
                                                rgb255 255 255 255

                                            Nothing ->
                                                rgb255 220 220 220
                                    ]
                                    { onPress = Just <| msgFn <| not <| Maybe.withDefault False maybeCurrentValue
                                    , label = Element.none
                                    }
                            )
                            inputRow
                )
                indexedInputs
        , if entryIndex == 0 then
            Input.button
                ([ alignRight ] ++ buttonAttrs False)
                { onPress = Just <| SetupMsg AddInput
                , label = text "Extend grid"
                }

          else
            Element.none
        ]


setupInputEntryUi : SetupData -> Int -> SetupDataEntry -> Element Msg
setupInputEntryUi setupData entryIndex setupDataEntry =
    row
        [ spacing 12 ]
        [ boolInputUi
            (\desired ->
                case List.Extra.getAt entryIndex setupData.entries of
                    Just entry ->
                        let
                            newEntry =
                                { entry | desired = Just desired }
                        in
                        SetupMsg <|
                            UpdateSetup { setupData | entries = List.Extra.setAt entryIndex newEntry setupData.entries }

                    Nothing ->
                        NoOp
            )
            setupDataEntry.desired
        , row
            [ spacing 4 ]
          <|
            List.append
                (List.indexedMap
                    (\inputIndex currentValue ->
                        let
                            msgFn =
                                \newValue ->
                                    case List.Extra.getAt entryIndex setupData.entries of
                                        Just entry ->
                                            let
                                                newEntry =
                                                    { entry | inputs = List.Extra.setAt inputIndex (Just newValue) entry.inputs }
                                            in
                                            SetupMsg <|
                                                UpdateSetup { setupData | entries = List.Extra.setAt entryIndex newEntry setupData.entries }

                                        Nothing ->
                                            NoOp
                        in
                        boolInputUi msgFn currentValue
                    )
                    setupDataEntry.inputs
                )
                (if entryIndex == 0 then
                    [ Input.button
                        (buttonAttrs False)
                        { onPress = Just <| SetupMsg AddInput
                        , label = text "Add input"
                        }
                    ]

                 else
                    []
                )
        ]


trainingScreen : Model -> UiMode -> Bool -> TrainingData -> Element Msg
trainingScreen model uiMode showWorking trainingData =
    let
        absMaxWeight =
            List.map abs (trainingData.offsetWeight :: trainingData.weights)
                |> List.maximum
                |> Maybe.withDefault 0
    in
    column
        [ width fill
        , spacing 16
        ]
        [ row
            [ width fill
            , spacing 12
            ]
          <|
            List.append
                [ text <| "μ = " ++ String.fromFloat trainingData.μ ]
                (if trainingData.finished then
                    [ Input.button
                        (List.append
                            (buttonAttrs False)
                            [ alignRight ]
                        )
                        { onPress = Just <| TrainingMsg <| TrainSteps 1
                        , label = text "Run again"
                        }
                    , Input.button
                        (List.append
                            (buttonAttrs False)
                            [ alignRight ]
                        )
                        { onPress = Just <| TrainingMsg AdvanceFromTraining
                        , label = text "Finish training"
                        }
                    ]

                 else
                    [ Input.button
                        (List.append
                            (buttonAttrs False)
                            [ alignRight ]
                        )
                        { onPress = Just <| TrainingMsg <| TrainSteps 10
                        , label = text "Run 10 iterations"
                        }
                    , Input.button
                        (List.append
                            (buttonAttrs False)
                            [ alignRight ]
                        )
                        { onPress = Just <| TrainingMsg <| TrainSteps 1
                        , label = text "Run"
                        }
                    ]
                )
        , row
            [ spacing 12
            , width fill
            ]
            [ el
                [ width <| px 100
                , Border.widthEach
                    { bottom = 6, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 210 210 210
                ]
              <|
                text "desired"
            , el
                [ width <| px 100
                , Border.widthEach
                    { bottom = 6, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 210 210 210
                ]
              <|
                text "offset"
            , el
                [ width fill
                , Border.widthEach
                    { bottom = 6, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 210 210 210
                ]
              <|
                text "inputs"
            , if showWorking then
                el
                    [ width <| px 100
                    , Border.widthEach
                        { bottom = 6, top = 0, left = 0, right = 0 }
                    , Border.color <| rgb255 210 210 210
                    ]
                <|
                    text "details"

              else
                Input.button
                    (buttonAttrs False)
                    { onPress = Just <| TrainingMsg ToggleShowWorking
                    , label = text "i"
                    }
            ]
        , row
            [ width fill
            , spacing 12
            ]
          <|
            List.append
                [ el [ width <| px 100 ] <| Element.none
                , weightUi absMaxWeight trainingData.offsetWeight
                ]
                (List.map (weightUi absMaxWeight) trainingData.weights)
        , column [ spacing 8 ] <| List.map (trainingDataEntryUi trainingData) trainingData.entries
        ]


trainingDataEntryUi : TrainingData -> TrainingDataEntry -> Element Msg
trainingDataEntryUi trainingData trainingDataEntry =
    row
        [ spacing 12 ]
    <|
        List.append
            [ boolUi trainingDataEntry.desired
            , boolUi True
            ]
            (List.map boolUi trainingDataEntry.inputs)


executeScreen : Model -> UiMode -> SetupData -> ExecuteData -> Element Msg
executeScreen model uiMode setupData executeData =
    let
        outputValue =
            executeData.offsetWeight
                + (List.map
                    (\executeInput ->
                        executeInput.weight * boolAsFloat executeInput.input
                    )
                    executeData.inputs
                    |> List.sum
                  )

        output =
            if outputValue >= 0 then
                True

            else
                False

        absMaxWeight =
            List.map abs (executeData.offsetWeight :: List.map .weight executeData.inputs)
                |> List.maximum
                |> Maybe.withDefault 0
    in
    column
        [ width fill
        , spacing 12
        ]
        [ row
            [ width fill
            , paddingEach
                { bottom = 10
                , top = 0
                , left = 0
                , right = 0
                }
            ]
            [ Input.button
                (buttonAttrs False)
                { onPress = Just <| ExecuteMsg ReturnToSetup
                , label = text "Back to setup"
                }
            ]
        , row
            [ width fill
            , spacing 12
            ]
            [ el
                [ width <| px 100
                , Border.widthEach
                    { bottom = 6, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 210 210 210
                ]
              <|
                text "offset"
            , el
                [ width fill
                , Border.widthEach
                    { bottom = 6, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 210 210 210
                ]
              <|
                text "inputs"
            , el
                [ width <| px 100
                , Border.widthEach
                    { bottom = 6, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 210 210 210
                ]
              <|
                text "output"
            ]
        , row
            [ width fill
            , spacing 12
            ]
          <|
            List.concat
                [ [ el
                        []
                    <|
                        weightUi absMaxWeight executeData.offsetWeight
                  ]
                , List.map (\executeInput -> weightUi absMaxWeight executeInput.weight) executeData.inputs
                , [ el
                        [ width <| px 100 ]
                        Element.none
                  ]
                ]
        , row
            [ width fill
            , spacing 12
            ]
          <|
            List.concat
                [ [ el
                        []
                    <|
                        boolUi True
                  ]
                , List.indexedMap
                    (\i executeInput ->
                        boolInputUi
                            (\newValue ->
                                let
                                    newInput =
                                        { executeInput | input = newValue }

                                    newData =
                                        { executeData | inputs = List.Extra.setAt i newInput executeData.inputs }
                                in
                                ExecuteMsg <| UpdateExecute newData
                            )
                            (Just executeInput.input)
                    )
                    executeData.inputs
                , [ el
                        [ alignRight ]
                    <|
                        boolUi output
                  ]
                ]
        ]


boolInputUi : (Bool -> Msg) -> Maybe Bool -> Element Msg
boolInputUi clickMsg maybeBool =
    let
        activeBg =
            "rgb(120,150,100)"

        activeText =
            "rgba(0,0,0,0.8)"

        inactiveText =
            "rgb(150,150,150)"

        ( falseBgColour, trueBgColour ) =
            case maybeBool of
                Just True ->
                    ( "transparent", activeBg )

                Just False ->
                    ( activeBg, "transparent" )

                Nothing ->
                    ( "transparent", "transparent" )

        ( falseTextColour, trueTextColour ) =
            case maybeBool of
                Just True ->
                    ( inactiveText, activeText )

                Just False ->
                    ( activeText, inactiveText )

                Nothing ->
                    ( inactiveText, inactiveText )
    in
    el
        [ width <| px 100
        , height <| px 60
        ]
    <|
        html <|
            Svg.svg
                [ Svg.Attributes.width "100"
                , Svg.Attributes.height "60"
                , Svg.Attributes.viewBox "0 0 100 60"
                ]
                [ Svg.style
                    []
                    [ Svg.text "text { pointer-events: none; }"
                    ]
                , Svg.polygon
                    [ Svg.Attributes.points "1,1 1,59 53,59 43,1"
                    , Svg.Attributes.stroke "rgb(50,50,50)"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.cursor "pointer"
                    , Svg.Attributes.fill falseBgColour
                    , Svg.Events.onClick (clickMsg False)
                    ]
                    []
                , Svg.polygon
                    [ Svg.Attributes.points "57,59 47,1 99,1 99,59"
                    , Svg.Attributes.stroke "rgb(50,50,50)"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.cursor "pointer"
                    , Svg.Attributes.fill trueBgColour
                    , Svg.Events.onClick (clickMsg True)
                    ]
                    []
                , Svg.text_
                    [ Svg.Attributes.x "17"
                    , Svg.Attributes.y "40"
                    , Svg.Attributes.fill falseTextColour
                    , Svg.Attributes.fontSize "22pt"
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.cursor "pointer"
                    , Svg.Events.onClick (clickMsg False)
                    ]
                    [ Svg.text "–" ]
                , Svg.text_
                    [ Svg.Attributes.x "64"
                    , Svg.Attributes.y "40"
                    , Svg.Attributes.fill trueTextColour
                    , Svg.Attributes.fontSize "22pt"
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.cursor "pointer"
                    , Svg.Events.onClick (clickMsg True)
                    ]
                    [ Svg.text "+" ]
                ]


boolUi : Bool -> Element Msg
boolUi value =
    let
        activeBg =
            "rgb(220,230,215)"

        activeText =
            "rgba(0,0,0,0.8)"

        inactiveText =
            "rgba(150,150,150,0.2)"

        activeBorderColour =
            "rgb(50,50,50)"

        inactiveBorderColour =
            "rgba(150,150,150,0.7)"

        ( falseBgColour, trueBgColour ) =
            case value of
                True ->
                    ( "transparent", activeBg )

                False ->
                    ( activeBg, "transparent" )

        ( falseTextColour, trueTextColour ) =
            case value of
                True ->
                    ( inactiveText, activeText )

                False ->
                    ( activeText, inactiveText )

        ( falseBorderColour, trueBorderColour ) =
            if value then
                ( inactiveBorderColour, activeBorderColour )

            else
                ( activeBorderColour, inactiveBorderColour )
    in
    el
        [ width <| px 100
        , height <| px 60
        ]
    <|
        html <|
            Svg.svg
                [ Svg.Attributes.width "100"
                , Svg.Attributes.height "60"
                , Svg.Attributes.viewBox "0 0 100 60"
                ]
                [ Svg.style
                    []
                    [ Svg.text "text { pointer-events: none; }"
                    ]
                , Svg.polygon
                    [ Svg.Attributes.points "1,1 1,59 53,59 43,1"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.stroke falseBorderColour
                    , Svg.Attributes.fill falseBgColour
                    ]
                    []
                , Svg.polygon
                    [ Svg.Attributes.points "57,59 47,1 99,1 99,59"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.stroke trueBorderColour
                    , Svg.Attributes.fill trueBgColour
                    ]
                    []
                , Svg.text_
                    [ Svg.Attributes.x "17"
                    , Svg.Attributes.y "40"
                    , Svg.Attributes.fill falseTextColour
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.fontSize "22pt"
                    ]
                    [ Svg.text "–" ]
                , Svg.text_
                    [ Svg.Attributes.x "64"
                    , Svg.Attributes.y "40"
                    , Svg.Attributes.fill trueTextColour
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.fontSize "22pt"
                    ]
                    [ Svg.text "+" ]
                ]


weightUi : Float -> Float -> Element Msg
weightUi maxWeight weight =
    let
        weightFactor =
            weight / max maxWeight 1

        weightBarPx =
            String.fromFloat <| 50 + (weightFactor * 50)
    in
    column
        [ width <| px 100
        , spacing 4
        ]
        [ el
            [ width <| px 100
            , height <| px 15
            ]
          <|
            html <|
                Svg.svg
                    [ Svg.Attributes.width "100"
                    , Svg.Attributes.height "15"
                    ]
                    [ Svg.polygon
                        [ if weightBarPx == "50" then
                            Svg.Attributes.points "48,13 48,3 52,3 52,13"

                          else
                            Svg.Attributes.points ("50,13 50,3 " ++ weightBarPx ++ ",3 " ++ weightBarPx ++ ",13")
                        , Svg.Attributes.fill "rgb(40,150,150)"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "50"
                        , Svg.Attributes.x2 "50"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.y2 "15"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.stroke "black"
                        ]
                        []
                    ]
        , el
            [ width <| px 100
            , height <| px 18
            , Font.size 18
            , clipX
            , inFront <|
                el
                    [ width <| px 12
                    , height fill
                    , alignRight
                    , Background.gradient
                        { angle = pi / 2, steps = [ rgba255 0 0 0 0, rgb255 255 255 255 ] }
                    ]
                    Element.none
            ]
          <|
            text (String.fromFloat weight)
        ]


gridInputUi : (Int -> Bool -> Msg) -> List Bool -> Element Msg
gridInputUi clickMsg inputs =
    let
        dimension =
            ceiling <| sqrt <| toFloat <| List.length inputs

        indexedInputGrid =
            List.Extra.greedyGroupsOf dimension <|
                List.indexedMap Tuple.pair inputs
    in
    column
        []
    <|
        List.map
            (\inputRow ->
                row
                    []
                <|
                    List.map
                        (\( index, selected ) ->
                            Input.button
                                [ height <| px 8
                                , width <| px 8
                                , Border.width 1
                                , Border.color <| rgb255 50 50 50
                                , Background.color
                                    (if selected then
                                        rgb255 180 180 180

                                     else
                                        rgba255 180 180 180 0
                                    )
                                ]
                                { onPress = Just <| clickMsg index <| not selected
                                , label = Element.none
                                }
                        )
                        inputRow
            )
            indexedInputGrid


buttonAttrs : Bool -> List (Attribute Msg)
buttonAttrs selected =
    [ padding 12
    , Border.width 2
    , Border.color <| rgb255 50 50 50
    , Background.color
        (if selected then
            rgb255 180 180 180

         else
            rgba255 180 180 180 0
        )
    ]
