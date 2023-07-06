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


type Lifecycle
    = Welcome
    | Setup String Config SetupData
    | Training Bool Config SetupData TrainingData
    | Execute Config SetupData ExecuteData


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
    | RemoveEntry Int
    | AddInput
    | RemoveInput Int
    | UpdateSetup SetupData
    | Updateμ String
    | AdvanceFromSetup


type TrainingMsg
    = TrainSteps Int
    | TrainToCompletion
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
                            in
                            ( { model | lifecycle = Setup (String.fromFloat μ) initialConfig initialData }
                            , Cmd.none
                            )

                _ ->
                    -- TODO: error feedback
                    ( model
                    , Cmd.none
                    )

        SetupMsg setupMsg ->
            case model.lifecycle of
                Setup μInput config currentSetupData ->
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
                            ( { model | lifecycle = Setup μInput config newSetupData }
                            , Cmd.none
                            )

                        RemoveEntry index ->
                            let
                                newSetupData =
                                    { currentSetupData
                                        | entries =
                                            List.Extra.removeAt index currentSetupData.entries
                                    }
                            in
                            ( { model | lifecycle = Setup μInput config newSetupData }
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
                            ( { model | lifecycle = Setup μInput config newSetupData }
                            , Cmd.none
                            )

                        RemoveInput index ->
                            let
                                newSetupData =
                                    { currentSetupData
                                        | entries =
                                            List.map
                                                (\e -> { e | inputs = List.Extra.removeAt index e.inputs })
                                                currentSetupData.entries
                                    }
                            in
                            ( { model | lifecycle = Setup μInput config newSetupData }
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
                                        , lifecycle = Training False config currentSetupData trainingData
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    -- TODO: error feedback
                                    ( model
                                    , Cmd.none
                                    )

                        UpdateSetup newSetupData ->
                            ( { model | lifecycle = Setup μInput config newSetupData }
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
                            ( { model | lifecycle = Setup newμ config newSetupData }
                            , Cmd.none
                            )

                _ ->
                    -- TODO: report error
                    ( model, Cmd.none )

        TrainingMsg trainingMsg ->
            case model.lifecycle of
                Training showWorking config setupData currentTrainingData ->
                    case trainingMsg of
                        TrainSteps iterations ->
                            let
                                newTrainingData =
                                    advanceTrainingTimes iterations config currentTrainingData
                            in
                            ( { model
                                | lifecycle = Training showWorking config setupData newTrainingData
                              }
                            , Cmd.none
                            )

                        TrainToCompletion ->
                            let
                                newTrainingData =
                                    advanceToCompletion config currentTrainingData
                            in
                            ( { model
                                | lifecycle = Training showWorking config setupData newTrainingData
                              }
                            , Cmd.none
                            )

                        ToggleShowWorking ->
                            ( { model
                                | lifecycle = Training (not showWorking) config setupData currentTrainingData
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
                                    | lifecycle = Execute config setupData executeData
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
                Execute config setupData currentExecuteData ->
                    case executeMsg of
                        UpdateExecute newExecuteData ->
                            ( { model | lifecycle = Execute config setupData newExecuteData }
                            , Cmd.none
                            )

                        ReturnToSetup ->
                            ( { model | lifecycle = Setup (String.fromFloat setupData.μ) config setupData }
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

            Setup _ _ _ ->
                "Adaline | setup"

            Training _ _ _ _ ->
                "Adaline | training"

            Execute _ _ _ ->
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

                Setup μInput config setupData ->
                    setupScreen model μInput setupData

                Training showWorking config setupData trainingData ->
                    trainingScreen model showWorking trainingData

                Execute config setupData executeData ->
                    executeScreen model setupData executeData
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
            (buttonAttrs CtaButton False)
            { onPress = Just <| WelcomeMsg BeginTraining
            , label =
                text "Begin training"
            }
        ]


setupScreen : Model -> String -> SetupData -> Element Msg
setupScreen model μInput setupData =
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
                (List.append
                    (buttonAttrs CtaButton False)
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
            [ width fill
            , spacing 4
            ]
          <|
            List.indexedMap (setupEntryUi setupData) setupData.entries
        , row
            [ spacing 12 ]
            [ Input.button
                ([ width <| px 100 ] ++ buttonAttrs AddButton False)
                { onPress = Just <| SetupMsg AddEntry
                , label = text "+ entry"
                }
            , row
                [ spacing 4 ]
              --[ Input.button
              --    ([ width <| px 100 ] ++ buttonAttrs DeleteButton False)
              --    { onPress = Just <| SetupMsg <| RemoveEntry 1
              --    , label = text "- input"
              --    }
              <|
                case setupData.entries of
                    [] ->
                        []

                    entry :: _ ->
                        List.indexedMap
                            (\i _ ->
                                Input.button
                                    ([ width <| px 100 ] ++ buttonAttrs DeleteButton False)
                                    { onPress = Just <| SetupMsg <| RemoveInput i
                                    , label = text "- input"
                                    }
                            )
                            entry.inputs
            ]
        ]


setupEntryUi : SetupData -> Int -> SetupDataEntry -> Element Msg
setupEntryUi setupData entryIndex setupDataEntry =
    row
        [ width fill
        , spacing 12
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
        , row
            [ width fill
            , spacing 4
            ]
          <|
            List.concat
                [ List.indexedMap
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
                , if entryIndex == 0 then
                    [ Input.button
                        (buttonAttrs AddButton False)
                        { onPress = Just <| SetupMsg AddInput
                        , label = text "+ input"
                        }
                    ]

                  else
                    []
                , [ Input.button
                        ([ alignRight ] ++ buttonAttrs DeleteButton False)
                        { onPress = Just <| SetupMsg <| RemoveEntry entryIndex
                        , label = text "- entry"
                        }
                  ]
                ]
        ]


trainingScreen : Model -> Bool -> TrainingData -> Element Msg
trainingScreen model showWorking trainingData =
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
                            (buttonAttrs CtaButton False)
                            [ alignRight ]
                        )
                        { onPress = Just <| TrainingMsg AdvanceFromTraining
                        , label = text "Finish training"
                        }
                    ]

                 else
                    [ Input.button
                        (List.append
                            (buttonAttrs InfoButton False)
                            [ alignRight ]
                        )
                        { onPress = Just <| TrainingMsg <| TrainSteps 1
                        , label = text "Single training step"
                        }
                    , Input.button
                        (List.append
                            (buttonAttrs CtaButton False)
                            [ alignRight ]
                        )
                        { onPress = Just <| TrainingMsg <| TrainToCompletion
                        , label = text "Train"
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


executeScreen : Model -> SetupData -> ExecuteData -> Element Msg
executeScreen model setupData executeData =
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
                (buttonAttrs InfoButton False)
                { onPress = Just <| ExecuteMsg ReturnToSetup
                , label = text "Back to setup"
                }
            ]
        , row
            [ width fill
            , spacing 12
            ]
            [ el
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
                text "offset"
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
            List.append
                (List.map (\executeInput -> weightUi absMaxWeight executeInput.weight) executeData.inputs)
                [ el
                    [ alignRight ]
                  <|
                    weightUi absMaxWeight executeData.offsetWeight
                , el
                    [ width <| px 100 ]
                    Element.none
                ]
        , row
            [ width fill
            , spacing 12
            ]
          <|
            List.append
                (List.indexedMap
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
                )
                [ el
                    [ alignRight ]
                  <|
                    boolUi True
                , el
                    [ alignRight ]
                  <|
                    boolUi output
                ]
        ]


boolInputUi : (Bool -> Msg) -> Maybe Bool -> Element Msg
boolInputUi msgFn maybeBool =
    let
        activeBg =
            "rgb(120,150,150)"

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
                [ Svg.polygon
                    [ Svg.Attributes.points "1,1 1,59 53,59 43,1"
                    , Svg.Attributes.stroke "rgb(50,50,50)"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.cursor "pointer"
                    , Svg.Attributes.fill falseBgColour
                    , Svg.Events.onClick (msgFn False)
                    ]
                    []
                , Svg.polygon
                    [ Svg.Attributes.points "57,59 47,1 99,1 99,59"
                    , Svg.Attributes.stroke "rgb(50,50,50)"
                    , Svg.Attributes.strokeWidth "2"
                    , Svg.Attributes.cursor "pointer"
                    , Svg.Attributes.fill trueBgColour
                    , Svg.Events.onClick (msgFn True)
                    ]
                    []
                , Svg.text_
                    [ Svg.Attributes.x "9"
                    , Svg.Attributes.y "40"
                    , Svg.Attributes.fill falseTextColour
                    , Svg.Attributes.fontSize "22pt"
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.cursor "pointer"
                    , Svg.Events.onClick (msgFn False)
                    ]
                    [ Svg.text "-1" ]
                , Svg.text_
                    [ Svg.Attributes.x "67"
                    , Svg.Attributes.y "40"
                    , Svg.Attributes.fill trueTextColour
                    , Svg.Attributes.fontSize "22pt"
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.cursor "pointer"
                    , Svg.Events.onClick (msgFn True)
                    ]
                    [ Svg.text "1" ]
                ]


boolUi : Bool -> Element Msg
boolUi value =
    let
        activeBg =
            "rgb(180,180,180)"

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
                [ Svg.polygon
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
                    [ Svg.Attributes.x "9"
                    , Svg.Attributes.y "40"
                    , Svg.Attributes.fill falseTextColour
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.fontSize "22pt"
                    ]
                    [ Svg.text "-1" ]
                , Svg.text_
                    [ Svg.Attributes.x "67"
                    , Svg.Attributes.y "40"
                    , Svg.Attributes.fill trueTextColour
                    , Svg.Attributes.fontFamily "sans-serif"
                    , Svg.Attributes.fontSize "22pt"
                    ]
                    [ Svg.text "1" ]
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


type ButtonType
    = CtaButton
    | InfoButton
    | DeleteButton
    | AddButton


buttonAttrs : ButtonType -> Bool -> List (Attribute Msg)
buttonAttrs buttonType selected =
    [ padding <|
        case buttonType of
            CtaButton ->
                12

            InfoButton ->
                6

            DeleteButton ->
                6

            AddButton ->
                12
    , Border.width 2
    , Border.color <| rgb255 50 50 50
    , Font.center
    , Background.color
        (if selected then
            rgb255 180 180 180

         else
            case buttonType of
                CtaButton ->
                    rgb255 175 195 235

                InfoButton ->
                    rgb255 230 230 240

                DeleteButton ->
                    rgb255 200 170 180

                AddButton ->
                    rgb255 190 200 180
        )
    ]
