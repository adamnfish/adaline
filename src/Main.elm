module Main exposing (..)

import Adaline exposing (..)
import Browser exposing (Document)
import Html exposing (Html, div, text)
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
    | SetupMsg SetupMsg
    | TrainingMsg TrainingMsg


type SetupMsg
    = AddEntry
    | AddInput
    | Update SetupData
    | AdvanceFromSetup


type TrainingMsg
    = TrainSteps Int
    | AdvanceFromTraining


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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

                        Update newSetupData ->
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
        [ div []
            [ text "Adaline"
            ]
        ]
    }
