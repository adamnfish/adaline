module Adaline exposing (..)

import List.Extra
import Maybe.Extra
import Random
import Random.Extra



-- TODO: carry this through subsequent steps, so it can be easily edited


type alias SetupData =
    { entries : List SetupDataEntry
    , μ : Float
    }


type alias SetupDataEntry =
    { inputs : List (Maybe Bool)
    , desired : Maybe Bool
    }



-- TODO: carry this to the execute phase for visibility


type alias TrainingData =
    { entries : List TrainingDataEntry
    , weights : List Float
    , offsetWeight : Float
    , finished : Bool
    , μ : Float
    }


type alias TrainingDataEntry =
    { inputs : List Bool
    , desired : Bool
    }


type alias ExecuteData =
    { inputs : List ExecuteInput }


type alias ExecuteInput =
    { input : Bool
    , weight : Float
    }


numberOfInputs : SetupData -> Int
numberOfInputs setupData =
    List.map (\e -> List.length e.inputs) setupData.entries
        |> List.maximum
        |> Maybe.withDefault 0


initialTrainingData : SetupData -> Maybe (Random.Generator TrainingData)
initialTrainingData setupData =
    let
        weightsGen =
            Random.Extra.sequence <|
                List.repeat (numberOfInputs setupData) (Random.float -1 1)

        offsetWeightGen =
            Random.float -1 1

        maybeTrainingEntries : Maybe (List TrainingDataEntry)
        maybeTrainingEntries =
            Maybe.Extra.combine
                (List.map
                    (\setupEntry ->
                        Maybe.map2
                            (\inputs desired ->
                                { inputs = inputs
                                , desired = desired
                                }
                            )
                            (Maybe.Extra.combine setupEntry.inputs)
                            setupEntry.desired
                    )
                    setupData.entries
                )
    in
    Maybe.map
        (\entries ->
            Random.map2
                (\weights offsetWeight ->
                    { μ = setupData.μ
                    , entries = entries
                    , weights = weights
                    , offsetWeight = offsetWeight
                    , finished = False
                    }
                )
                weightsGen
                offsetWeightGen
        )
        maybeTrainingEntries


boolAsFloat : Bool -> Float
boolAsFloat bool =
    if bool then
        1

    else
        -1


advanceTraining : TrainingData -> TrainingData
advanceTraining trainingData =
    -- TODO: training
    -- TODO: check if the delta is small enough to call training 'finished'
    let
        offsetNet =
            1 * trainingData.offsetWeight

        nets : List Float
        nets =
            List.map
                (\entry ->
                    let
                        inputValues =
                            List.map boolAsFloat entry.inputs

                        inputNet =
                            List.Extra.zip inputValues trainingData.weights
                                |> List.map
                                    (\( i, w ) -> i * w)
                                |> List.sum

                        net =
                            inputNet + offsetNet

                        error =
                            boolAsFloat entry.desired - net

                        delta =
                            error * trainingData.μ

                        newWeights =
                            1

                        -- zip weights and entries and apply (delta * x_i) to each
                    in
                    inputNet + offsetNet
                )
                trainingData.entries
    in
    trainingData


advanceTrainingTimes : Int -> TrainingData -> TrainingData
advanceTrainingTimes count trainingData =
    if count <= 0 then
        trainingData

    else
        advanceTrainingTimes (count - 1) (advanceTraining trainingData)


finishTraining : TrainingData -> ExecuteData
finishTraining trainingData =
    { inputs =
        List.map
            (\weight ->
                { input = True
                , weight = weight
                }
            )
            trainingData.weights
    }
