module Adaline exposing (..)

import List.Extra
import Maybe.Extra
import Random
import Random.Extra



-- TODO: carry setupdata through subsequent steps, so it can be easily edited


type alias SetupData =
    -- entries should be an array for setup, since it changes so much
    { entries : List SetupDataEntry
    , μ : Float
    }


type alias SetupDataEntry =
    -- inputs should be an array for setup, since it changes so much
    { inputs : List (Maybe Bool)
    , desired : Maybe Bool
    }



-- TODO: carry trainingdata to the execute phase for visibility


type alias TrainingData =
    { entries : List TrainingDataEntry
    , μ : Float
    , weights : List Float
    , offsetWeight : Float
    , finished : Bool
    }


type alias TrainingDataEntry =
    { inputs : List Bool
    , desired : Bool
    }


type alias ExecuteData =
    { inputs : List ExecuteInput
    , offsetWeight : Float
    }


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
    if trainingData.finished then
        trainingData

    else
        let
            inputCount =
                List.length trainingData.weights

            -- input and offset deltas for each entry in the training data
            entryDeltas =
                List.Extra.zip
                    trainingData.entries
                    trainingData.weights
                    |> List.map
                        (\( entry, weight ) ->
                            let
                                weightedSum =
                                    trainingData.offsetWeight
                                        + (List.map
                                            (\inputB ->
                                                boolAsFloat inputB * weight
                                            )
                                            entry.inputs
                                            |> List.sum
                                          )

                                error =
                                    boolAsFloat entry.desired - weightedSum

                                deltaMagnitude =
                                    error * trainingData.μ

                                entryInputDeltas =
                                    List.map
                                        (\i ->
                                            boolAsFloat i * deltaMagnitude
                                        )
                                        entry.inputs
                            in
                            ( entryInputDeltas, deltaMagnitude )
                        )

            inputWeightDeltas =
                List.map
                    (\( inputDeltas, _ ) -> inputDeltas)
                    entryDeltas
                    |> List.Extra.transpose
                    |> List.map
                        (\inputDeltas ->
                            List.sum inputDeltas / toFloat inputCount
                        )

            offsetWeightDelta =
                (List.map
                    (\( _, tmp ) -> tmp)
                    entryDeltas
                    |> List.sum
                )
                    / toFloat inputCount

            trainingCompleteThreshold =
                0.00000001

            trainingComplete =
                trainingData.finished || (offsetWeightDelta < trainingCompleteThreshold) && List.all (\w -> w < trainingCompleteThreshold) inputWeightDeltas

            newWeights =
                List.Extra.zip
                    trainingData.weights
                    inputWeightDeltas
                    |> List.map
                        (\( w, d ) ->
                            w + d
                        )
        in
        { trainingData
            | offsetWeight = trainingData.offsetWeight + offsetWeightDelta
            , weights = newWeights
            , finished = trainingComplete
        }


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
    , offsetWeight = trainingData.offsetWeight
    }
