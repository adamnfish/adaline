module Adaline exposing (..)

import List.Extra
import Maybe.Extra
import Random
import Random.Extra



-- TODO: carry setupdata through subsequent steps, so it can be easily edited


type alias Config =
    { trainingCompleteThreshold : Float
    }


type alias SetupData =
    -- TODO: entries should be an array for setup, since it changes so much
    { entries : List SetupDataEntry
    , μ : Float
    }


type alias SetupDataEntry =
    -- TODO: inputs should be an array for setup, since it changes so much
    { inputs : List (Maybe Bool)
    , desired : Maybe Bool
    }



-- TODO: carry trainingdata to the execute phase for visibility


type alias TrainingData =
    { entries : List TrainingDataEntry
    , μ : Float
    , finished : Bool

    -- the weights for each input
    , weights : List Float

    -- single weight to normalise the output
    , offsetWeight : Float
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


type alias EntryDeltas =
    { inputDeltas : List Float
    , offsetDelta : Float
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


advanceTraining : Config -> TrainingData -> TrainingData
advanceTraining config trainingData =
    if trainingData.finished then
        trainingData

    else
        let
            entriesCount =
                List.length trainingData.entries |> toFloat

            -- input and offset deltas for each entry in the training data
            -- this is a measure of the current 'error'
            entryDeltas =
                calculateEntryDeltas trainingData

            -- calculate the changes to be applied to each input's weight
            inputWeightDeltas : List Float
            inputWeightDeltas =
                let
                    entriesInputDeltas =
                        List.map
                            .inputDeltas
                            entryDeltas
                in
                entriesInputDeltas
                    -- switch the focus to all of one input's entry data, instead of all inputs in one entry
                    |> List.Extra.transpose
                    |> List.map
                        (\entryInputDeltas ->
                            List.sum entryInputDeltas / entriesCount
                        )

            -- the change to be applied to the offset weight
            offsetWeightDelta =
                let
                    offsetDeltas =
                        List.map
                            .offsetDelta
                            entryDeltas
                in
                List.sum offsetDeltas / entriesCount

            newInputWeights =
                List.map2 (+) trainingData.weights inputWeightDeltas

            trainingComplete =
                trainingData.finished
                    || ((offsetWeightDelta < config.trainingCompleteThreshold)
                            && List.all (\δw -> δw < config.trainingCompleteThreshold) inputWeightDeltas
                       )
        in
        { trainingData
            | offsetWeight = trainingData.offsetWeight + offsetWeightDelta
            , weights = newInputWeights
            , finished = trainingComplete
        }


calculateEntryDeltas : TrainingData -> List EntryDeltas
calculateEntryDeltas trainingData =
    List.map
        (\entry ->
            let
                weightedInputs =
                    List.map2
                        (\inputB inputWeight ->
                            boolAsFloat inputB * inputWeight
                        )
                        entry.inputs
                        trainingData.weights

                weightedSum =
                    trainingData.offsetWeight
                        + List.sum weightedInputs

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
            { inputDeltas = entryInputDeltas, offsetDelta = deltaMagnitude }
        )
        trainingData.entries


advanceTrainingTimes : Int -> Config -> TrainingData -> TrainingData
advanceTrainingTimes count config trainingData =
    if count <= 0 then
        trainingData

    else
        advanceTrainingTimes (count - 1) config (advanceTraining config trainingData)


advanceToCompletion : Config -> TrainingData -> TrainingData
advanceToCompletion config initialData =
    let
        -- protect against a diverging training process with a count limit
        loop : Int -> TrainingData -> TrainingData
        loop count trainingData =
            if trainingData.finished then
                trainingData

            else if count > 5000 then
                trainingData

            else
                loop (count + 1) (advanceTraining config trainingData)
    in
    loop 0 initialData


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
