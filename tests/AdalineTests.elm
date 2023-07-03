module AdalineTests exposing (..)

import Adaline exposing (SetupData, SetupDataEntry, TrainingData, TrainingDataEntry, advanceTraining, advanceTrainingTimes, initialTrainingData, numberOfInputs)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz
import List.Extra
import Random exposing (Generator)
import Test exposing (Test)


suite : Test
suite =
    let
        config =
            { trainingCompleteThreshold = 0.0000001 }
    in
    Test.describe "Adaline tests"
        [ Test.describe "numberOfInputs"
            [ Test.test "0 for empty data" <|
                \_ ->
                    numberOfInputs
                        { entries = []
                        , μ = 0.1
                        }
                        |> Expect.equal 0
            , Test.fuzz (fuzzSetupData 0) "0 for entries with no inputs" <|
                \setupData ->
                    numberOfInputs setupData
                        |> Expect.equal 0
            , Test.fuzz (fuzzSetupData 1) "1 for entries with 1 input" <|
                \setupData ->
                    numberOfInputs setupData
                        |> Expect.equal 1
            , Test.fuzz (fuzzSetupData 10) "10 for entries with 10 input" <|
                \setupData ->
                    numberOfInputs setupData
                        |> Expect.equal 10
            , Test.fuzz (Fuzz.intRange 2 20) "n for entry with n inputs" <|
                \inputCount ->
                    numberOfInputs
                        { entries =
                            [ { inputs = List.repeat inputCount (Just True)
                              , desired = Nothing
                              }
                            ]
                        , μ = 0.1
                        }
                        |> Expect.equal inputCount
            , Test.fuzz2 (Fuzz.intRange 2 20) (Fuzz.list <| Fuzz.maybe <| Fuzz.bool) "n for entries with n inputs" <|
                \entryCount inputs ->
                    numberOfInputs
                        { entries =
                            List.repeat entryCount
                                { inputs = inputs
                                , desired = Nothing
                                }
                        , μ = 0.1
                        }
                        |> Expect.equal (List.length inputs)
            ]
        , Test.describe "initialTrainingData"
            [ Test.test "returns Nothing when an input is missing" <|
                \_ ->
                    initialTrainingData
                        { entries =
                            [ { inputs = [ Nothing, Just True ]
                              , desired = Just True
                              }
                            ]
                        , μ = 0.1
                        }
                        |> Expect.equal Nothing
            , Test.test "returns Nothing when a desired is missing" <|
                \_ ->
                    initialTrainingData
                        { entries =
                            [ { inputs = [ Just True, Just True ]
                              , desired = Nothing
                              }
                            ]
                        , μ = 0.1
                        }
                        |> Expect.equal Nothing
            , Test.fuzz2 Fuzz.int (Fuzz.floatRange -1 1) "μ is drawn from the setup data" <|
                \seed μ ->
                    let
                        setupData =
                            { entries = []
                            , μ = μ
                            }

                        result =
                            initialTrainingData setupData
                    in
                    case result of
                        Just genTrainingData ->
                            let
                                ( trainingData, _ ) =
                                    Random.step genTrainingData (Random.initialSeed seed)
                            in
                            trainingData.μ
                                |> Expect.within (Absolute weightTolerance) μ

                        Nothing ->
                            Expect.fail "returned Nothing instead of expected value"
            , Test.fuzz2 Fuzz.int (Fuzz.floatRange -1 1) "finished is False" <|
                \seed μ ->
                    let
                        setupData =
                            { entries = []
                            , μ = μ
                            }

                        result =
                            initialTrainingData setupData
                    in
                    case result of
                        Just genTrainingData ->
                            let
                                ( trainingData, _ ) =
                                    Random.step genTrainingData (Random.initialSeed seed)
                            in
                            trainingData.μ
                                |> Expect.within (Absolute weightTolerance) μ

                        Nothing ->
                            Expect.fail "returned Nothing instead of expected value"
            ]
        , Test.describe "advanceTraining in-progress example"
            [ Test.describe "weights check for in-progress example" <|
                let
                    trainingData =
                        { entries =
                            [ { inputs = [ True, True ]
                              , desired = True
                              }
                            , { inputs = [ False, True ]
                              , desired = False
                              }
                            , { inputs = [ True, False ]
                              , desired = False
                              }
                            ]
                        , μ = 0.2
                        , finished = False

                        -- the weights for each input
                        , weights = [ -0.37, 0.79 ]

                        -- the single offset weight (to normalise the output)
                        , offsetWeight = -0.28
                        }
                in
                [ Test.test "in-progress example weights" <|
                    \_ ->
                        let
                            result =
                                advanceTraining config trainingData

                            weightsAsInts =
                                List.map
                                    (\weight ->
                                        round <| weight * 100
                                    )
                                    result.weights
                        in
                        Expect.equal weightsAsInts [ -16, 69 ]
                , Test.test "in-progress example offset weight" <|
                    \_ ->
                        let
                            result =
                                advanceTraining config trainingData
                        in
                        Expect.within (Absolute 0.01) result.offsetWeight -0.32
                ]
            , Test.test "finished example should see no changes, except to be marked as finished" <|
                \_ ->
                    let
                        trainingData =
                            { entries =
                                [ { inputs = [ True, True ]
                                  , desired = True
                                  }
                                , { inputs = [ False, True ]
                                  , desired = False
                                  }
                                , { inputs = [ True, False ]
                                  , desired = False
                                  }
                                ]
                            , μ = 0.2
                            , finished = False

                            -- the weights for each input
                            , weights = [ 1, 1 ]

                            -- single weight to normalise the output
                            , offsetWeight = -1
                            }
                    in
                    advanceTraining config trainingData
                        |> Expect.equal
                            { entries =
                                [ { inputs = [ True, True ]
                                  , desired = True
                                  }
                                , { inputs = [ False, True ]
                                  , desired = False
                                  }
                                , { inputs = [ True, False ]
                                  , desired = False
                                  }
                                ]
                            , μ = 0.2
                            , finished = True

                            -- the weights for each input
                            , weights = [ 1, 1 ]

                            -- single weight to normalise the output
                            , offsetWeight = -1
                            }
            ]
        , Test.describe "advanceTrainingTimes"
            [ Test.test "0 times does nothing" <|
                \_ ->
                    advanceTrainingTimes 0 config testTrainingData
                        |> Expect.equal testTrainingData
            , Test.test "1 time equals advanceTraining" <|
                \_ ->
                    advanceTrainingTimes 1 config testTrainingData
                        |> Expect.equal (advanceTraining config testTrainingData)
            , Test.fuzz (Fuzz.intRange 2 50) "n + 1 times equals advanceTraining and n times" <|
                \times ->
                    advanceTrainingTimes (times + 1) config testTrainingData
                        |> Expect.equal (advanceTraining config (advanceTrainingTimes times config testTrainingData))
            ]
        , Test.todo "finishTraining"

        --[ Test.test "example" <|
        --    \_ ->
        --        1
        --            |> Expect.equal 1
        --]
        ]


testTrainingData : TrainingData
testTrainingData =
    { entries =
        [ { inputs = [ True, True, True ]
          , desired = True
          }
        ]
    , weights = [ 0.1, 0.2, 0.3 ]
    , offsetWeight = 0.1
    , finished = False
    , μ = 0.1
    }


weightTolerance : Float
weightTolerance =
    0.000000001


fuzzMaybeBool =
    Fuzz.maybe Fuzz.bool


fuzzFixedLengthList : Int -> Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
fuzzFixedLengthList length fuzzer =
    Fuzz.map2
        (\aa fallback ->
            if List.length aa >= length then
                List.take length aa

            else
                List.repeat length fallback
        )
        (Fuzz.list fuzzer)
        fuzzer


fuzzNonEmptyList : Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
fuzzNonEmptyList fuzzer =
    Fuzz.map2
        (\aa fallback ->
            if List.length aa >= 1 then
                aa

            else
                [ fallback ]
        )
        (Fuzz.list fuzzer)
        fuzzer


fuzzWeight =
    Fuzz.floatRange -1 1


fuzzμ =
    Fuzz.floatRange -1 1


fuzzSetupData : Int -> Fuzz.Fuzzer SetupData
fuzzSetupData inputCount =
    Fuzz.map2
        (\entries μ ->
            { entries = entries
            , μ = μ
            }
        )
        (fuzzNonEmptyList <| fuzzSetupDataEntry inputCount)
        fuzzμ


fuzzSetupDataEntry : Int -> Fuzz.Fuzzer SetupDataEntry
fuzzSetupDataEntry inputCount =
    Fuzz.map2
        (\inputs desired ->
            { inputs = inputs
            , desired = desired
            }
        )
        (fuzzFixedLengthList inputCount fuzzMaybeBool)
        fuzzMaybeBool


fuzzTrainingData : Int -> Fuzz.Fuzzer TrainingData
fuzzTrainingData inputCount =
    Fuzz.map4
        (\entries weights offsetWeight μ ->
            { entries = entries
            , weights = weights
            , offsetWeight = offsetWeight
            , finished = False
            , μ = μ
            }
        )
        (fuzzNonEmptyList <| fuzzTrainingDataEntry inputCount)
        (fuzzFixedLengthList inputCount fuzzWeight)
        fuzzWeight
        fuzzμ


fuzzTrainingDataEntry : Int -> Fuzz.Fuzzer TrainingDataEntry
fuzzTrainingDataEntry inputCount =
    Fuzz.map2
        (\inputs desired ->
            { inputs = inputs
            , desired = desired
            }
        )
        (fuzzFixedLengthList inputCount Fuzz.bool)
        Fuzz.bool
