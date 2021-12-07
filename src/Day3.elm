module Day3 exposing (..)

import Array exposing (Array)
import Helpers
import Parser exposing (..)
import ParserHelpers



-- SOLUTION


part1 : List String -> String
part1 binaryNumbers =
    getPowerConsumption binaryNumbers |> String.fromInt


getPowerConsumption : List String -> Int
getPowerConsumption binaryNumbers =
    getGammaRate binaryNumbers * getEpsilonRate binaryNumbers


getGammaRate : List String -> Int
getGammaRate binaryNumbers =
    getGammaRateHelp (getStringLength binaryNumbers) 0 "" binaryNumbers


getGammaRateHelp : Int -> Int -> String -> List String -> Int
getGammaRateHelp length index gammaRateAcc binaryNumbers =
    if index >= length then
        binaryToInt gammaRateAcc

    else
        let
            mostCommon =
                binaryNumbers
                    |> getBitsAt index
                    |> getMostCommon
                    |> String.fromInt
        in
        getGammaRateHelp length (index + 1) (gammaRateAcc ++ mostCommon) binaryNumbers


getBitsAt index binaryNumbers =
    binaryNumbers
        |> List.map (getAt index)


getAt index str =
    str
        |> String.slice index (index + 1)
        |> String.toInt
        |> Maybe.withDefault 0


getStringLength : List String -> Int
getStringLength str =
    List.head str
        |> Maybe.withDefault ""
        |> String.length


binaryToInt : String -> Int
binaryToInt binaryString =
    binaryToIntHelp 0 0 (String.reverse binaryString)


binaryToIntHelp result index binaryString =
    if index >= String.length binaryString then
        result

    else
        let
            nextResult =
                result + (getAt index binaryString * 2 ^ index)
        in
        binaryToIntHelp nextResult (index + 1) binaryString


getMostCommon : List Int -> Int
getMostCommon ns =
    let
        sum =
            List.foldl (+) 0 ns
    in
    if sum >= List.length ns - sum then
        1

    else
        0


getEpsilonRate : List String -> Int
getEpsilonRate binaryNumbers =
    getEpsilonRateHelp (getStringLength binaryNumbers) 0 "" binaryNumbers


getEpsilonRateHelp : Int -> Int -> String -> List String -> Int
getEpsilonRateHelp length index gammaRateAcc binaryNumbers =
    if index >= length then
        binaryToInt gammaRateAcc

    else
        let
            mostCommon =
                binaryNumbers
                    |> getBitsAt index
                    |> getLeastCommon
                    |> String.fromInt
        in
        getEpsilonRateHelp length (index + 1) (gammaRateAcc ++ mostCommon) binaryNumbers


getLeastCommon : List Int -> Int
getLeastCommon ns =
    let
        sum =
            List.foldl (+) 0 ns
    in
    if sum < List.length ns - sum then
        1

    else
        0


part2 : List String -> String
part2 binaryNumbers =
    getLifeSupportRating binaryNumbers |> String.fromInt


getLifeSupportRating binaryNumbers =
    getOxygenGeneratorRating binaryNumbers * getC02ScrubberRating binaryNumbers


getOxygenGeneratorRating : List String -> Int
getOxygenGeneratorRating binaryNumbers =
    getOxygenGeneratorRatingHelp
        { length = getStringLength binaryNumbers
        , bitIndex = 0
        , result = ""
        }
        binaryNumbers
        |> binaryToInt


type alias OxygenGeneratorState =
    { length : Int
    , bitIndex : Int
    , result : String
    }


getOxygenGeneratorRatingHelp : OxygenGeneratorState -> List String -> String
getOxygenGeneratorRatingHelp state binaryNumbers =
    if List.length binaryNumbers == 1 then
        Maybe.withDefault "" (List.head binaryNumbers)

    else if state.bitIndex >= state.length then
        -- Couldn't find it...
        ""

    else
        let
            nextBit =
                binaryNumbers
                    |> getBitsAt state.bitIndex
                    |> getMostCommon
                    |> String.fromInt

            nextResult =
                state.result ++ nextBit

            matching =
                binaryNumbers
                    |> List.filter (String.startsWith nextResult)
        in
        getOxygenGeneratorRatingHelp
            { state
                | bitIndex = state.bitIndex + 1
                , result = nextResult
            }
            matching


getC02ScrubberRating binaryNumbers =
    getC02ScrubberRatingHelp
        { length = getStringLength binaryNumbers
        , bitIndex = 0
        , result = ""
        }
        binaryNumbers
        |> binaryToInt


type alias C02ScrubberState =
    { length : Int
    , bitIndex : Int
    , result : String
    }


getC02ScrubberRatingHelp : C02ScrubberState -> List String -> String
getC02ScrubberRatingHelp state binaryNumbers =
    if List.length binaryNumbers == 1 then
        Maybe.withDefault "" (List.head binaryNumbers)

    else if state.bitIndex >= state.length then
        -- Couldn't find it...
        ""

    else
        let
            nextBit =
                binaryNumbers
                    |> getBitsAt state.bitIndex
                    |> getLeastCommon
                    |> String.fromInt

            nextResult =
                state.result ++ nextBit

            matching =
                binaryNumbers
                    |> List.filter (String.startsWith nextResult)
        in
        getC02ScrubberRatingHelp
            { state
                | bitIndex = state.bitIndex + 1
                , result = nextResult
            }
            matching



-- RUN


main =
    Helpers.solve part1 part2
