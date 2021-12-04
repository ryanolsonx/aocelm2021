module Day3 exposing (..)

import Array exposing (Array)
import Helpers
import Parser exposing (..)
import ParserHelpers



-- SOLUTION


part1 : List String -> String
part1 binaryNumbers =
    getGammaRate binaryNumbers
        * getEpsilonRate binaryNumbers
        |> String.fromInt


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
part2 input =
    "TODO"



-- RUN


main =
    Helpers.solve part1 part2
