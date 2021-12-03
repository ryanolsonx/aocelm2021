module Day3 exposing (..)

import Array exposing (Array)
import Helpers
import Parser exposing (..)
import ParserHelpers



-- MODEL


type alias State =
    { input : List String
    , rate : String
    , binaryLength : Int
    , mostCommon : Bool
    }



-- SOLUTION


part1 : List String -> String
part1 input =
    let
        length =
            String.length <| Maybe.withDefault "" <| List.head input
    in
    getGammaRate input length
        * getEpsilonRate input length
        |> String.fromInt


getGammaRate : List String -> Int -> Int
getGammaRate input length =
    getRateHelp
        { input = input
        , binaryLength = length
        , rate = ""
        , mostCommon = True
        }
        0


getEpsilonRate : List String -> Int -> Int
getEpsilonRate input length =
    getRateHelp
        { input = input
        , binaryLength = length
        , rate = ""
        , mostCommon = False
        }
        0


getRateHelp : State -> Int -> Int
getRateHelp state index =
    if index == state.binaryLength then
        binaryStringToInt state.rate

    else
        let
            mostCommonBit =
                mostCommonBitAtIndex index state.input

            next =
                if state.mostCommon then
                    mostCommonBit

                else if mostCommonBit == 1 then
                    0

                else
                    1
        in
        getRateHelp
            { state | rate = state.rate ++ String.fromInt next }
            (index + 1)


binaryStringToInt : String -> Int
binaryStringToInt binaryString =
    binaryStringToIntHelp 0 0 (String.reverse binaryString)


binaryStringToIntHelp result index binaryString =
    if index >= String.length binaryString then
        result

    else
        let
            nextResult =
                result + (intAt index binaryString * 2 ^ index)
        in
        binaryStringToIntHelp nextResult (index + 1) binaryString


mostCommonBitAtIndex : Int -> List String -> Int
mostCommonBitAtIndex index input =
    let
        ones =
            input
                |> List.map (intAt index)
                |> List.foldl (+) 0

        zeros =
            List.length input - ones
    in
    if ones > zeros then
        1

    else
        0


intAt index str =
    str
        |> String.slice index (index + 1)
        |> String.toInt
        |> Maybe.withDefault 0


part2 : List String -> String
part2 input =
    "TODO"



-- RUN


main =
    Helpers.solve part1 part2
