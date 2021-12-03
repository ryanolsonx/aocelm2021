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


type alias BinaryResult =
    { zeros : Int
    , ones : Int
    }



-- SOLUTION


part1 : List String -> String
part1 input =
    getGammaRate input
        * getEpsilonRate input
        |> String.fromInt


getGammaRate : List String -> Int
getGammaRate input =
    getRateHelp
        { input = input
        , binaryLength = String.length <| Maybe.withDefault "" <| List.head input
        , rate = ""
        , mostCommon = True
        }
        0


getEpsilonRate : List String -> Int
getEpsilonRate input =
    getRateHelp
        { input = input
        , binaryLength = String.length <| Maybe.withDefault "" <| List.head input
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
            res =
                countZerosAndOnesAtIndex index state.input

            next =
                if state.mostCommon then
                    if res.zeros > res.ones then
                        "0"

                    else
                        "1"

                else if res.zeros < res.ones then
                    "0"

                else
                    "1"
        in
        getRateHelp
            { state | rate = state.rate ++ next }
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


countZerosAndOnesAtIndex : Int -> List String -> BinaryResult
countZerosAndOnesAtIndex index input =
    input
        |> List.map (intAt index)
        |> countZerosAndOnes


countZerosAndOnes : List Int -> BinaryResult
countZerosAndOnes ns =
    let
        ones =
            List.foldl (+) 0 ns
    in
    { zeros = List.length ns - ones, ones = ones }


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
