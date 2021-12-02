module Day1 exposing (..)

import Array exposing (Array)
import Debug exposing (..)
import Helpers
import List
import Parser exposing (..)



-- SOLUTION


part1 : List String -> String
part1 depths =
    part1Help
        { timesInc = 0
        , previous = Nothing
        , remainingDepths = Helpers.toInts depths
        }
        |> String.fromInt


part1Help { timesInc, previous, remainingDepths } =
    case remainingDepths of
        [] ->
            timesInc

        current :: remaining ->
            part1Help
                { timesInc =
                    if didDepthIncrease timesInc previous current then
                        timesInc + 1

                    else
                        timesInc
                , previous = Just current
                , remainingDepths = remaining
                }


didDepthIncrease timesInc previous current =
    case previous of
        Just prev ->
            if current > prev then
                True

            else
                False

        Nothing ->
            False


part2 : List String -> String
part2 input =
    let
        windows =
            input
                |> Helpers.toInts
                |> toSlidingWindows
    in
    part1Help
        { timesInc = 0
        , previous = Nothing
        , remainingDepths = windows
        }
        |> String.fromInt


toSlidingWindows : List Int -> List Int
toSlidingWindows input =
    toSlidingWindowsHelp [] input


toSlidingWindowsHelp : List Int -> List Int -> List Int
toSlidingWindowsHelp windows remaining =
    if List.length remaining < 3 then
        windows

    else
        let
            window =
                List.foldl (+) 0 (List.take 3 remaining)
        in
        toSlidingWindowsHelp (windows ++ [ window ]) (List.drop 1 remaining)



-- RUN


main =
    Helpers.solve part1 part2
