module Day1 exposing (..)

import Helpers



-- STATE


type alias State =
    { timesIncreased : Int
    , previousDepth : Int
    }



-- SOLUTION


part1 : List String -> String
part1 depths =
    depths
        |> Helpers.toInts
        |> countTimesDepthIncreasesBetweenEach
        |> String.fromInt


countTimesDepthIncreasesBetweenEach : List Int -> Int
countTimesDepthIncreasesBetweenEach depths =
    case depths of
        [] ->
            0

        depth :: remainingDepths ->
            countTimesDepthIncreasesBetweenEachHelp
                { timesIncreased = 0
                , previousDepth = depth
                }
                remainingDepths


countTimesDepthIncreasesBetweenEachHelp : State -> List Int -> Int
countTimesDepthIncreasesBetweenEachHelp { timesIncreased, previousDepth } depths =
    case depths of
        [] ->
            timesIncreased

        depth :: remainingDepths ->
            countTimesDepthIncreasesBetweenEachHelp
                { timesIncreased =
                    if depth > previousDepth then
                        timesIncreased + 1

                    else
                        timesIncreased
                , previousDepth = depth
                }
                remainingDepths


part2 : List String -> String
part2 depths =
    depths
        |> Helpers.toInts
        |> toSlidingWindows
        |> countTimesDepthIncreasesBetweenEach
        |> String.fromInt


toSlidingWindows : List Int -> List Int
toSlidingWindows =
    toSlidingWindowsHelp []


toSlidingWindowsHelp : List Int -> List Int -> List Int
toSlidingWindowsHelp windows remaining =
    if List.length remaining < 3 then
        windows

    else
        let
            window =
                List.foldl (+) 0 (List.take 3 remaining)
        in
        toSlidingWindowsHelp
            (windows ++ [ window ])
            (List.drop 1 remaining)



-- RUN


main =
    Helpers.solve part1 part2
