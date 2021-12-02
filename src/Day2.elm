module Day2 exposing (..)

import Helpers
import Parser exposing ((|.), (|=), Parser, int, oneOf, succeed, symbol)
import ParserHelpers



-- STATE


type Command
    = Forward Int
    | Down Int
    | Up Int


type alias Part1State =
    { depth : Int
    , horizontal : Int
    }


type alias Part2State =
    { depth : Int
    , horizontal : Int
    , aim : Int
    }



-- SOLUTION


part1 : List String -> String
part1 input =
    input
        |> toCommands
        |> runCommands
        |> depthTimesHorizontal
        |> String.fromInt


runCommands : List Command -> Part1State
runCommands =
    runCommandsHelp { depth = 0, horizontal = 0 }


runCommandsHelp : Part1State -> List Command -> Part1State
runCommandsHelp state commands =
    case commands of
        [] ->
            state

        command :: remainingCommands ->
            let
                nextState =
                    case command of
                        Forward x ->
                            { state
                                | horizontal = state.horizontal + x
                            }

                        Down x ->
                            { state
                                | depth = state.depth + x
                            }

                        Up x ->
                            { state
                                | depth = state.depth - x
                            }
            in
            runCommandsHelp nextState remainingCommands


depthTimesHorizontal { depth, horizontal } =
    depth * horizontal


part2 : List String -> String
part2 input =
    input
        |> toCommands
        |> runCommandsUsingAim
        |> depthTimesHorizontal
        |> String.fromInt


runCommandsUsingAim : List Command -> Part2State
runCommandsUsingAim =
    runCommandsUsingAimHelp
        { depth = 0
        , horizontal = 0
        , aim = 0
        }


runCommandsUsingAimHelp : Part2State -> List Command -> Part2State
runCommandsUsingAimHelp state commands =
    case commands of
        [] ->
            state

        command :: remainingCommands ->
            let
                nextState =
                    case command of
                        Forward x ->
                            { state
                                | horizontal = state.horizontal + x
                                , depth = state.depth + state.aim * x
                            }

                        Down x ->
                            { state | aim = state.aim + x }

                        Up x ->
                            { state | aim = state.aim - x }
            in
            runCommandsUsingAimHelp nextState remainingCommands



-- HELPERS


toCommands : List String -> List Command
toCommands =
    ParserHelpers.runOnList commandParser


commandParser : Parser Command
commandParser =
    oneOf
        [ succeed Forward
            |. symbol "forward "
            |= int
        , succeed Down
            |. symbol "down "
            |= int
        , succeed Up
            |. symbol "up "
            |= int
        ]



-- RUN


main =
    Helpers.solve part1 part2
