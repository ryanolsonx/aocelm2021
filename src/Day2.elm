module Day2 exposing (..)

import Array exposing (Array)
import Debug exposing (..)
import Helpers
import List
import Parser exposing (..)
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
part1 commands =
    commands
        |> toCommands
        |> moveUsingCommands
        |> depthTimesHorizontal
        |> String.fromInt


moveUsingCommands : List Command -> Part1State
moveUsingCommands commands =
    moveUsingCommandsHelp { depth = 0, horizontal = 0 } commands


moveUsingCommandsHelp : Part1State -> List Command -> Part1State
moveUsingCommandsHelp state commands =
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
            moveUsingCommandsHelp nextState remainingCommands


depthTimesHorizontal { depth, horizontal } =
    depth * horizontal


part2 : List String -> String
part2 commands =
    commands
        |> toCommands
        |> aimAndMoveUsingCommands
        |> depthTimesHorizontal
        |> String.fromInt


aimAndMoveUsingCommands : List Command -> Part2State
aimAndMoveUsingCommands commands =
    aimAndMoveUsingCommandsHelp
        { depth = 0
        , horizontal = 0
        , aim = 0
        }
        commands


aimAndMoveUsingCommandsHelp : Part2State -> List Command -> Part2State
aimAndMoveUsingCommandsHelp state commands =
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
            aimAndMoveUsingCommandsHelp nextState remainingCommands



-- HELPERS


toCommands : List String -> List Command
toCommands commands =
    commands
        |> List.map (Parser.run commandParser)
        |> ParserHelpers.withoutErrors


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
