module Day2 exposing (..)

import Array exposing (Array)
import Debug exposing (..)
import Helpers
import List
import Parser exposing (..)
import ParserHelpers



-- SOLUTION


type Command
    = Forward Int
    | Down Int
    | Up Int


type alias State =
    { depth : Int
    , horizontal : Int
    }


part1 : List String -> String
part1 commands =
    commands
        |> List.map (Parser.run commandParser)
        |> ParserHelpers.withoutErrors
        |> runCommands { depth = 0, horizontal = 0 }
        |> String.fromInt


runCommands : State -> List Command -> Int
runCommands state commands =
    case commands of
        [] ->
            state.depth * state.horizontal

        cmd :: remainingCommands ->
            let
                nextState =
                    case cmd of
                        Forward amt ->
                            { state | horizontal = state.horizontal + amt }

                        Down amt ->
                            { state | depth = state.depth + amt }

                        Up amt ->
                            { state | depth = state.depth - amt }
            in
            runCommands nextState remainingCommands


part2 : List String -> String
part2 commands =
    "TODO"


parseCommand cmd =
    run commandParser cmd


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
