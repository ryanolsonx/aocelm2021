module Day1 exposing (..)

import Aoc
import Array exposing (Array)
import Debug exposing (..)
import List
import Parser exposing (..)

-- TODO:
--   1. 1 -> day you're on
--   2. $TITLE -> title of the day you're on
--   2. $INSTRUCTIONS -> the part 1 (and later part 2) instructions
--   3. $INPUT_TYPE -> type of input you're capturing (i.e. List String)
--       - My strategy is to cleanup the input with multiple-cursors in my editor to something
--         better than just raw input.
--   4. Remove this TODO block.

{-| Day 1: $TITLE

$INSTRUCTIONS
-}


main =
    let
        results =
            { day = 1
            , examplePart1 = part1 exampleInput
            , part1 = Nothing -- part1 realInput
            , examplePart2 = Nothing -- part2 exampleInput
            , part2 = Nothing -- part2 realInput
            }
    in
    Aoc.showResults results



-- INPUT


exampleInput : $INPUT_TYPE
exampleInput =
    []


realInput : $INPUT_TYPE
realInput =
    []



-- APP


part1 : $INPUT_TYPE -> Maybe Int
part1 input =
    Nothing


part2 : $INPUT_TYPE -> Maybe Int
part2 input =
    Nothing



