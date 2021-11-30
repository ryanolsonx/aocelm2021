module Helpers exposing (..)

import Expect exposing (Expectation)
import Parser exposing (DeadEnd, Problem(..), run)
import Test exposing (..)


deadEndsToString deadEnds =
    String.join "    " <| List.map deadEndToString deadEnds


deadEndToString : DeadEnd -> String
deadEndToString { row, col, problem } =
    String.fromInt row ++ ":" ++ String.fromInt col ++ ": " ++ problemToString problem


problemToString : Problem -> String
problemToString problem =
    case problem of
        Expecting str ->
            "Expecting = " ++ str

        ExpectingInt ->
            "ExpectingInt"

        ExpectingHex ->
            "ExpectingHex"

        ExpectingOctal ->
            "ExpectingOctal"

        ExpectingBinary ->
            "ExpectingBinary"

        ExpectingFloat ->
            "ExpectingFloat"

        ExpectingNumber ->
            "ExpectingNumber"

        ExpectingVariable ->
            "ExpectingVariable"

        ExpectingSymbol sym ->
            "ExpectingSymbol = " ++ sym

        ExpectingKeyword keyword ->
            "ExpectingKeyword = " ++ keyword

        ExpectingEnd ->
            "ExpectingEnd"

        UnexpectedChar ->
            "UnexpectedChar"

        Problem prob ->
            "Problem = " ++ prob

        BadRepeat ->
            "BadRepeat"


testParser parser expected input =
    case run parser input of
        Ok v ->
            Expect.equal expected v

        Err err ->
            Expect.fail <| "Could not parse " ++ deadEndsToString err
