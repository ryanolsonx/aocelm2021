module Aoc exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)


noUpdate _ m =
    m


type Msg
    = None


type alias Results =
    { day : Int
    , examplePart1 : Maybe Int
    , part1 : Maybe Int
    , examplePart2 : Maybe Int
    , part2 : Maybe Int
    }


showResults : Results -> Program () Results Msg
showResults results =
    Browser.sandbox
        { init = results
        , update = \_ m -> m
        , view = view >> toUnstyled
        }


view : Results -> Html Msg
view results =
    let
        title =
            "Day " ++ String.fromInt results.day
    in
    div
        [ css
            [ backgroundColor (hex "000000")
            , color (hex "00ff00")
            , fontFamily sansSerif
            , paddingTop (px 50)
            , paddingLeft (px 50)
            , height (px 1000)
            ]
        ]
        [ h1 [] [ text title ]
        , viewDivider
        , h2 [] [ text "Part 1" ]
        , viewResult "Example" results.examplePart1
        , viewResult "Input" results.part1
        , viewDivider
        , h2 [] [ text "Part 2" ]
        , viewResult "Example" results.examplePart2
        , viewResult "Input" results.part2
        ]


viewDivider =
    div
        [ css
            [ width (px 500)
            , height (px 2)
            , backgroundColor (hex "00ff00")
            ]
        ]
        []


viewResult label n =
    let
        resToString res =
            case res of
                Just x ->
                    String.fromInt x

                Nothing ->
                    "TODO"
    in
    p [] [ text (label ++ ": " ++ resToString n) ]
