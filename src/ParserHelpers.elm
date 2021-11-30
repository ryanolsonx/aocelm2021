module ParserHelpers exposing (Hex, hexParser, maybe, notSpace, oneOrMore, withoutErrors, zeroOrMore)

import Debug exposing (..)
import Parser exposing (..)


type alias ParserResult a =
    Result (List DeadEnd) a


withoutErrors : List (ParserResult a) -> List a
withoutErrors results =
    withoutErrorsRecur [] results


withoutErrorsRecur : List a -> List (ParserResult a) -> List a
withoutErrorsRecur acc remaining =
    if List.isEmpty remaining then
        acc

    else
        let
            rest =
                Maybe.withDefault [] <| List.tail remaining
        in
        case List.head remaining of
            Just first ->
                case first of
                    Ok v ->
                        withoutErrorsRecur (acc ++ [ v ]) rest

                    Err err ->
                        let
                            e =
                                err |> log "!!! ERROR !!!"
                        in
                        withoutErrorsRecur acc rest

            Nothing ->
                acc


type alias Hex =
    String


{-| Parse a hex string.
hairColorParser : Parser Field
hairColorParser =
succeed HairColor
|. symbol "hcl:"
|= maybe hex
|. zeroOrMore notSpace
-}
hexParser : Parser Hex
hexParser =
    succeed (\s -> "#" ++ s)
        |. symbol "#"
        |= oneOrMore "hex" isHexChar
        |. zeroOrMore notSpace


isHexChar : Char -> Bool
isHexChar c =
    if Char.isDigit c then
        True

    else
        case c of
            'a' ->
                True

            'b' ->
                True

            'c' ->
                True

            'd' ->
                True

            'e' ->
                True

            'f' ->
                True

            'A' ->
                True

            'B' ->
                True

            'C' ->
                True

            'D' ->
                True

            'E' ->
                True

            'F' ->
                True

            _ ->
                False


{-| parse zeroOrMore characters
companyIdParser : Parser Field
companyIdParser =
succeed CompanyId
|. symbol "<cid:">
|= oneOrMore "cid" notSpace
-}
zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
    succeed ()
        |. chompWhile isOk
        |> getChompedString


{-| parse oneOrMore characters
eyeColorParser : Parser Field
eyeColorParser =
succeed toEyeColor
|. symbol "ecl:"
|= maybe (oneOrMore "eye color alpha" Char.isAlpha)
|. zeroOrMore notSpace
-}
oneOrMore : String -> (Char -> Bool) -> Parser String
oneOrMore label isOk =
    succeed ()
        |. oneOf
            [ chompIf isOk
            , problem ("I was expected at least one " ++ label)
            ]
        |. chompWhile isOk
        |> getChompedString


{-| Maybe parse something.

This is great for Maybe values you're filling.

    eyeColorParser : Parser Field
    eyeColorParser =
        succeed toEyeColor
            |. symbol "ecl:"
            |= maybe (oneOrMore "eye color alpha" Char.isAlpha)
            |. zeroOrMore notSpace

-}
maybe : Parser String -> Parser (Maybe String)
maybe parser =
    oneOf
        [ succeed Just
            |= parser
        , succeed Nothing
            |. zeroOrMore notSpace
        ]


notSpace : Char -> Bool
notSpace c =
    c /= ' '
