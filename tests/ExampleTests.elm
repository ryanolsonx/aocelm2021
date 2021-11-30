module ExampleTests exposing (..)

import Day1 exposing (..)
import Debug exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Helpers exposing (..)
import Parser exposing (DeadEnd, Problem(..), run)
import Test exposing (..)


suite =
    [ isFieldValidTests
    , parserTests
    , part2Tests
    ]


isFieldValidTests =
    describe "isFieldValid"
        [ fuzz (intRange 0 1919) "birth year invalid under" <|
            \year ->
                isFieldValid (BirthYear year) |> Expect.equal False
        , fuzz (intRange 1920 2002) "birth year valid" <|
            \year ->
                isFieldValid (BirthYear year) |> Expect.equal True
        , fuzz (intRange 2003 2020) "birth year invalid over" <|
            \year ->
                isFieldValid (BirthYear year) |> Expect.equal False
        , fuzz (intRange 0 2009) "issue year invalid under" <|
            \year ->
                isFieldValid (IssueYear year) |> Expect.equal False
        , fuzz (intRange 2010 2020) "issue year valid" <|
            \year ->
                isFieldValid (IssueYear year) |> Expect.equal True
        , fuzz (intRange 2021 2050) "issue year invalid over" <|
            \year ->
                isFieldValid (IssueYear year) |> Expect.equal False
        , fuzz (intRange 0 2019) "expiration year invalid under" <|
            \year ->
                isFieldValid (ExpirationYear year) |> Expect.equal False
        , fuzz (intRange 2020 2030) "expiration year valid" <|
            \year ->
                isFieldValid (ExpirationYear year) |> Expect.equal True
        , fuzz (intRange 2031 2050) "expiration year invalid over" <|
            \year ->
                isFieldValid (ExpirationYear year) |> Expect.equal False
        , test "height without measurement is invalid" <|
            \_ ->
                isFieldValid (Height 200 Nothing) |> Expect.equal False
        , fuzz (intRange 0 58) "height in inches invalid under" <|
            \height ->
                isFieldValid (Height height (Just Inches)) |> Expect.equal False
        , fuzz (intRange 59 76) "height in inches valid" <|
            \height ->
                isFieldValid (Height height (Just Inches)) |> Expect.equal True
        , fuzz (intRange 77 100) "height in inches invalid over" <|
            \height ->
                isFieldValid (Height height (Just Inches)) |> Expect.equal False
        , fuzz (intRange 0 149) "height in centimeters invalid under" <|
            \height ->
                isFieldValid (Height height (Just Centimeters)) |> Expect.equal False
        , fuzz (intRange 150 193) "height in centimeters valid" <|
            \height ->
                isFieldValid (Height height (Just Centimeters)) |> Expect.equal True
        , fuzz (intRange 194 500) "height in centimeters invalid over" <|
            \height ->
                isFieldValid (Height height (Just Centimeters)) |> Expect.equal False
        , test "hair color invalid hex" <|
            \_ ->
                isFieldValid (HairColor (Just "#a")) |> Expect.equal False
        , test "hair color valid hex" <|
            \_ ->
                isFieldValid (HairColor (Just "#abc123")) |> Expect.equal True
        , test "hair color no hex" <|
            \_ ->
                isFieldValid (HairColor Nothing) |> Expect.equal False
        , test "eye color invalid" <|
            \_ ->
                isFieldValid (EyeColor Nothing) |> Expect.equal False
        , test "eye color valid" <|
            \_ ->
                isFieldValid (EyeColor (Just Amber)) |> Expect.equal True
        , test "passport id with leading zeroes valid" <|
            \_ ->
                isFieldValid (PassportId (Just "000012345")) |> Expect.equal True
        , test "passport id with leading zeroes but too many numbers is invalid" <|
            \_ ->
                isFieldValid (PassportId (Just "0000123454")) |> Expect.equal False
        , test "passport id only numbers valid" <|
            \_ ->
                isFieldValid (PassportId (Just "786812345")) |> Expect.equal True
        , test "passport id too few numbers invalid" <|
            \_ ->
                isFieldValid (PassportId (Just "1234")) |> Expect.equal False
        , test "passport id hex invalid" <|
            \_ ->
                isFieldValid (PassportId (Just "#abc123")) |> Expect.equal False
        , test "passport id string invalid" <|
            \_ ->
                isFieldValid (PassportId (Just "sadfasdf")) |> Expect.equal False
        ]


parserTests =
    describe "Parser"
        [ test "toFields" <|
            \_ ->
                [ "hgt:71in eyr:2037"
                , "ecl:#8e276e hcl:z iyr:2019"
                , "byr:2022 pid:157cm"
                , ""
                ]
                    |> toFields
                    |> Expect.equal [ "hgt:71in eyr:2037 ecl:#8e276e hcl:z iyr:2019 byr:2022 pid:157cm" ]
        , test "passport parser" <|
            \_ ->
                --"hgt:71in eyr:2037 ecl:#8e276e hcl:z iyr:2019 byr:2022 pid:157cm"
                "hgt:71in eyr:2037 ecl:#8e276e hcl:z iyr:2019 byr:2022 pid:157cm"
                    |> testParser passportParser
                        [ Height 71 (Just Inches)
                        , ExpirationYear 2037
                        , EyeColor Nothing
                        , HairColor Nothing
                        , IssueYear 2019
                        , BirthYear 2022
                        , PassportId (Just "157")
                        ]
        , fuzz (intRange 0 3000) "birth year parser" <|
            \year ->
                "byr:"
                    ++ String.fromInt year
                    |> testParser birthYearParser (BirthYear year)
        , fuzz (intRange 0 3000) "issue year parser" <|
            \year ->
                "iyr:"
                    ++ String.fromInt year
                    |> testParser issueYearParser (IssueYear year)
        , fuzz (intRange 0 3000) "expiration year parser" <|
            \year ->
                "eyr:"
                    ++ String.fromInt year
                    |> testParser expirationYearParser (ExpirationYear year)
        , describe "height parser"
            [ fuzz (intRange 0 3000) "height in inches" <|
                \height ->
                    "hgt:"
                        ++ String.fromInt height
                        ++ "in"
                        |> testParser heightParser (Height height (Just Inches))
            , fuzz (intRange 0 3000) "height in centimeters" <|
                \height ->
                    "hgt:"
                        ++ String.fromInt height
                        ++ "cm"
                        |> testParser heightParser (Height height (Just Centimeters))
            , fuzz (intRange 0 3000) "height without measurement" <|
                \height ->
                    "hgt:"
                        ++ String.fromInt height
                        |> testParser heightParser (Height height Nothing)
            ]
        , describe "hair color parser"
            [ test "no hash" <|
                \_ ->
                    "hcl:z"
                        |> testParser hairColorParser (HairColor Nothing)
            , test "valid hex" <|
                \_ ->
                    "hcl:#abc012"
                        |> testParser hairColorParser (HairColor (Just "#abc012"))
            , test "invalid hex letters" <|
                \_ ->
                    "hcl:#azc012"
                        |> testParser hairColorParser (HairColor (Just "#a"))
            ]
        , describe "eye color parser"
            [ test "number" <|
                \_ ->
                    "ecl:123"
                        |> testParser eyeColorParser (EyeColor Nothing)
            , test "valid color" <|
                \_ ->
                    "ecl:blu"
                        |> testParser eyeColorParser (EyeColor <| Just Blue)
            , test "with symbols" <|
                \_ ->
                    "ecl:#abc123"
                        |> testParser eyeColorParser (EyeColor Nothing)
            , test "wrong color" <|
                \_ ->
                    "ecl:zzz"
                        |> testParser eyeColorParser (EyeColor Nothing)
            ]
        , describe "passport id parser"
            [ test "letters" <|
                \_ ->
                    "pid:abc"
                        |> testParser passportIdParser (PassportId Nothing)
            , test "valid numbers" <|
                \_ ->
                    "pid:0123"
                        |> testParser passportIdParser (PassportId <| Just "0123")
            , test "hex" <|
                \_ ->
                    "pid:#abc123"
                        |> testParser passportIdParser (PassportId Nothing)
            , test "letters 2" <|
                \_ ->
                    "pid:zzz"
                        |> testParser passportIdParser (PassportId Nothing)
            , test "height" <|
                \_ ->
                    "pid:157cm"
                        |> testParser passportIdParser (PassportId (Just "157"))
            ]
        , describe "company id parser"
            [ test "number" <|
                \_ ->
                    "cid:123"
                        |> testParser companyIdParser (CompanyId "123")
            , test "id" <|
                \_ ->
                    "cid:0123"
                        |> testParser companyIdParser (CompanyId "0123")
            , test "hex" <|
                \_ ->
                    "cid:#abc123"
                        |> testParser companyIdParser (CompanyId "#abc123")
            , test "letters" <|
                \_ ->
                    "cid:zzz"
                        |> testParser companyIdParser (CompanyId "zzz")
            ]
        , describe "unknown parser"
            [ test "number" <|
                \_ ->
                    "x:123"
                        |> testParser unknownParser (Unknown "x" "123")
            , test "id" <|
                \_ ->
                    "x:0123"
                        |> testParser unknownParser (Unknown "x" "0123")
            , test "hex" <|
                \_ ->
                    "x:#abc123"
                        |> testParser unknownParser (Unknown "x" "#abc123")
            , test "letters" <|
                \_ ->
                    "x:zzz"
                        |> testParser unknownParser (Unknown "x" "zzz")
            ]
        ]


part2Tests =
    describe "part 2"
        [ test "invalid passports" <|
            \_ ->
                [ "eyr:1972 cid:100"
                , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
                , ""
                , "iyr:2019"
                , "hcl:#602927 eyr:1967 hgt:170cm"
                , "ecl:grn pid:012533040 byr:1946"
                , ""
                , "hcl:dab227 iyr:2012"
                , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
                , ""
                , "hgt:59cm ecl:zzz"
                , "eyr:2038 hcl:74454a iyr:2023"
                , "pid:3556412378 byr:2007"
                , ""
                ]
                    |> toFields
                    |> toPassports
                    |> part2
                    |> Expect.equal 0
        , test "valid passports" <|
            \_ ->
                [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
                , "hcl:#623a2f"
                , ""
                , "eyr:2029 ecl:blu cid:129 byr:1989"
                , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
                , ""
                , "hcl:#888785"
                , "hgt:164cm byr:2001 iyr:2015 cid:88"
                , "pid:545766238 ecl:hzl"
                , "eyr:2022"
                , ""
                , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
                , ""
                ]
                    |> toFields
                    |> toPassports
                    |> part2
                    |> Expect.equal 4
        ]
