module Day4 exposing (..)

import Array
import Helpers
import Parser exposing (..)
import Regex



-- MODEL


type alias Board =
    List (List Square)


type Square
    = Unmarked Int
    | Marked Int
    | Invalid


type alias ParseBoardState =
    { boards : List Board
    , currentBoard : Board
    }


type alias PlayResult =
    { winningBoard : Board
    , lastDraw : Int
    }



-- PART 1


part1 : List String -> String
part1 input =
    let
        draws =
            getDrawsFromInput input

        boards =
            getBoardsFromInput input
    in
    case playUntilWinner boards draws of
        Just { winningBoard, lastDraw } ->
            (sumOfUnmarked winningBoard * lastDraw)
                |> String.fromInt

        Nothing ->
            "Failure: could not find a winning board"



-- PARSE INPUT


getDrawsFromInput : List String -> List Int
getDrawsFromInput input =
    case List.head input of
        Just draws ->
            draws
                |> String.split ","
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)

        Nothing ->
            []


getBoardsFromInput : List String -> List Board
getBoardsFromInput input =
    let
        boardsLines =
            List.drop 2 input
    in
    getBoardsFromInputHelp
        { boards = [], currentBoard = [] }
        boardsLines


getBoardsFromInputHelp : ParseBoardState -> List String -> List Board
getBoardsFromInputHelp state input =
    case input of
        [] ->
            state.boards

        line :: remainingLines ->
            if String.isEmpty line then
                getBoardsFromInputHelp
                    { boards = state.boards ++ [ state.currentBoard ]
                    , currentBoard = []
                    }
                    remainingLines

            else
                let
                    nextCurrentBoard =
                        state.currentBoard ++ [ parseBoardLine line ]
                in
                getBoardsFromInputHelp { state | currentBoard = nextCurrentBoard } remainingLines


parseBoardLine : String -> List Square
parseBoardLine line =
    line
        |> String.trimLeft
        |> Regex.split whitespace
        |> List.map String.toInt
        |> List.map toInitialSquare


whitespace : Regex.Regex
whitespace =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\s+"


toInitialSquare : Maybe Int -> Square
toInitialSquare maybeN =
    case maybeN of
        Just n ->
            Unmarked n

        Nothing ->
            Invalid



-- PLAY BINGO


playUntilWinner : List Board -> List Int -> Maybe PlayResult
playUntilWinner boards draws =
    case draws of
        [] ->
            Nothing

        draw :: remainingDraws ->
            let
                nextBoards =
                    boards
                        |> List.map (markDrawOnBoard draw)
            in
            case getWinningBoard nextBoards of
                Just winningBoard ->
                    Just
                        { winningBoard = winningBoard
                        , lastDraw = draw
                        }

                Nothing ->
                    playUntilWinner nextBoards remainingDraws


markDrawOnBoard : Int -> Board -> Board
markDrawOnBoard n board =
    board
        |> List.map (markDrawOnBoardLine n)


markDrawOnBoardLine n boardLine =
    boardLine
        |> List.map (markDrawOnSquare n)


markDrawOnSquare n square =
    case square of
        Unmarked sqN ->
            if sqN == n then
                Marked n

            else
                square

        Marked _ ->
            square

        Invalid ->
            square


getWinningBoard : List Board -> Maybe Board
getWinningBoard boards =
    let
        winningBoards =
            List.filter isWinningBoard boards
    in
    if List.length winningBoards == 1 then
        List.head winningBoards

    else
        Nothing


isWinningBoard : Board -> Bool
isWinningBoard board =
    hasBingoRow board || hasBingoColumn board


hasBingoRow board =
    let
        bingoRowsCount =
            board
                |> List.filter (List.all isMarked)
                |> List.length
    in
    bingoRowsCount > 0


isMarked square =
    case square of
        Marked _ ->
            True

        _ ->
            False


isUnmarked square =
    case square of
        Unmarked _ ->
            True

        _ ->
            False


hasBingoColumn board =
    let
        columnsCount =
            List.length <| Maybe.withDefault [] <| List.head board
    in
    hasBingoColumnHelp columnsCount 0 board


hasBingoColumnHelp length index board =
    if index >= length then
        False

    else
        let
            columnSquares =
                board
                    |> List.map Array.fromList
                    |> List.map (Array.get index)
                    |> List.map (Maybe.withDefault Invalid)
        in
        if List.all isMarked columnSquares then
            True

        else
            hasBingoColumnHelp length (index + 1) board



-- RESULT CALCULATION


sumOfUnmarked board =
    sumOfUnmarkedHelp 0 board


sumOfUnmarkedHelp sum board =
    case board of
        [] ->
            sum

        row :: remainingRows ->
            sumOfUnmarkedHelp (sum + sumOfUnmarkedRow row) remainingRows


sumOfUnmarkedRow row =
    row
        |> List.filter isUnmarked
        |> List.map getUnmarkedN
        |> List.foldl (+) 0


getUnmarkedN square =
    case square of
        Unmarked n ->
            n

        _ ->
            0



-- PART 2


part2 : List String -> String
part2 input =
    "TODO"



-- RUN


main =
    Helpers.solve part1 part2
