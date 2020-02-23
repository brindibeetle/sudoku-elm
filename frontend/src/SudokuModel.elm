module SudokuModel exposing (..)

import Array exposing (..)
import Dict exposing (..)

-- -- sudoku fieldNumbers
-- --  0  1  2  3  4  5  6  7  8
-- --  9 10 11 12 13 14 15 16 17
-- -- 18 19 20 21 22 23 24 25 26
-- -- 27 28 29 30 31 32 33 34 35
-- -- 36 37 38 39 40 41 42 43 44
-- -- 45 46 47 48 49 50 51 52 53
-- -- 54 55 56 57 58 59 60 61 62
-- -- 63 64 65 66 67 68 69 70 71
-- -- 72 73 74 75 76 77 78 79 80


type alias Model =
    {
        fields : Array Field
        , focus : Focus
        , faults : Dict String ( Dict ( Int, Int ) Bool )        -- String is here column, row or block, 
    }                                                            -- ( Int, 0 ) for Edit en Frozen fields and ( Int, Int ) for Options

type Field =
    Edit (Maybe Int)
    | Frozen Int
    | Options (Array (Maybe Int))


type Focus =
    FocusBlurred
    | FocusEdit Int
    | FocusOptions Int Int


sudokuExample : String
sudokuExample = "004300209005009001070060043006002087190007400050083000600000105003508690042910300"


charToCijfer : Char -> Maybe Int
charToCijfer char =
    let
        offset =  Char.toCode char - Char.toCode '0'
    in
    if offset <= 0 then
        Nothing
    else
        if offset <= 9 then
            Just offset
        else
            Nothing

modelToField : Array Field -> Int -> Field
modelToField fields field =
    Array.get field fields |> Maybe.withDefault (Frozen 0)


focusToField : Array Field -> Focus -> Maybe Field
focusToField fields focus =
    case focus of
        FocusBlurred ->
            Nothing
        
        FocusEdit fieldFocus ->
            Array.get fieldFocus fields

        FocusOptions fieldFocus _ -> 
            Array.get fieldFocus fields


fieldNumberToFocus : Array Field -> ( Int, Int ) -> Focus
fieldNumberToFocus fields ( fieldNumber, fieldOptionNumber ) =
    case Array.get fieldNumber fields of
        Just (Edit _) ->
            FocusEdit fieldNumber
        
        Just (Options _) ->
            FocusOptions fieldNumber fieldOptionNumber

        _ ->
            FocusBlurred

focusToFieldOption : Focus -> Maybe Int
focusToFieldOption focus =
    case focus of
        FocusBlurred ->
            Nothing
        
        FocusEdit _ ->
            Nothing

        FocusOptions _ fieldOption -> 
            Just fieldOption


focusOnField : Focus -> Int -> Focus
focusOnField focus fieldNumber =
    case focus of
        FocusBlurred ->
            FocusBlurred
        
        FocusEdit focusFieldNumber ->
            if (focusFieldNumber == fieldNumber) then
                FocusEdit focusFieldNumber
            else
                FocusBlurred

        FocusOptions focusFieldNumber focusFieldOptionNumber -> 
            if (focusFieldNumber == fieldNumber) then
                FocusOptions focusFieldNumber focusFieldOptionNumber
            else
                FocusBlurred

