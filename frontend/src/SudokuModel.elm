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
                                                                 -- ( Int, 0 ) for Edit en Frozen fields and ( Int, Int ) for Options
        , highlight : Maybe Int
        , retrieving : Bool
    }


type Field =
    Edit (Maybe Int)
    | Frozen Int
    | Options (Array (Maybe Int))


type Focus =
    FocusBlurred
    | Focus Int Int


sudokuExample : String
--sudokuExample = "004300209005009001070060043006002087190007400050083000600000105003508690042910300"
sudokuExample = String.repeat 81 "0"


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
        
        Focus fieldFocus _ ->
            Array.get fieldFocus fields


fieldNumberToFocus : Array Field -> ( Int, Int ) -> Focus
fieldNumberToFocus fields ( fieldNumber, fieldOptionNumber ) =
    Focus fieldNumber fieldOptionNumber

focusToFieldOption : Focus -> Maybe Int
focusToFieldOption focus =
    case focus of
        FocusBlurred ->
            Nothing
        
        Focus _ fieldOption ->
            Just fieldOption


focusOnField : Focus -> Int -> Focus
focusOnField focus fieldNumber =
    case focus of
        FocusBlurred ->
            FocusBlurred
        
        Focus focusFieldNumber focusFieldOptionNumber ->
            if (focusFieldNumber == fieldNumber) then
                Focus focusFieldNumber focusFieldOptionNumber
            else
                FocusBlurred

focusToFieldValue : Array Field -> Focus -> Maybe Int
focusToFieldValue fields focus =
    case focusToField fields focus of
        Just (Edit maybeValue) ->
            maybeValue

        Just (Frozen value) ->
            Just value

        Just (Options options) ->
            case focusToFieldOption focus of
                Just value ->
                    Array.get value options |> maybeJoin
                _ ->
                    Nothing

        Nothing ->
            Nothing




isHighlighted : Maybe Int -> Int -> Bool
isHighlighted highlight value =
    case highlight of
        Nothing ->
            False

        Just highlightValue ->
            highlightValue == value


fieldsFilledOut : Array Field -> Bool
fieldsFilledOut fields =
    Array.foldl fieldFilledOut True fields


fieldFilledOut : Field -> Bool -> Bool
fieldFilledOut field filled =
    filled
    &&
    case field of
        Edit Nothing ->
            False
        Edit (Just _) ->
            True
        Frozen _ ->
            True
        Options _ ->
            False


-- ####
-- ####   HELPER
-- ####

maybeJoin : Maybe ( Maybe a) -> Maybe a
maybeJoin maybeMaybeA =
    case maybeMaybeA of
        Just (Just value) ->
            Just value

        _ ->
            Nothing


setField : Int -> Array Field -> Field -> Array Field
setField fieldNumber fields field  =
    Array.set fieldNumber field fields


initOptions : Maybe Int -> Array ( Maybe Int )
initOptions maybeValue =
    case maybeValue of
        Just value ->
            Array.repeat 9 Nothing |> Array.set 0 (Just value)

        Nothing ->
            Array.repeat 9 Nothing

setOption : Int -> Int -> Array (Maybe Int) -> Array (Maybe Int)
setOption field value options =
    -- value present in options?
    case Array.toList options |> List.filter (\val -> val == Just value) of
        [] ->
            Array.set field (Just value) options

        -- if value present :
        _ ->
            options

getOption : Int -> Array (Maybe Int) -> Maybe Int
getOption field options =
    Array.get field options |> maybeJoin

clearOption : Int -> Array (Maybe Int) -> Array (Maybe Int)
clearOption field options =
    -- value present in options?
    Array.set field Nothing options


optionsIsEmpty : Array (Maybe Int) -> Bool
optionsIsEmpty options =
    options |> Array.filter (\option -> option /= Nothing) |> Array.isEmpty

clearFields : Array Field -> Array Field
clearFields fields =
    Array.map clearField fields

clearField : Field -> Field
clearField field =
    case field of
        Edit _ ->
            Edit Nothing
        Options _ ->
            Edit Nothing
        Frozen _ ->
            field

