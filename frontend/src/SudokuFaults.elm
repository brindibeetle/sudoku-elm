module SudokuFaults exposing (initFaults, recomputeFaults, getFault, getOptionFault)

import SudokuModel exposing (..)

import Array exposing (..)
import Dict exposing (..)

initFaults : Dict String ( Dict ( Int, Int) Bool )
initFaults = Dict.fromList [ ("column", Dict.empty), ("row", Dict.empty), ("block", Dict.empty) ]

recomputeFaults : Array Field -> Int -> Dict String (Dict (Int, Int) Bool) -> Dict String (Dict (Int, Int) Bool) 
recomputeFaults fields fieldNumber faults =
    Dict.map 
        (\check subFaults ->
            let
                fieldNumbers = fieldToEffected fieldNumber check        -- fieldToEffected : Int -> String -> Array Int
            in
                fieldNumbers       
                |> fieldsToValueDictionary fields                       -- fieldsToValueDictionary : Array Field -> Array Int -> Dict Int ( Array Int )
                |> valuesDictionaryToFaults fields fieldNumbers subFaults
                                -- valuesDictionaryToFaults : Array Field -> Array Int -> Dict Int Bool -> Dict Int ( Array Int ) -> Dict Int Bool
        )
        faults

--
-- when a field is changed, all fields in the row, column and block are effected
fieldToEffected : Int -> String -> Array Int
fieldToEffected fieldNumber check =
    case check of
        "column" ->
            let
                topMostOfColumn = modBy 9 fieldNumber
            in
                Array.initialize 9 (\i -> topMostOfColumn + i * 9)

        "row" ->
            let
                leftMostOfRow = ( fieldNumber // 9 ) * 9
            in
                Array.initialize 9 (\i -> leftMostOfRow + i)

        "block" ->
            let
                leftMostOfTopRow = ( fieldNumber // 27 ) * 27
                topMostOfLeftColumn = modBy 9 (( fieldNumber // 3 ) * 3)
                topLeftMostOfBlock = leftMostOfTopRow + topMostOfLeftColumn
            in
                Array.initialize 9 (\i -> topLeftMostOfBlock + modBy 3 i + ( i // 3 * 9 ))
        
        _ ->
            Array.empty

--
-- Helper function that returns the value of a field
fieldToValue : Array Field -> Int -> Maybe Int
fieldToValue fields fieldNumber =
    case Array.get fieldNumber fields of
        Just (Edit (Just value)) ->
            Just value
        Just (Frozen value) ->
            Just value
        _ ->
            Nothing

--
-- Dictionary value as key and the fields as an array in the value
fieldsToValueDictionary : Array Field -> Array Int -> Dict Int ( Array Int )
fieldsToValueDictionary fields fieldNumbers =
    Array.foldl (fieldToValueDictionary fields) Dict.empty fieldNumbers


fieldToValueDictionary : Array Field -> Int -> Dict Int ( Array Int ) -> Dict Int ( Array Int )
fieldToValueDictionary fields fieldNumber valueDictionary =
    case fieldToValue fields fieldNumber of
        Just value ->
            case Dict.get value valueDictionary of
                Just fieldsOfValue ->
                    let
                        fieldsOfValueUpdated =  fieldsOfValue |> Array.push fieldNumber
                    in
                        Dict.insert value fieldsOfValueUpdated valueDictionary
                Nothing ->
                    Dict.insert value ( Array.fromList [fieldNumber] ) valueDictionary

        Nothing ->
            valueDictionary


--
-- with the Dictionary of value fields we can determine the faults of the fields
valuesDictionaryToFaults : Array Field -> Array Int -> Dict (Int, Int) Bool -> Dict Int ( Array Int ) -> Dict (Int, Int) Bool
valuesDictionaryToFaults fields fieldNumbers faults valueDictionary =
    Array.foldl (valueDictionaryToFaults fields valueDictionary) faults fieldNumbers


valueDictionaryToFaults : Array Field -> Dict Int ( Array Int ) -> Int -> Dict (Int, Int) Bool -> Dict (Int, Int) Bool
valueDictionaryToFaults fields valueDictionary fieldNumber faults =
    case Array.get fieldNumber fields of
        Just (Edit Nothing) ->
            Dict.remove (fieldNumber, 0) faults
        
        Just (Edit (Just value)) ->
            if ( ( Dict.get value valueDictionary |> Maybe.withDefault Array.empty |> Array.length ) > 1 ) then
                Dict.insert (fieldNumber, 0) True faults
            else
                Dict.remove (fieldNumber, 0) faults

        Just (Frozen value) ->
            if ( ( Dict.get value valueDictionary |> Maybe.withDefault Array.empty |> Array.length ) > 1 ) then
                Dict.insert (fieldNumber, 0) True faults
            else
                Dict.remove (fieldNumber, 0) faults

        Just (Options options) ->
            Array.foldl (valueDictionaryOptionToFault valueDictionary fieldNumber options) faults optionNumbers

        _ ->
            faults


optionNumbers : Array Int
optionNumbers = Array.initialize 9 identity 


valueDictionaryOptionToFault : Dict Int ( Array Int ) -> Int -> Array (Maybe Int) -> Int -> Dict (Int, Int) Bool -> Dict (Int, Int) Bool
valueDictionaryOptionToFault valueDictionary fieldNumber options optionNumber faults =
    case Array.get optionNumber options of
        Just (Just value) ->
            if ( ( Dict.get value valueDictionary |> Maybe.withDefault Array.empty |> Array.length ) > 0 ) then
                Dict.insert (fieldNumber, optionNumber) True faults
            else
                Dict.remove (fieldNumber, optionNumber) faults

        _ ->
            Dict.remove (fieldNumber, optionNumber) faults




getFault : Int -> Dict String (Dict (Int, Int) Bool) -> Bool
getFault fieldNumber faults =
    Dict.foldl 
        (\key dict fault -> 
            fault || ( Dict.get (fieldNumber, 0) dict |> Maybe.withDefault False )
        )
        False
        faults


getOptionFault : Int -> Int -> Dict String (Dict (Int, Int) Bool) -> Bool
getOptionFault fieldNumber optionNumber faults =
    Dict.foldl 
        (\key dict fault -> 
            fault || ( Dict.get (fieldNumber, optionNumber) dict |> Maybe.withDefault False )
        )
        False
        faults
