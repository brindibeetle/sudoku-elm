module Sudoku exposing (..)

import Dict exposing (Dict)
import SudokuModel exposing (..)
import SudokuFaults exposing (..)
import Domain.SudokuQuiz exposing (..)

import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Session exposing (..)
import Bootstrap.Form as Form
import Bootstrap.Table as Table exposing (CellOption)
import Bootstrap.Button as Button
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Fieldset as Fieldset
import Array exposing (..)
import RemoteData exposing (RemoteData, WebData, succeed)


init : Session -> ( Model, Cmd Msg )
init session =
    (
        { fields = 
            String.toList sudokuExample 
            |> List.map charToCijfer
            |> List.map 
                ( \maybeInt -> 
                    case maybeInt of
                        Just int ->
                            Frozen int
                        Nothing ->
                            Edit Nothing
                )
            |> Array.fromList
        , focus = FocusBlurred
        , faults = initFaults
        , highlight = Nothing
        }
        , getRandomSudokuQuiz SudokuQuizReceived session )


-- ####
-- ####   VIEW
-- #### 


view : Model -> Html Msg
view model =
    let
        isSolved = solved model.fields model.faults
    in
        div [ class "center" ]
            [ viewExplanations
             , viewSudoku model isSolved
             , viewMessage isSolved
             , viewButtons
            ]

viewExplanations : Html Msg
viewExplanations =
    div [ class "center-explanations"]
        [ div [ class "alert alert-warning alert-dismissible fade show" ]
            [ viewExplainItem [ viewExplainStrong ("navigate"), viewExplainText ( " - use the arrow keys to navigate through the grid." ) ]
            , viewExplainItem [ viewExplainStrong ("edit"), viewExplainText ( " - a number, backspace, space, delete changes the value in a cell." ) ]
            , viewExplainItem
                [ viewExplainText ("toggle ")
                , viewExplainStrong ("options")
                , viewExplainText (" with the letter 'o', ")
                , viewExplainStrong ("highlight")
                , viewExplainText(" the current value with 'h'.")
                --, show all ")
                --, viewExplainStrong ("possible")
                --, viewExplainText(" values with 'p'.")
                ]
            ]
        ]

viewExplainStrong : String -> Html Msg
viewExplainStrong item =
    strong [] [ (text item) ]

viewExplainText : String -> Html Msg
viewExplainText description =
    text description

viewExplainItem : List(Html Msg) -> Html Msg
viewExplainItem items =
    div [] items

viewMessage : Bool -> Html Msg
viewMessage isSolved =
    if isSolved then
        div [ class "center-message" ]
            [ text "Congratulations! Try a new one?" ]
    else
        div [ class "center-message" ]
            [ text "" ]


viewButtons : Html Msg
viewButtons =
        div [ class "center-buttons" ]
            [ Button.button [ Button.attrs [ class "button" ], Button.onClick ButtonNew ] [ text "New Quiz" ]
            , Button.button [ Button.attrs [ class "button" ], Button.onClick ButtonClear ] [ text "Clear" ]
            ]


viewSudoku : Model -> Bool -> Html Msg
viewSudoku model isSolved =
    div [ class "center-sudoku"]
        [ div [ class "sudoku" ]
            (viewCells model isSolved )
        ]

viewCells : Model -> Bool -> List (Html Msg)
viewCells model isSolved =
    Array.initialize 81 (viewCell model isSolved)
    |> Array.toList


viewCell : Model -> Bool -> Int -> Html Msg
viewCell model isSolved fieldNumber =
    let
        borders = getBorders fieldNumber
        field = modelToField model.fields fieldNumber
        focus = focusOnField model.focus fieldNumber
        fault = getFault fieldNumber model.faults
        cssArgs = { css = borders, field = field, isFocus = (focus /= FocusBlurred), isFault = fault, isSolved = isSolved }
        (faultEdit, faultFrozen) = if getFault fieldNumber model.faults then (" fault-edit"," fault-frozen") else ("", "")
    in
        case ( field, focus ) of
        -- focusOnField returns FocusBlurred if focus NOT ON FIELD
            ( Frozen value, Focus _ _ ) ->
                div
                    [ getCellClasses cssArgs (isHighlighted model.highlight value) value, onClick ( FocusChanged fieldNumber ) ]
                    [ value |> String.fromInt |> text ]

            ( Frozen value, FocusBlurred ) ->
                div
                    [ getCellClasses cssArgs (isHighlighted model.highlight value) value, onClick ( FocusChanged fieldNumber ) ]
                    [ value |> String.fromInt |> text ]

            ( Edit (Just value), Focus _ _) ->
                div
                    [ getCellClasses cssArgs (isHighlighted model.highlight value) value ]
                    [ value |> String.fromInt |> text ]

            ( Edit Nothing, Focus _ _ ) ->
                div
                    [ getCellClasses cssArgs False 0 ]
                    [ text " " ]

            ( Edit (Just value), FocusBlurred ) ->
                div
                    [ getCellClasses cssArgs (isHighlighted model.highlight value) value, onClick ( FocusChanged fieldNumber ) ]
                    [ value |> String.fromInt |> text ]

            ( Edit Nothing, FocusBlurred ) ->
                div
                    [ getCellClasses cssArgs False 0, onClick ( FocusChanged fieldNumber ) ]
                    [ text " " ]

            ( Options options, focus1 ) ->
                div
                    [ class (borders ++ " options" ) ]
                    ( viewOptions options model.faults focus1 fieldNumber )


getBorders : Int -> String
getBorders fieldNumber =
    let
        row = fieldNumber // 9
        column = modBy 9 fieldNumber
    in
        ( if modBy 3 row == 0 then
            "fat-border-top"
        else if modBy 3 row == 2 then
            "fat-border-bottom"
        else
            "normal-border-vertical"
         )
        ++
        " "
        ++
        ( if modBy 3 column == 0 then
            "fat-border-left"
        else if modBy 3 column == 2 then
            "fat-border-right"
        else
            "normal-border-horizontal"
        )

getCellClasses : { css : String, field : Field, isFocus : Bool, isFault : Bool, isSolved : Bool } -> Bool -> Int ->  Attribute Msg
getCellClasses { css, field, isFocus, isFault, isSolved } isHighlight value =
    class
        ( "cell"
            ++ " " ++ css
            ++ " "
                ++ ( case field of
                        Frozen _ ->
                            "frozen"
                        Edit _ ->
                            "edit"
                        Options _ ->
                            "options"
                    )
            ++ " " ++ ( if isFocus then "focus" else ""  )
            ++ " " ++ ( if isFault then "fault" else ""  )
            ++ " " ++ ( if isHighlight then "highlight" else ""  )
            ++ " " ++ ( if isSolved then "animation" ++ String.fromInt value else "" )
         )


viewOptions : Array (Maybe Int) -> Faults -> Focus -> Int -> List(Html Msg)
viewOptions options faults focus fieldNumber =
    Array.initialize 9 (viewOption options faults focus fieldNumber )
        |> Array.toList
    -- (getOptions field model.optionCijfers |> Maybe.withDefault Array.empty |> Array.toList |> List.map (viewOption model isFocus) )


-- div [ class "option" ] [ text (String.fromInt option) ]))
viewOption : Array (Maybe Int) -> Faults -> Focus -> Int -> Int -> Html Msg
viewOption options faults focus fieldNumber optionNumber =
    let
        option = options |> Array.get optionNumber |> maybeJoin
        isFocus = ( focus == Focus fieldNumber optionNumber )
        isFault = getOptionFault fieldNumber optionNumber faults
    in
        case option of
           Just optionValue ->
               div [ getOptionClasses optionNumber isFocus isFault, onClick ( OptionFocusChanged fieldNumber optionNumber) ] [ text (String.fromInt optionValue) ]

           Nothing ->
                div [ getOptionClasses optionNumber isFocus isFault, onClick ( OptionFocusChanged fieldNumber optionNumber) ] [ text "" ]


getOptionClasses : Int -> Bool -> Bool -> Attribute Msg
getOptionClasses optionNumber isFocus isFault =
    class
        ( "option"
            ++ " " ++ "option" ++ String.fromInt optionNumber
            ++ ( if isFocus then " focus" else ""  )
            ++ ( if isFault then " fault" else ""  )
        )


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
     

getFrozen : Int -> Array Bool -> Bool
getFrozen field frozens =
    Array.get field frozens |> Maybe.withDefault False


getCijfer : Int -> Array (Maybe Int) -> Maybe Int
getCijfer field cijfers =
    Array.get field cijfers |> maybeJoin


getCijferString : Maybe Int -> String
getCijferString maybeCijfer =
    case maybeCijfer of
        Just cijfer ->
            String.fromInt cijfer
        
        Nothing ->
            ""


setField : Int -> Array Field -> Field -> Array Field
setField fieldNumber fields field  =
    Array.set fieldNumber field fields


getOptions : Maybe Int -> Array (Array Int) -> Array Int
getOptions maybeField options =
    case maybeField of
        Just field ->
            Array.get field options |> Maybe.withDefault Array.empty

        Nothing ->
            Array.empty


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


addOption : Int -> Maybe Int -> Array (Array Int) -> Array (Array Int)
addOption field maybeCijfer optionCijfers =
    case maybeCijfer of
        Just cijfer ->
            let
                optionCijfers1 = Array.get field optionCijfers |> Maybe.withDefault Array.empty
            in
                -- no doubles
                if optionCijfers1 |> Array.filter (\option -> option == cijfer) |> Array.isEmpty then
                    Array.set field ( optionCijfers1 |> Array.push cijfer) optionCijfers
                else
                    optionCijfers
        Nothing ->
            optionCijfers


optionsIsEmpty : Array (Maybe Int) -> Bool
optionsIsEmpty options =
    options |> Array.filter (\option -> option /= Nothing) |> Array.isEmpty


generateOptions : Array Field -> Int -> Field
generateOptions fields fieldNumber =
    let
        options =
            List.range 1 9
                |> List.filter (valueOkayOrNot fields fieldNumber)
    in
        case options of
            [] ->
                Edit Nothing
            [ i ] ->
                Edit (Just i)
            ii ->
                List.map (\i -> Just i) ii
                |> (\maybeImaybeI -> List.append maybeImaybeI (List.repeat 7 Nothing))
                |> List.take 9
                |> Array.fromList
                |> Options


maybeComparison : Maybe Int -> Maybe Int -> Order
maybeComparison maybeI maybeJ =
    case ( maybeI, maybeJ ) of
        ( Just i, Just j ) ->
            compare i j
        ( Just i, Nothing ) ->
            LT
        ( Nothing, Just j ) ->
            GT
        ( Nothing, Nothing ) ->
            EQ


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


solved : Array Field -> Dict String (Dict (Int, Int) Bool) -> Bool
solved fields faults =
    fieldsFilledOut fields && noFaultsDetected faults
    --|| True

-- ####
-- ####   MOVE
-- #### 

type Direction =
    North       -- Up
    | South     -- Down
    | West      -- Left
    | East      -- Right


moveField : ( Int, Int ) -> Direction -> ( Int, Int )
moveField (fieldNumber, fieldOptionNumber) direction =
    case direction of
        North ->
            if (fieldOptionNumber == 0 || fieldOptionNumber == 1 || fieldOptionNumber == 2) then
                ( fieldNumber - 9 |> modBy 81, fieldOptionNumber + 6 )
            else
                ( fieldNumber, fieldOptionNumber - 3 )

        South ->
            if (fieldOptionNumber == 6 || fieldOptionNumber == 7 || fieldOptionNumber == 8) then
                ( fieldNumber + 9 |> modBy 81, fieldOptionNumber - 6 )
            else
                ( fieldNumber, fieldOptionNumber + 3 )

        East ->
            if (fieldOptionNumber == 2 || fieldOptionNumber == 5 || fieldOptionNumber == 8) then
                ( if ( fieldNumber + 1 |> modBy 9 ) == 0 then fieldNumber - 8 else fieldNumber + 1 |> modBy 81
                , fieldOptionNumber - 2 )
            else
                ( fieldNumber, fieldOptionNumber + 1 )

        West ->
            if (fieldOptionNumber == 0 || fieldOptionNumber == 3 || fieldOptionNumber == 6) then
                ( if ( fieldNumber - 1 |> modBy 9 ) == 8 then fieldNumber + 8 else fieldNumber - 1 |> modBy 81
                , fieldOptionNumber + 2 )
            else
                ( fieldNumber, fieldOptionNumber - 1 )

moveFocus : Field -> Focus -> Direction -> Array Field -> Focus
moveFocus field focus direction fields =
    let
        ( fieldNumber1, fieldOptionNumber1 ) =
            case ( field, focus, direction ) of
                ( Options _, Focus fieldNumber fieldOptionNumber, dir ) ->
                    moveField (fieldNumber, fieldOptionNumber ) dir

                ( _, Focus fieldNumber _, North ) ->
                    moveField (fieldNumber, 0 ) North

                ( _, Focus fieldNumber _, South ) ->
                    moveField (fieldNumber, 6 ) South

                ( _, Focus fieldNumber _, West ) ->
                    moveField (fieldNumber, 0 ) West

                ( _, Focus fieldNumber _, East ) ->
                    moveField (fieldNumber, 2 ) East

                ( _, FocusBlurred, dir ) ->
                    ( 0, 0 ) -- something went wrong seriously
    in
        fieldNumberToFocus fields (fieldNumber1, fieldOptionNumber1)


-- ####
-- ####   UPDATE
-- #### 


type Msg =
    ValueChanged Int
    | ValueCleared
    | FocusChanged Int
    | OptionValueChanged Int
    | OptionFocusChanged Int Int
    | MovedFocus Direction
    | OptionsToggled
    | MsgNone
    | SudokuQuizReceived (WebData SudokuQuiz)
    | HighLighted
    | Possibilities
    | ButtonNew
    | ButtonClear


update : Msg -> Model -> Session -> { model : Model, session : Session, cmd : Cmd Msg } 
update msg model session =
    let
        focusField = focusToField model.fields model.focus
        -- focusOptionField = focusToFieldOption model.focus
    in
        case ( msg, model.focus, focusField ) of
            ( ValueChanged value, Focus fieldNumber _, Just (Edit _ )) ->
                let
                    fields = Edit (Just value) |> setField fieldNumber model.fields
                in
                    { model = 
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            -- Option changed value
            ( ValueChanged value, Focus fieldNumber fieldOptionNumber, Just (Options options) ) ->
                let
                    fields = setOption fieldOptionNumber value options |> Options |> setField fieldNumber model.fields
                in
                    { model = 
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        -- , focus = Just (modBy 81 (field + 1))
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( ValueCleared, Focus fieldNumber fieldOptionNumber, Just(Options options) ) ->
                let
                    fields = clearOption fieldOptionNumber options |> Options |> setField fieldNumber model.fields
                in
                    { model =
                        { model
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( ValueCleared, Focus fieldNumber _, _ ) ->
                let
                    fields = Edit Nothing |> setField fieldNumber model.fields
                in
                    { model =
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        }
                    , session = session
                    , cmd = Cmd.none
                    }
            

            ( FocusChanged fieldNumber, Focus fieldNumberOld _, Just (Options options) ) ->
                --when leaving an empty Options field, change it to Editfield
                let
                    fields =
                        if optionsIsEmpty options then
                            setField fieldNumberOld model.fields (Edit Nothing)
                        else
                            model.fields
                in
                    { model =
                        { model
                        | fields = fields
                        , focus = fieldNumberToFocus model.fields (fieldNumber, 0)
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( FocusChanged fieldNumber, _, _ ) ->
                { model =
                    { model
                    | focus = fieldNumberToFocus model.fields (fieldNumber, 0)
                    }
                , session = session
                , cmd = Cmd.none
                }

            ( MovedFocus dir, Focus fieldNumberOld fieldNumberOptionOld, Just (Options options) ) ->
                --when leaving an empty Options field, change it to Editfield
                let
                    oldfocus = Focus fieldNumberOld fieldNumberOptionOld
                    field = Options options
                    focus = moveFocus field oldfocus dir model.fields
                    fields =
                        case focus of
                            Focus fieldNumber _ ->
                                if optionsIsEmpty options && fieldNumber /= fieldNumberOld then
                                    setField fieldNumberOld model.fields (Edit Nothing)
                                else
                                    model.fields
                            _ ->
                                model.fields
                in
                    { model =
                        { model
                        | fields = fields
                        , focus = focus
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( MovedFocus dir, focus, Just field ) ->
                { model =
                    { model
                    | focus = moveFocus field focus dir model.fields
                    }
                , session = session
                , cmd = Cmd.none
                }

            ( OptionsToggled, Focus fieldNumber _, Just (Edit maybeValue) ) ->
                let
                    fields = initOptions maybeValue |> Options |> setField fieldNumber model.fields
                in
                    { model = 
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , focus = Focus fieldNumber 0
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( OptionsToggled, Focus fieldNumber fieldOptionNumber, Just (Options options) ) ->
                let
                    fields = Edit (getOption fieldOptionNumber options ) |> setField fieldNumber model.fields
                in
                    { model = 
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , focus = Focus fieldNumber 0
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( OptionFocusChanged fieldNumber optionFieldNumber, _, _ ) ->
                { model = 
                    { model 
                    | focus = Focus fieldNumber optionFieldNumber
                    }
                , session = session
                , cmd = Cmd.none
                }
        -- ( OptionChanged value, Just field ) ->
        --     let
        --         cijfer = getCijfer field model.cijfers
        --     in
        --     { model = 
        --         { model 
        --         | cijfers = setCijfer field Nothing model.cijfers
        --         , options = setOption field True model.options
        --         , optionCijfers = addOption field cijfer model.optionCijfers |> addOption field (Just value )
        --         }
        --     , session = session
        --     , cmd = Cmd.none
        --     }

            ( SudokuQuizReceived sudokuQuizWebData, _, _ ) ->
                case sudokuQuizWebData of
                    RemoteData.Success sudokuQuiz ->
                        { model = 
                            { model
                            | fields =
                                String.toList sudokuQuiz.quiz 
                                |> List.map charToCijfer 
                                |> List.map 
                                    ( \maybeInt -> 
                                        case maybeInt of
                                            Just int ->
                                                Frozen int
                                            Nothing ->
                                                Edit Nothing
                                    )
                                |> Array.fromList
                            , focus = FocusBlurred
                            , faults = initFaults
                            }
                            , session = session, cmd = Cmd.none }
                        
                    _ ->
                        { model = model, session = session, cmd = Cmd.none }

            ( HighLighted, _, Just (Edit maybeValue)) ->
                if model.highlight == maybeValue then
                    { model =
                        { model
                        | highlight = Nothing
                        }
                        , session = session, cmd = Cmd.none
                    }
                else
                    { model =
                        { model
                        | highlight = maybeValue
                        }
                        , session = session, cmd = Cmd.none
                    }

            ( HighLighted, _, Just (Frozen value)) ->
                if model.highlight == Just value then
                    { model =
                        { model
                        | highlight = Nothing
                        }
                        , session = session, cmd = Cmd.none
                    }
                else
                    { model =
                        { model
                        | highlight = Just value
                        }
                        , session = session, cmd = Cmd.none
                    }

            ( Possibilities, Focus fieldNumber _, Just (Edit _) ) ->
                let
                    fields = generateOptions model.fields fieldNumber |> setField fieldNumber model.fields
                in
                    { model =
                        { model
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , focus = fieldNumberToFocus fields (fieldNumber, 0)
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( Possibilities, Focus fieldNumber _, Just (Options _) ) ->
                let
                    fields = generateOptions model.fields fieldNumber |> setField fieldNumber model.fields
                in
                    { model =
                        { model
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , focus = fieldNumberToFocus fields (fieldNumber, 0)
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( ButtonClear, _, _ ) ->
                { model =
                    { model
                    | fields = clearFields model.fields
                    , focus = FocusBlurred
                    , highlight = Nothing
                    , faults = initFaults
                    }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( ButtonNew, _, _ ) ->
                let
                    ( model1, cmd ) = init session
                in
                    { model = model1
                    , session = session
                    , cmd = cmd
                    }

            ( _, _, _ ) ->
                { model = model, session = session, cmd = Cmd.none }


-- ####
-- ####   SUBSCRIPTION
-- #### 


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey ( Decode.field "key" Decode.string )


toKey : String -> Msg
toKey keyValue =
    case keyValue of
        "ArrowRight" ->
            MovedFocus East
        "ArrowLeft" ->
            MovedFocus West
        "ArrowUp" ->
            MovedFocus North
        "ArrowDown" ->
            MovedFocus South
        "Backspace" ->
            ValueCleared
        "Delete" ->
            ValueCleared

        _ ->
            case String.uncons keyValue of
                Just ( char, _ ) ->
                    toKeyChar char

                _ ->
                    MsgNone


toKeyChar : Char -> Msg
toKeyChar char =
    case char of
        '1' -> ValueChanged 1 
        '2' -> ValueChanged 2 
        '3' -> ValueChanged 3 
        '4' -> ValueChanged 4 
        '5' -> ValueChanged 5 
        '6' -> ValueChanged 6 
        '7' -> ValueChanged 7 
        '8' -> ValueChanged 8 
        '9' -> ValueChanged 9 
        ' ' -> ValueCleared 
        'O' -> OptionsToggled
        'o' -> OptionsToggled
        'h' -> HighLighted
        'H' -> HighLighted
        --'p' -> Possibilities
        --'P' -> Possibilities
        _ -> MsgNone


                
