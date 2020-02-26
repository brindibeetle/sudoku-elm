module Sudoku exposing (..)

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
import Bootstrap.Table as Table
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
        , buttonClicked = Nothing
        }
        , getRandomSudokuQuiz SudokuQuizReceived session )



-- ####
-- ####   VIEW
-- #### 


view : Model -> Html Msg
view model =
    div [ class  "center" ]
        [ viewExplanations
         , viewSudoku model
         , viewButtons model.buttonClicked
        ]

viewExplanations : Html Msg
viewExplanations =
    div [ class "center-explainations"]
        [ div [ class "alert alert-warning alert-dismissible fade show" ]
            [ viewExplainItem [ viewExplainStrong ("navigate"), viewExplainText ( " - use the arrow keys to navigate through the grid." ) ]
            , viewExplainItem [ viewExplainStrong ("edit"), viewExplainText ( " - a number, backspace, space, delete changes the value in a cell." ) ]
            , viewExplainItem
                [ viewExplainText ("toggle ")
                , viewExplainStrong ("options")
                , viewExplainText (" with the letter 'o', ")
                , viewExplainStrong ("highlight")
                , viewExplainText(" the current value with 'h', show all ")
                , viewExplainStrong ("possible")
                , viewExplainText(" values with 'p'.")
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

viewButtons : Maybe ButtonClicked -> Html Msg
viewButtons buttonClicked =
    case buttonClicked of
        Nothing ->
            div [ class "center-buttons"]
                [ Button.button [ Button.attrs [ class "button" ], Button.onClick ButtonNew1 ] [ text "New Quiz" ]
                , Button.button [ Button.attrs [ class "button" ], Button.onClick ButtonClear1 ] [ text "Clear" ]
                ]

        Just ButtonNew ->
            div [ class "center-buttons"]
                [ Button.button [ Button.attrs [ class "button" ], Button.onClick ButtonNew2 ] [ text "New Quiz" ]
                , Form.group []
                    [ Form.label [] [ text "Custom radios" ]
                    , Fieldset.config
                        |> Fieldset.children
                            ( Radio.radioList "customradiogroup"
                                [ Radio.createCustom [ Radio.id "rdi1", Radio.inline ] "Radio 1"
                                , Radio.createCustom [ Radio.id "rdi2", Radio.inline ] "Radio 2"
                                , Radio.createCustom [ Radio.id "rdi3", Radio.inline ] "Radio 3"
                                ]
                            )
                        |> Fieldset.view
                    ]
                ]

        Just ButtonClear ->
            div [ class "center-buttons"]
                [ Button.button [ Button.attrs [ class "button" ], Button.onClick ButtonNew1 ] [ text "New Quiz" ]
                , Button.button [ Button.attrs [ class "button" ], Button.onClick ButtonClear1 ] [ text "Clear" ]
                ]


viewSudoku : Model -> Html Msg
viewSudoku model =
    div [ class "center-sudoku"]
        [ Table.table
            { options = [ Table.attr ( class "table table-bordered sudoku" ) ]
            , thead = Table.thead [] []
            , tbody = Table.tbody [] ( viewRows model )
            }
        ]

viewRows : Model -> List (Table.Row Msg)
viewRows model =
    [
        viewRow model 0 "bordertop"
        , viewRow model 1 ""
        , viewRow model 2 "borderbottom"
        , viewRow model 3 "bordertop"
        , viewRow model 4 ""
        , viewRow model 5 "borderbottom"
        , viewRow model 6 "bordertop"
        , viewRow model 7 ""
        , viewRow model 8 "borderbottom"
    ]

viewRow : Model -> Int -> String -> Table.Row Msg
viewRow model row borders =
    let
        field0 = row * 9
    in
        Table.tr [] 
        [ 
            viewCell model field0 (borders ++ " borderleft")
            , viewCell model (field0 + 1) borders 
            , viewCell model (field0 + 2) (borders ++ " borderright")
            , viewCell model (field0 + 3) (borders ++ " borderleft")
            , viewCell model (field0 + 4) borders
            , viewCell model (field0 + 5) (borders ++ " borderright")
            , viewCell model (field0 + 6) (borders ++ " borderleft")
            , viewCell model (field0 + 7) borders
            , viewCell model (field0 + 8) (borders ++ " borderright")
        ]


viewCell : Model -> Int -> String -> Table.Cell Msg
viewCell model fieldNumber borders =
    let
        (faultEdit, faultFrozen) = if getFault fieldNumber model.faults then (" fault-edit"," fault-frozen") else ("", "")
    in
        case ( modelToField model.fields fieldNumber, focusOnField model.focus fieldNumber ) of
        -- focusOnField returns FocusBlurred if focus NOT ON FIELD
            ( Frozen value, FocusFrozen _ ) ->
                Table.td 
                    [ Table.cellAttr (class (borders ++ " focus-frozen" ++ faultFrozen ++ (getHighlight model.highlight value "frozen") )), Table.cellAttr( onClick ( FocusChanged fieldNumber )) ]
                    [ value |> String.fromInt |> text ]

            ( Frozen value, FocusBlurred ) ->
                Table.td
                    [ Table.cellAttr (class (borders ++ " frozen" ++ faultFrozen ++ (getHighlight model.highlight value "frozen") )), Table.cellAttr( onClick ( FocusChanged fieldNumber )) ]
                    [ value |> String.fromInt |> text ]

            ( Edit (Just value), FocusEdit _ ) ->
                Table.td 
                    [ Table.cellAttr (class (borders ++ " focus-edit" ++ faultEdit ++ (getHighlight model.highlight value "edit") )) ]
                    [ value |> String.fromInt |> text ]

            ( Edit Nothing, FocusEdit _ ) ->
                Table.td
                    [ Table.cellAttr (class (borders ++ " focus-edit" ++ faultEdit )) ]
                    [ text " " ]

            ( Edit (Just value), FocusBlurred ) ->
                Table.td 
                    [ Table.cellAttr (class (borders ++ " edit" ++ faultEdit ++ (getHighlight model.highlight value "edit") )), Table.cellAttr( onClick ( FocusChanged fieldNumber )) ]
                    [ value |> String.fromInt |> text ]

            ( Edit Nothing, FocusBlurred ) ->
                Table.td 
                    [ Table.cellAttr (class (borders ++ " edit" ++ faultEdit )), Table.cellAttr( onClick ( FocusChanged fieldNumber )) ]
                    [ text " " ]

            ( Options options, FocusOptions _ focusFieldOptionNumber ) ->
                Table.td 
                    [ Table.cellAttr (class (borders ++ " options focus" )) ] 
                    [ viewOptions model fieldNumber options (Just focusFieldOptionNumber) ]  

            ( Options options, FocusBlurred ) ->
                Table.td 
                    [ Table.cellAttr (class (borders ++ " options" )) ] 
                    [ viewOptions model fieldNumber options Nothing ]

            ( _, _) ->
                Table.td
                    [ Table.cellDanger ]
                    [ text "Something is wrong here "]

viewOptions : Model -> Int -> Array (Maybe Int) -> Maybe Int -> Html Msg
viewOptions model fieldNumber options optionFocus =
    div 
        [ class "options" ]
        [ viewOption model fieldNumber options optionFocus 0 
        , viewOption model fieldNumber options optionFocus 1
        , viewOption model fieldNumber options optionFocus 2
        , viewOption model fieldNumber options optionFocus 3
        , viewOption model fieldNumber options optionFocus 4
        , viewOption model fieldNumber options optionFocus 5
        , viewOption model fieldNumber options optionFocus 6
        , viewOption model fieldNumber options optionFocus 7
        , viewOption model fieldNumber options optionFocus 8
        ]
        -- (getOptions field model.optionCijfers |> Maybe.withDefault Array.empty |> Array.toList |> List.map (viewOption model isFocus) )


-- div [ class "option" ] [ text (String.fromInt option) ]))
viewOption : Model -> Int -> Array (Maybe Int) -> Maybe Int -> Int -> Html Msg
viewOption model fieldNumber options maybeOptionFocus optionNumber =
    let
        option = options |> Array.get optionNumber |> maybeJoin
        isFocus = ( Maybe.withDefault -1 maybeOptionFocus == optionNumber )
        optionFaultCss = if getOptionFault fieldNumber optionNumber model.faults then " fault" else ""
    in
        case ( option, isFocus ) of
           ( Just optionValue, False ) ->
                div [ class ("option" ++ optionFaultCss), onClick ( OptionFocusChanged fieldNumber optionNumber) ] [ text (String.fromInt optionValue) ]

           ( Just optionValue, True ) ->
                div [ class ("option-focus"  ++ optionFaultCss)] [ text (String.fromInt optionValue) ]

           ( Nothing, True ) ->
                div [ class "option-focus" ] [ text "" ]

           ( Nothing, False ) ->
                div [ class "option", onClick ( OptionFocusChanged fieldNumber optionNumber) ] [ text "" ]


getHighlight : Maybe Int -> Int -> String -> String
getHighlight highlight value postfix =
    if isHighlighted highlight value then
        " highlight-" ++ postfix
    else
        ""

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

moveFocus : Focus -> Direction -> Array Field -> Focus
moveFocus focus direction fields =
    let
        ( fieldNumber1, fieldOptionNumber1 ) =
            case ( focus, direction ) of
                ( FocusEdit fieldNumber, North ) ->
                    moveField (fieldNumber, 0 ) North

                ( FocusEdit fieldNumber, South ) ->
                    moveField (fieldNumber, 6 ) South

                ( FocusEdit fieldNumber, West ) ->
                    moveField (fieldNumber, 0 ) West

                ( FocusEdit fieldNumber, East ) ->
                    moveField (fieldNumber, 2 ) East

                ( FocusFrozen fieldNumber, North ) ->
                    moveField (fieldNumber, 0 ) North

                ( FocusFrozen fieldNumber, South ) ->
                    moveField (fieldNumber, 6 ) South

                ( FocusFrozen fieldNumber, West ) ->
                    moveField (fieldNumber, 0 ) West

                ( FocusFrozen fieldNumber, East ) ->
                    moveField (fieldNumber, 2 ) East

                ( FocusOptions fieldNumber fieldOptionNumber, dir ) ->
                    moveField (fieldNumber, fieldOptionNumber ) dir

                ( FocusBlurred, dir ) ->
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
    | ButtonNew1
    | ButtonNew2
    | ButtonClear1
    | ButtonClear2


update : Msg -> Model -> Session -> { model : Model, session : Session, cmd : Cmd Msg } 
update msg model session =
    let
        msg1 = Debug.log "msg" msg
        msg2 = Debug.log "model.focus" model.focus
        focusField = Debug.log "focusField" (focusToField model.fields model.focus)
        -- focusOptionField = focusToFieldOption model.focus
    in
        case ( msg, model.focus, focusField ) of
            ( ValueChanged value, FocusEdit fieldNumber, _ ) ->
                let
                    fields = Edit (Just value) |> setField fieldNumber model.fields
                in
                    { model = 
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            -- Option changed value
            ( ValueChanged value, FocusOptions fieldNumber fieldOptionNumber, Just (Options options) ) ->
                let
                    fields = setOption fieldOptionNumber value options |> Options |> setField fieldNumber model.fields
                in
                    { model = 
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , buttonClicked = Nothing
                        -- , focus = Just (modBy 81 (field + 1))
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( ValueCleared, FocusEdit fieldNumber, _ ) ->
                let
                    fields = Edit Nothing |> setField fieldNumber model.fields
                in
                    { model =
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }
            
            ( ValueCleared, FocusOptions fieldNumber fieldOptionNumber, Just(Options options) ) ->
                let
                    fields = clearOption fieldOptionNumber options |> Options |> setField fieldNumber model.fields
                in
                    { model =
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( FocusChanged fieldNumber, FocusOptions fieldNumberOld _, Just (Options options) ) ->
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
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( FocusChanged fieldNumber, _, _ ) ->
                { model =
                    { model
                    | focus = fieldNumberToFocus model.fields (fieldNumber, 0)
                    , buttonClicked = Nothing
                    }
                , session = session
                , cmd = Cmd.none
                }

            ( MovedFocus dir, FocusOptions fieldNumberOld fieldNumberOptionOld, Just (Options options) ) ->
                --when leaving an empty Options field, change it to Editfield
                let
                    focus = FocusOptions fieldNumberOld fieldNumberOptionOld
                    (fieldNumber, _) = moveField (fieldNumberOld, fieldNumberOptionOld) dir
                    fields =
                        if optionsIsEmpty options && fieldNumber /= fieldNumberOld then
                            setField fieldNumberOld model.fields (Edit Nothing)
                        else
                            model.fields
                in
                    { model =
                        { model
                        | fields = fields
                        , focus = moveFocus focus dir fields
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( MovedFocus dir, focus, _ ) ->
                { model =
                    { model
                    | focus = moveFocus focus dir model.fields
                    , buttonClicked = Nothing
                    }
                , session = session
                , cmd = Cmd.none
                }

            ( OptionsToggled, FocusEdit fieldNumber, Just (Edit maybeValue) ) ->
                let
                    fields = initOptions maybeValue |> Options |> setField fieldNumber model.fields
                in
                    { model = 
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , focus = FocusOptions fieldNumber 0
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( OptionsToggled, FocusOptions fieldNumber fieldOptionNumber, Just (Options options) ) ->
                let
                    fields = Edit (getOption fieldOptionNumber options ) |> setField fieldNumber model.fields
                in
                    { model = 
                        { model 
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , focus = FocusEdit fieldNumber
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( OptionFocusChanged fieldNumber optionFieldNumber, _, _ ) ->
                { model = 
                    { model 
                    | focus = FocusOptions fieldNumber optionFieldNumber
                    , buttonClicked = Nothing
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
                            , buttonClicked = Nothing
                            }
                            , session = session, cmd = Cmd.none }
                        
                    _ ->
                        { model = model, session = session, cmd = Cmd.none }

            ( HighLighted, _, Just (Edit maybeValue)) ->
                if model.highlight == maybeValue then
                    { model =
                        { model
                        | highlight = Nothing
                        , buttonClicked = Nothing
                        }
                        , session = session, cmd = Cmd.none
                    }
                else
                    { model =
                        { model
                        | highlight = maybeValue
                        , buttonClicked = Nothing
                        }
                        , session = session, cmd = Cmd.none
                    }

            ( HighLighted, _, Just (Frozen value)) ->
                if model.highlight == Just value then
                    { model =
                        { model
                        | highlight = Nothing
                        , buttonClicked = Nothing
                        }
                        , session = session, cmd = Cmd.none
                    }
                else
                    { model =
                        { model
                        | highlight = Just value
                        , buttonClicked = Nothing
                        }
                        , session = session, cmd = Cmd.none
                    }

            ( Possibilities, FocusEdit fieldNumber, _ ) ->
                let
                    fields = generateOptions model.fields fieldNumber |> setField fieldNumber model.fields
                in
                    { model =
                        { model
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , focus = fieldNumberToFocus fields (fieldNumber, 0)
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( Possibilities, FocusOptions fieldNumber _, _ ) ->
                let
                    fields = generateOptions model.fields fieldNumber |> setField fieldNumber model.fields
                in
                    { model =
                        { model
                        | fields = fields
                        , faults = recomputeFaults fields fieldNumber model.faults
                        , focus = fieldNumberToFocus fields (fieldNumber, 0)
                        , buttonClicked = Nothing
                        }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( ButtonClear1, _, _ ) ->
                { model =
                    { model
                    | buttonClicked = Just ButtonClear
                    }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( ButtonClear2, _, _ ) ->
                { model =
                    { model
                    | fields = clearFields model.fields
                    , focus = FocusBlurred
                    , highlight = Nothing
                    , faults = initFaults
                    , buttonClicked = Nothing

                    }
                    , session = session
                    , cmd = Cmd.none
                    }

            ( ButtonNew1, _, _ ) ->
                { model =
                    { model
                    | buttonClicked = Just ButtonNew
                    }
                , session = session
                , cmd = Cmd.none
                }

            ( ButtonNew2, _, _ ) ->
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
        'p' -> Possibilities
        'P' -> Possibilities
        _ -> MsgNone


                