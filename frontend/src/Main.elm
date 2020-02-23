module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (..)
import Bootstrap.CDN as CDN

import Session exposing (..)
import Sudoku exposing (..)
import SudokuModel exposing (..)
import SudokuCDN exposing (..)
import Domain.InitFlags exposing (..)

main : Program String Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Sudoku sudokuModel session ->
            Sudoku.subscriptions sudokuModel |> Sub.map SudokuMsg


type Model =
    Sudoku SudokuModel.Model Session
    

-- refresh page : 
init : String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        session = Session.initialSession (getInitFlags flags)
        ( model, cmd ) = Sudoku.init session
    in
        ( Sudoku model session, cmd |> Cmd.map SudokuMsg)


-- #####
-- #####   VIEW
-- #####


view : Model -> Document Msg
view model =
    case model of
        Sudoku model1 session ->
            { title = "Sudoku"
            , body = 
                [ CDN.stylesheet
                , SudokuCDN.stylesheet
                , Sudoku.view model1 |> Html.map SudokuMsg
                ]
            }


toSession : Model -> Session
toSession model =
    case model of
        Sudoku _ session ->
            session


toModel :  Model -> Cmd Msg -> Session -> ( Model, Cmd Msg )
toModel model cmd session =
    case ( session.page, model ) of
        ( SudokuPage, Sudoku sudokuModel session1 ) ->
            ( Sudoku sudokuModel session1, cmd )


-- #####
-- #####   UPDATE
-- #####


type Msg
    = SudokuMsg Sudoku.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        ( SudokuMsg subMsg, Sudoku sudokuModel session ) ->
           let
                updated = Sudoku.update subMsg sudokuModel session
            in
                toModel (Sudoku updated.model session) (updated.cmd |> Cmd.map SudokuMsg) updated.session

        ( LinkClicked _, _ ) ->
            ( model, Cmd.none )

        ( UrlChanged _, _ ) ->
            ( model, Cmd.none )


