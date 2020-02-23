module Domain.SudokuQuiz exposing (..)

import Session exposing (..)

import RemoteData exposing (RemoteData, WebData, succeed)
import Array exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias SudokuQuiz = 
    {
        id : Int
        , quiz : String
        , solution : String
    }

getRandomSudokuQuiz : (WebData SudokuQuiz -> msg) -> Session ->  Cmd msg
getRandomSudokuQuiz msg session =
    let
        requestUrl = Debug.log "requestUrl" (getSudokuApiBaseUrl session)
    in
        Http.get
            { url = requestUrl
            , expect =  
                sudokuQuizDecoder
                |> Http.expectJson (RemoteData.fromResult >> msg)
            }

sudokuQuizDecoder : Decoder SudokuQuiz
sudokuQuizDecoder =
    Decode.succeed SudokuQuiz
        |> required "id" int
        |> required "quizzes" string
        |> required "solutions" string
