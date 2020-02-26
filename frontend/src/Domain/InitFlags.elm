module Domain.InitFlags exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

type alias InitFlags =
    {
        sudoku_api_base_url : String
        , thisBaseUrlString : String
    }

-- Opaque
emptyInitFlags : InitFlags
emptyInitFlags =
    {
        sudoku_api_base_url = ""
        , thisBaseUrlString = ""
    }


initFlagsBookDecoder : Decode.Decoder InitFlags
initFlagsBookDecoder =
    Decode.succeed InitFlags
        |> required "sudoku_api_base_url" string
        |> required "this_base_url" string



getInitFlags : String -> InitFlags
getInitFlags dvalue =
    case Decode.decodeString initFlagsBookDecoder dvalue  of

        Result.Ok initFlags ->
            initFlags
    
        Result.Err a ->
            emptyInitFlags