module Flags exposing (Flags, decoder)

import Codec exposing (Codec)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import LocalStorage exposing (LocalStorage)


type alias Flags =
    { localStorage : LocalStorage
    , timestamp : Int
    }


decoder : JD.Decoder Flags
decoder =
    JD.succeed Flags
        |> JDP.required "localStorage" (Codec.decoder LocalStorage.codec)
        |> JDP.required "timestamp" JD.int
