port module PlugMap.Plugins.ColorUnderPointer exposing (API, APITranslator, ColorUnderPointerOptions, InternalMsg(..), Model, Msg(..), apiTranslator, colorChanged, colorDecoder, defaultModel, denit, disableColorUnderPointerCmd, enableColorUnderPointerCmd, encodeColorUnderPointerOptions, handleColorChange, init, subscriptions, update)

import Color as Color exposing (Color)
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as E
import Maybe.Extra exposing (unwrap)



{- Ports go here
   This module has three ports used in the JS
       Outbound enableColorUnderPointerCmd
           This enables the color under pointer plugin. It requires a config to tell it what layers it should be looking at
       Outbound disableColorUnderPointerCmd
           This will disable the color under pointer plugin (Problem for future me)
       Inbound colorChanged
           This is the color that JS land is responding with. It should be an exact value from the last layer it had a hit on

-}


port enableColorUnderPointerCmd : E.Value -> Cmd msg


port disableColorUnderPointerCmd : () -> Cmd msg


port colorChanged : (Value -> msg) -> Sub msg


type alias Model =
    {}



{-
   We need internal messages to be able to handle changes to our own state ... I think
   New color is technically outbound which is handled by the translator
-}


type Msg
    = Internal InternalMsg
    | NewColor Color


type InternalMsg
    = I_NoOp
    | ColorChange Color



{-
   Record to hold the config for this plugin
-}


type alias ColorUnderPointerOptions =
    { layers : List String
    }



{-
   Encode the options into json
-}


encodeColorUnderPointerOptions : ColorUnderPointerOptions -> E.Value
encodeColorUnderPointerOptions options =
    E.object
        [ ( "layers", E.list E.string options.layers )
        ]



{-
   This is how outside code will interact with our plugin and how we interact with ourselves
       Internal messages are to be used for talking to ourselves
       onNewColor is called when we get a new color. THis should be hooked up outside as we dont really care about the color in here ... yet
-}


type alias API msg =
    { onInternalMessage : Msg -> msg
    , onNewColor : Color -> msg
    }


type alias APITranslator parentMsg =
    Msg -> parentMsg


apiTranslator : API parentMsg -> APITranslator parentMsg
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage <| Internal m

        NewColor color ->
            td.onNewColor color



{-
   Decoder for colors coming back from the port
-}


colorDecoder : D.Decoder Color
colorDecoder =
    D.succeed Color.rgb255
        |> required "r" D.int
        |> required "g" D.int
        |> required "b" D.int


defaultModel : Model
defaultModel =
    Model



--Yea yea this code aint great. Iterate when we get to themes.


init : List String -> ( Model, Cmd Msg )
init layersToWatch =
    ( Model
    , enableColorUnderPointerCmd
        (encodeColorUnderPointerOptions <|
            ColorUnderPointerOptions layersToWatch
        )
    )


denit : Model -> ( Model, Cmd Msg )
denit model =
    ( Model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ colorChanged handleColorChange ]



{-
   This is called when we get a new color from JS land
-}


handleColorChange : Value -> Msg
handleColorChange value =
    case D.decodeValue colorDecoder value of
        Ok color ->
            NewColor color

        _ ->
            Internal <| I_NoOp



--Something happened and we should handle it better
--Here for when we need it


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )
