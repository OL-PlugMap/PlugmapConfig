module PlugMap.Core.Options exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Json.Encode as E
import Json.Encode.Extra as EEx


type alias Center =
    ( Float, Float )


type alias Options =
    { target : String
    , center : Center
    , zoom : Int
    , maxZoom : Maybe Int
    , minZoom : Maybe Int
    }


defaultOptions : Options
defaultOptions =
    Options
        "ol-map"
        ( -111.6, 39.4 )
        7
        (Just 22)
        (Just 3)


encodeCenter : Center -> E.Value
encodeCenter ( lat, lon ) =
    E.list E.float [ lat, lon ]


encodeOptions : Options -> E.Value
encodeOptions options =
    E.object
        [ ( "target", E.string options.target )
        , ( "center", encodeCenter options.center )
        , ( "zoom", E.int options.zoom )
        , ( "maxZoom", EEx.maybe E.int options.maxZoom )
        , ( "minZoom", EEx.maybe E.int options.minZoom )
        ]


centerDecoder : Decoder Center
centerDecoder =
    D.index 0 D.float
        |> D.andThen
            (\lat ->
                D.index 1 D.float
                    |> D.andThen (\lon -> D.succeed ( lat, lon ))
            )


optionsDecoder : Decoder Options
optionsDecoder =
    D.field "options"
        (D.succeed Options
            |> required "target" D.string
            |> required "center" centerDecoder
            |> required "zoom" D.int
            |> optional "maxZoom" (D.map Just D.int) Nothing
            |> optional "minZoom" (D.map Just D.int) Nothing
        )

optionsFromConfigDecoder : Decoder Options
optionsFromConfigDecoder =
    D.succeed Options
        |> hardcoded "ol-map"
        |> optional "center" centerDecoder ( -111.6, 39.4 )
        |> optional "zoom" D.int 7
        |> optional "maxZoom" (D.maybe D.int) (Just 22)
        |> optional "minZoom" (D.maybe D.int) (Just 3)

{-
   CONFIG!!!!!!!!!!!!!!!!!!!!!!!!!
-}


type alias Config =
    { swipe : SwipeConfig
    }


type alias SwipeConfig =
    { showKeyhole : Bool
    , showNone : Bool
    }


defaultSwipeConfig : SwipeConfig
defaultSwipeConfig =
    { showKeyhole = True
    , showNone = True
    }


defaultConfig : Config
defaultConfig =
    { swipe = defaultSwipeConfig
    }
