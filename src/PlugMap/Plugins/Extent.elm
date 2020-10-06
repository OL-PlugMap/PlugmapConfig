port module PlugMap.Plugins.Extent exposing (..)

import Json.Decode as D exposing (Value)
import Json.Encode as E
import Maybe.Extra exposing (unwrap)


--Ports go here
--This module has two ports used in the JS
--getExtentCmd which is an incomming port that it subscribes to
--samplePluginSubscription which is an outgoing port it sends data to
--NOTE: msg is lowercase on purpose
--Outgoing ports should be either () -> Cmd msg    OR    E.Value -> Cmd Msg


port extentToPointZoomCmd : E.Value -> Cmd msg


port extentToBoundingBoxCmd : E.Value -> Cmd msg


port extentToWKTCmd : E.Value -> Cmd msg



--Incomming ports should be either (() -> msg) -> Sub msg    OR    (Something -> msg) -> Sub msg


port extentSubscription : (ExtentSubResponse -> msg) -> Sub msg


type alias Point =
    { x : Float
    , y : Float
    }


type alias Zoom =
    Int


type alias BBox =
    { xmin : Float
    , ymin : Float
    , xmax : Float
    , ymax : Float
    }


type alias Wkts =
    List String


type alias ExtentSubResponse =
    {}


type alias Model =
    {}


type Msg
    = NoOp
    | ExtentSubscriptionResponse ExtentSubResponse


defaultModel : Model
defaultModel =
    Model


init : String -> ( Model, Cmd Msg )
init wkt =
    let
        peWkt =
            [ wkt ]
    in
        ( Model, Cmd.none )


denit : Model -> ( Model, Cmd Msg )
denit model =
    ( Model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ extentSubscription ExtentSubscriptionResponse ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExtentSubscriptionResponse response ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


encodePointZoom : Point -> Zoom -> E.Value
encodePointZoom point zoom =
    E.object
        [ ( "x", E.float point.x )
        , ( "y", E.float point.y )
        , ( "zoom", E.int zoom )
        ]


encodeBBox : BBox -> E.Value
encodeBBox bbox =
    E.object
        [ ( "xmin", E.float bbox.xmin )
        , ( "ymin", E.float bbox.ymin )
        , ( "xmax", E.float bbox.xmax )
        , ( "ymax", E.float bbox.ymax )
        ]


encodeWkts : List String -> E.Value
encodeWkts wkts =
    E.list (E.string) wkts


setExtentPointZoom : Point -> Zoom -> Cmd Msg
setExtentPointZoom point zoom =
    extentToPointZoomCmd (encodePointZoom point zoom)


setExtentBBox : BBox -> ( Model, Cmd Msg )
setExtentBBox bbox =
    ( Model, extentToBoundingBoxCmd (encodeBBox bbox) )


setExtentWKT : String -> ( Model, Cmd Msg )
setExtentWKT wkt =
    let
        peWkt =
            [ wkt ]
    in
        ( Model, extentToWKTCmd (encodeWkts peWkt) )
