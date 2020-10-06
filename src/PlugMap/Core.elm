module PlugMap.Core exposing 
    ( InternalizedPlugin, Model, InternalMsg(..), Plugin, defaultModel, denit, init, initWithDefaults, initWithOptions, refresh, setServices, subscriptions, update, view
    , getLocation, Location
    , Extent(..), MapExtent, APITranslator, apiTranslator
    )

import Element exposing (Element, column, el, fill, height, none, width)
import Element.Events as Events
import Element.Keyed as Keyed
import Json.Decode as D exposing (Value, Decoder)
import Json.Encode as E
import Json.Decode.Pipeline exposing (required)

import PAM.MapServices exposing (MapService(..), encodeMapService)
import PAM.UI.Basic as UI
import PlugMap.Core.Options exposing (..)
import PlugMap.Core.Ports exposing (..)


import Process
import Task


type alias Plugin model msg =
    { model : model
    , view : model -> Element msg
    }


type alias InternalizedPlugin =
    {}



-- MODEL


type alias Model =
    { mapOptions : Maybe Options
    , plugins : List InternalizedPlugin
    , currentExtent : Extent
    }


type alias MapExtent =
    { xmin : Float
    , ymin : Float
    , xmax : Float
    , ymax : Float
    , center : Location
    }

type alias Location =
    { x : Float
    , y : Float 
    } 



type Extent 
    = Unknown
    | Known 
        MapExtent
        

getLocation : Model -> Location
getLocation model =
    case model.currentExtent of
        Unknown ->
            Location 0 0 
        Known ext ->
            ext.center

mapExtentDecoder : Decoder MapExtent
mapExtentDecoder =
    D.succeed MapExtent
    |> required "xmin" D.float
    |> required "ymin" D.float
    |> required "xmax" D.float
    |> required "ymax" D.float
    |> required "center" locationDecoder


locationDecoder : Decoder Location
locationDecoder =
    D.succeed Location
    |> required "x" D.float
    |> required "y" D.float

extentDecoder : Decoder Extent
extentDecoder =
    D.oneOf
        [ D.map 
            Known
            mapExtentDecoder
        , D.succeed Unknown
        ]

defaultModel : Model
defaultModel =
    { mapOptions = Nothing
    , plugins = []
    , currentExtent = Unknown
    }


initWithDefaults : ( Model, Cmd Msg )
initWithDefaults =
    initWithOptions defaultOptions


initWithOptions : Options -> ( Model, Cmd Msg )
initWithOptions options =
    let
        ( mapOptions, encodedOptions ) =
            ( Just options, encodeOptions options )

        initialModel =
            { defaultModel
                | mapOptions = mapOptions
            }
    in
    ( initialModel, renderMapCmd encodedOptions )


setServices : Model -> List MapService -> ( Model, Cmd Msg )
setServices model services =
    let
        encodedServices =
            E.list encodeMapService services
    in
    ( model
    , setServicesCmd encodedServices
    )


denit : Model -> ( Model, Cmd Msg )
denit model =
    ( model
    , destructMapCmd ()
    )


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        ( mapOptions, encodedOptions ) =
            case D.decodeValue optionsDecoder flags of
                Ok opts ->
                    ( Just opts, encodeOptions opts )

                Err err ->
                    ( Nothing, E.null )

        initialModel =
            { defaultModel
                | mapOptions = mapOptions
            }

        -- we can location stuff here
    in
    ( initialModel
    , Cmd.batch
        [ renderMapCmd encodedOptions
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ extentUpdated ( handleExtentUpdated model )
        ]



handleExtentUpdated : Model -> E.Value -> Msg
handleExtentUpdated model value =
    case D.decodeValue extentDecoder value of
        Ok extent ->
            Internal <| HandleExtent extent
        Err err ->
            Internal Noop


-- Update

type Msg
    = Internal InternalMsg
    | MouseLeaveMap
    | ExtentUpdated Extent 

type alias API msg =
    { onInternalMessage : InternalMsg -> msg
    , onMouseLeaveMap : msg
    , onExtentUpdated : Extent -> msg
    }


type alias APITranslator parentMsg =
    Msg -> parentMsg



apiTranslator : API parentMsg -> APITranslator parentMsg
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage m

        MouseLeaveMap ->
            td.onMouseLeaveMap
        
        ExtentUpdated ext ->
            td.onExtentUpdated ext



type InternalMsg
    = Noop
    | HandleExtent Extent

update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        HandleExtent newExtent ->
            ( { model | currentExtent = newExtent }
            , Process.sleep 500
                |> Task.perform 
                    (always <| ExtentUpdated newExtent )
            )


view : Model -> Element Msg
view model =
    Keyed.el
        [ height fill
        , width fill
        , Events.onMouseLeave MouseLeaveMap
        ]
        ("ol-map"
        , Keyed.el
            [ height fill
            , width fill
            , UI.id "ol-map"
            ]
            ( "nasd", none)
        )


refresh : Model -> Cmd Msg
refresh model =
    refreshMap ()
