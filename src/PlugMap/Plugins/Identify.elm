port module PlugMap.Plugins.Identify exposing (..)

import PlugMap.Plugins.Themes exposing (Layer, encodeLayer)

import Json.Decode as D exposing (Value)
import Json.Encode as E
import Maybe.Extra exposing (unwrap)
import Json.Decode.Pipeline exposing (..)

import Element as El exposing (Element)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

import Html.Attributes exposing (id)

import Dict exposing (Dict)

port enableIdentify : E.Value -> Cmd msg

port disableIdentify : () -> Cmd msg

port identifyResults : (E.Value -> msg) -> Sub msg

port identifyGetDetails : (Int -> msg) -> Sub msg

port identifyFeature : (E.Value -> msg) -> Sub msg

type alias IdentifyResults =
    { clickX : Int 
    , clickY : Int
    , results : List Results
    }


type alias Results =
    { layerId : String
    , fields : Dict String String
    }


{-
    This is how outside code will interact with our plugin and how we interact with ourselves
        Internal messages are to be used for talking to ourselves
        onNewColor is called when we get a new color. THis should be hooked up outside as we dont really care about the color in here ... yet
-}
type alias API msg =
    { onInternalMessage : InternalMsg -> msg 
    , onGetDetails : Int -> msg
    , onFeature : String -> String -> String -> msg
    }


type alias APITranslator parentMsg =
    Msg -> parentMsg


apiTranslator : API parentMsg -> APITranslator parentMsg
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage m
        GetDetails id ->
            td.onGetDetails id
        Feature key id typeKey ->
            td.onFeature key id typeKey 





resultsDecoder : D.Decoder Results
resultsDecoder =
    D.succeed Results
        |> required "layerID" D.string
        |> required "fields" (D.dict D.string)


identifyResultsDecoder : D.Decoder IdentifyResults
identifyResultsDecoder =
    D.succeed IdentifyResults
        |> required "clickX" D.int
        |> required "clickY" D.int
        |> required "results" (D.list resultsDecoder)



type alias Model =
    { showingResults : Bool
    , curentResults : Maybe IdentifyResults
    , selectedIndex : Maybe Int
    , layers : List Layer
    , featureConfig : List ExtraFeature
    }


type Msg
    = Internal InternalMsg
    | GetDetails Int
    | Feature String String String

type InternalMsg
    = IdentifyResultsReceived IdentifyResults
    | NoOp


defaultModel : Model
defaultModel =
    { showingResults = False
    , curentResults = Nothing
    , selectedIndex = Nothing
    , layers = []
    , featureConfig = []
    }

type alias ExtraFeature =
    { name : String
    , key : String
    }

encodeExtraFeature : ExtraFeature -> E.Value
encodeExtraFeature feature =
    E.object
        [ ( "name", E.string feature.name )
        , ( "key", E.string feature.key )
        ]

init : List ExtraFeature -> List Layer -> ( Model, Cmd Msg )
init featureConfig layers  =
    let
        filteredLayers = 
            List.filter
                (\layer -> layer.identify /= Nothing)        
                layers
    in
    
    ( { defaultModel | layers = filteredLayers, featureConfig = featureConfig }
    , enableIdentify 
      <| E.object 
            [ ( "layers", (E.list encodeLayer filteredLayers) )
            , ("extraFeatures", E.list encodeExtraFeature featureConfig ) 
            ]
    )


destruct : Model -> ( Model, Cmd Msg )
destruct model =
    ( defaultModel
    , disableIdentify ()
    )


enable : Model -> ( Model, Cmd Msg)
enable model =
    ( model
    , enableIdentify 
      <| E.object 
            [ ( "layers", (E.list encodeLayer model.layers) )
            , ("extraFeatures", E.list encodeExtraFeature model.featureConfig ) 
            ]
    )

disable : Model -> ( Model, Cmd Msg )
disable model =
    ( model
    , disableIdentify () 
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ identifyResults handleIdentifyResults 
        , identifyGetDetails (getDetails model)
        , identifyFeature handleFeature
        ]


getDetails : Model -> Int -> Msg
getDetails model id =
    GetDetails id

handleIdentifyResults : D.Value -> Msg
handleIdentifyResults value =
    case D.decodeValue identifyResultsDecoder value of
        Ok results ->
            Internal <| IdentifyResultsReceived results
        Err err ->
            Internal NoOp

handleFeature : D.Value -> Msg
handleFeature value =
    case D.decodeValue featureResultDecoder value of
        Ok res ->
            Feature res.key res.id res.typeKey
        Err err ->
            Internal NoOp

featureResultDecoder =
    D.succeed FeatureResult
    |> required "key" D.string
    |> required "id" D.string
    |> required "typeKey" D.string

type alias FeatureResult =
    { key : String
    , id : String
    , typeKey : String
    }

update : Model -> InternalMsg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        IdentifyResultsReceived results ->
            if List.length results.results > 0 then
                (   { model 
                    | showingResults = True
                    , curentResults = Just results
                    , selectedIndex = Just 0
                    }
                , Cmd.none
                )
            else --TODO: Handle no results?
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    El.none