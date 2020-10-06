port module PlugMap.Plugins.FeaturePicker exposing 
    ( API, APITranslator, EsriFeature, EsriProperty(..), EsriResponse
    , Esri_Config, Feature, FeatureLoad(..), Filter(..), InternalMsg(..)
    , Model, Msg(..), NormalizeEsriRequest, Options, ReferenceLayer
    , ReferenceLayerConfig(..), ResponseFrom(..), Shape, ToJsResult
    , WFSProperty(..), WFSResponseMode(..), WFS_Config, WMS_Config
    , WfsAttributes, WfsMember, WfsResult, XmlToJsType(..), apiTranslator
    , buildFilterXml, buildUrl, decodeEsriFeature, decodeEsriFeatures
    , decodeEsriProperty, decodeNormalizeEsri, decodeResponseFrom
    , decodeWFSMembers, decodeWFSProperty, decodeXmlToJsType, defaultModel
    , defaultOptions, deselect, deselectFeature, dictGet, encodeEsriConfig
    , encodeNormalizeEsri, encodeReferenceLayer, encodeReferenceLayerConfig
    , encodeWFSConfig, encodeWMSConfig, encodeXMLToJsRequest, equalToDecoder
    , esriConfigDecoder, esriDecoder, esriFeatureToFeature
    , esriFeaturesToFeatures, esriRequest, esriRequestById
    , esriResponseDecoder, featurePickerDisable, featurePickerSetLayer
    , featureSelected, featureSelectedProcess, filterDecoder, getFeatures
    , getServiceFor, getServiceKey, getSingleFeature
    , handleEsriRequestByIdResponse, handleEsriRequestResponse
    , handleWfsRequestByIdResponse, handleWfsRequestResponse
    , hideReferenceLayer, hideReferenceLayerDisplay, init, initWithoutFlags
    , noop, normalizeEsri, normalizeEsriProcess, normalizeEsriResult
    , referenceLayerDecoder, responseFromToString, select, selectFeature
    , setReferenceLayer, subscriptions, update, wfsAttributesDecoder
    , wfsConfigDecoder, wfsMemberToFeature, wfsRequest, wfsRequestById
    , wfsResponseModeDecoder, wfsResultDecoder, wfsResultToFeatures
    , wmsConfigDecoder, wmsWfsDecoder, xmlToJs, xmlToJsDecoder, xmlToJsProcess
    , xmlToJsResult, xmlToJsTypeToString, destruct, clear
    )

import Dict exposing (..)
import Http
import Json.Decode as D exposing (Decoder, Value, map)
import Json.Decode.Pipeline exposing (hardcoded, optional, required, requiredAt)
import Json.Encode as E exposing (Value)
import Json.Encode.Extra as Ex
import List exposing (..)
import Maybe.Extra exposing (unwrap)
import PAM.MapServices exposing (MapService(..))
import PlugMap.Plugins.Themes exposing (Layer, LayerConfig(..), encodeLayer)
import Task exposing (..)



--import QueryString as QS


port featurePickerSetLayer : E.Value -> Cmd msg


port featurePickerDisable : () -> Cmd msg


port selectFeature : Int -> Cmd msg


port deselectFeature : Int -> Cmd msg


port featureSelected : (D.Value -> msg) -> Sub msg


port hideReferenceLayerDisplay : () -> Cmd msg


port clearReferenceFeatures : () -> Cmd msg

port addReferenceItems : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ xmlToJsResult xmlToJsProcess
        , normalizeEsriResult normalizeEsriProcess
        , featureSelected (featureSelectedProcess model)
        ]


type alias Feature =
    { id : Int
    , olid : Int
    , name : String
    , geometry : Maybe String
    , acres : Maybe Float
    }


type alias Shape =
    { id : Int
    , wkt : String
    }


type alias Model =
    { services : Dict String MapService
    , layer : Maybe ReferenceLayer
    , trackingRequests : Int
    , trackingFeatures : FeatureLoad
    , selectedFeatures : List Feature
    }


type FeatureLoad
    = NoFeatures
    | Loading (List Feature)
    | Loaded (List Feature)


type alias ReferenceLayer =
    { name : String
    , key : String
    , config : ReferenceLayerConfig
    }


type ReferenceLayerConfig
    = Esri Esri_Config
    | WMS_WFS WMS_Config WFS_Config



{-
   This is how outside code will interact with our plugin and how we interact with ourselves
       Internal messages are to be used for talking to ourselves
       onNewColor is called when we get a new color. THis should be hooked up outside as we dont really care about the color in here ... yet
-}


type alias API msg =
    { onInternalMessage : InternalMsg -> msg
    , onFeaturesSelected : List Feature -> msg
    , onFeaturesLoaded : List Feature -> msg
    , onFeaturesLoadError : String -> msg
    , onFeatureLoading : msg
    }


type alias APITranslator parentMsg =
    Msg -> parentMsg


apiTranslator : API parentMsg -> APITranslator parentMsg
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage m

        FeaturesSelected feats ->
            td.onFeaturesSelected feats

        FeaturesLoaded feats ->
            td.onFeaturesLoaded feats

        FeaturesLoadError err ->
            td.onFeaturesLoadError err

        FeatureLoading ->
            td.onFeatureLoading


type Msg
    = Internal InternalMsg
    | FeaturesSelected (List Feature)
    | FeaturesLoaded (List Feature)
    | FeaturesLoadError String
    | FeatureLoading


type InternalMsg
    = NoOp
    | WFSXmlResult ResponseFrom (Result Http.Error String)
    | EsriResponded ResponseFrom String --(List Feature)
    | EsriRespondedError String
    | EsriRespondedConvert NormalizeEsriRequest
    | EsriRespondedConvertError String
    | FeatureClicked Int


type ResponseFrom
    = Select
    | SetLayer


featureSelectedProcess : Model -> D.Value -> Msg
featureSelectedProcess model val =
    case D.decodeValue D.string val of
        Ok id ->
            let
                int =
                    String.toInt id |> Maybe.withDefault -1
            in
            -- let
            --  (a,b) = getSingleFeature model.services model.layer int
            Internal <| FeatureClicked int

        Err err ->
            Internal NoOp


encodeReferenceLayer : ReferenceLayer -> E.Value
encodeReferenceLayer layer =
    E.object
        [ ( "name", E.string layer.name )
        , ( "key", E.string layer.key )
        , ( "config", encodeReferenceLayerConfig layer.config )
        ]


encodeReferenceLayerConfig : ReferenceLayerConfig -> E.Value
encodeReferenceLayerConfig layer =
    E.object <|
        case layer of
            Esri conf ->
                [ ( "esri", encodeEsriConfig conf ) ]

            WMS_WFS wms wfs ->
                [ ( "wfs", encodeWFSConfig wfs )
                , ( "wms", encodeWMSConfig wms )
                ]


referenceLayerDecoder : Decoder ReferenceLayer
referenceLayerDecoder =
    D.succeed ReferenceLayer
        |> required "name" D.string
        |> required "key" D.string
        |> required "config"
            (D.oneOf
                [ esriDecoder
                , wmsWfsDecoder
                ]
            )


responseFromToString : ResponseFrom -> String
responseFromToString from =
    case from of
        Select ->
            "Select"

        SetLayer ->
            "SetLayer"


decodeResponseFrom : Decoder ResponseFrom
decodeResponseFrom =
    D.string
        |> D.andThen
            (\val ->
                D.succeed <|
                    if val == "Select" then
                        Select

                    else if val == "SetLayer" then
                        SetLayer

                    else
                        SetLayer
            )


type alias Options =
    {}


defaultOptions : Options
defaultOptions =
    {}


defaultModel : Model
defaultModel =
    Model
        Dict.empty
        Nothing
        0
        NoFeatures
        []


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WFSXmlResult from result ->
            case ( from, result ) of
                ( SetLayer, Ok res ) ->
                    -- ( model
                    -- , xmlToJs res
                    -- )
                    ( model
                    , Cmd.batch
                        [ xmlToJs <| encodeXMLToJsRequest { typ = X2J_WFS, xml = res, result = Nothing }
                        ]
                    )

                ( _, Err _ ) ->
                    ( model
                    , Cmd.none
                    )

                ( _, _ ) ->
                    noop model

        EsriResponded from feats ->
            ( model
            , normalizeEsri <|
                encodeNormalizeEsri
                    { from = from
                    , esri = feats
                    , value = Nothing
                    , showGeometry = True
                    }
            )

        EsriRespondedError error ->
            ( model
            , Task.perform
                (\dontCareAboutThisValue ->
                    FeaturesLoadError
                        error
                )
              <|
                Task.succeed "Hello"
            )

        EsriRespondedConvertError error ->
            ( model
            , Task.perform
                (\dontCareAboutThisValue ->
                    FeaturesLoadError
                        error
                )
              <|
                Task.succeed "Hello"
            )

        EsriRespondedConvert res ->
            case res.from of
                SetLayer ->
                    if (model.trackingRequests - 1) > 0 then
                        let
                            oldFeats =
                                case model.trackingFeatures of
                                    Loading featz ->
                                        featz

                                    _ ->
                                        []

                            allFeats =
                                oldFeats
                                    ++ (res.value
                                            |> Maybe.map .features
                                            |> Maybe.map (List.map (esriFeatureToFeature model.layer))
                                            |> Maybe.withDefault []
                                       )
                        in
                        ( { model
                            | trackingRequests = model.trackingRequests - 1
                            , trackingFeatures = Loading allFeats
                          }
                        , Cmd.none
                        )

                    else
                        let
                            oldFeats =
                                case model.trackingFeatures of
                                    Loading featz ->
                                        featz

                                    _ ->
                                        []

                            allFeats =
                                oldFeats
                                    ++ (res.value
                                            |> Maybe.map .features
                                            |> Maybe.map (List.map (esriFeatureToFeature model.layer))
                                            |> Maybe.withDefault []
                                       )
                        in
                        ( { model
                            | trackingRequests = 0
                            , trackingFeatures = Loaded allFeats
                          }
                        , Task.perform
                            (\dontCareAboutThisValue ->
                                FeaturesLoaded
                                    allFeats
                            )
                          <|
                            Task.succeed "Hello"
                        )

                Select ->
                    if (res.value |> Maybe.map .features |> Maybe.withDefault [] |> List.length) > 0 then
                        let
                            allFeats =
                                res.value
                                    |> Maybe.map .features
                                    |> Maybe.map (List.map (esriFeatureToFeature model.layer))
                                    |> Maybe.withDefault []
                        in
                        ( model
                        , Task.perform
                            (\dontCareAboutThisValue ->
                                FeaturesSelected
                                    allFeats
                            )
                          <|
                            Task.succeed "Hello"
                        )

                    else
                        noop model

        FeatureClicked id ->
            ( model, Cmd.none )
                |> getSingleFeature model.services model.layer id

        _ ->
            noop model


select : Int -> Model -> ( Model, Cmd Msg )
select id model =
    (case model.trackingFeatures of
        Loaded feats ->
            let
                newFeat =
                    List.filter (\a -> a.id == id) feats
                        |> List.head
            in
            case newFeat of
                Just f ->
                    ( { model
                        | selectedFeatures = model.selectedFeatures ++ [ f ]
                      }
                    , selectFeature id
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        _ ->
            ( model
            , Cmd.none
            )
    )
        |> getSingleFeature model.services model.layer id


deselect : Int -> Model -> ( Model, Cmd Msg )
deselect id model =
    ( model
    , deselectFeature id
    )


encodeXMLToJsRequest : ToJsResult -> E.Value
encodeXMLToJsRequest result =
    E.object
        [ ( "type", E.string <| xmlToJsTypeToString result.typ )
        , ( "xml", E.string result.xml )
        ]


setReferenceLayer : Model -> Maybe ReferenceLayer -> ( Model, Cmd Msg )
setReferenceLayer model refLayer =
    (case refLayer of
        Just layer ->
            ( model
            , featurePickerSetLayer <| encodeReferenceLayer layer
            )

        Nothing ->
            ( model
            , featurePickerDisable ()
              --TODO Task Complete load features []
            )
    )
        |> getFeatures model.services refLayer
        |> (\( a, b ) ->
                ( { a | layer = refLayer }
                , b
                )
           )



--Depending on the config type we need to construct our queries here and submit them
--The result should then call onFeaturesLoaded
--We should also keep track of what defines the minimum of a feature ie. id and name


getFeatures : Dict String MapService -> Maybe ReferenceLayer -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
getFeatures services refLayer ( model, cmds ) =
    case refLayer of
        Just layer ->
            case layer.config of
                WMS_WFS wms wfs ->
                    ( { model | trackingRequests = 1, trackingFeatures = NoFeatures, selectedFeatures = [] }
                    , Cmd.batch
                        [ wfsRequest services wfs
                        , cmds
                        ]
                    )

                Esri conf ->
                    ( { model | trackingRequests = conf.layers |> List.length, trackingFeatures = NoFeatures, selectedFeatures = [] }
                    , Cmd.batch
                        [ esriRequest services conf
                        , cmds
                        ]
                    )

        _ ->
            ( model
            , cmds
            )


getSingleFeature : Dict String MapService -> Maybe ReferenceLayer -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
getSingleFeature services refLayer id ( model, cmds ) =
    case refLayer of
        Just layer ->
            case layer.config of
                WMS_WFS wms wfs ->
                    ( model
                    , Cmd.batch
                        [ wfsRequestById services id wfs
                        , cmds
                        , Task.perform
                            (\dontCareAboutThisValue ->
                                FeatureLoading
                            )
                          <|
                            Task.succeed "Hello"
                        ]
                    )

                Esri conf ->
                    ( model
                    , Cmd.batch
                        [ esriRequestById services id conf
                        , cmds
                        , Task.perform
                            (\dontCareAboutThisValue ->
                                FeatureLoading
                            )
                          <|
                            Task.succeed "Hello"
                        ]
                    )

        _ ->
            ( model
            , cmds
            )


getServiceKey : MapService -> Maybe String
getServiceKey service =
    case service of
        Insecured conf ->
            Just conf.key

        Secured conf _ ->
            Just conf.key


getServiceFor : Maybe String -> Dict String MapService -> Maybe MapService
getServiceFor key svc =
    key
        |> Maybe.map (dictGet svc)
        |> Maybe.withDefault Nothing


dictGet : Dict comparable b -> comparable -> Maybe b
dictGet dict key =
    Dict.get key dict


buildUrl : { layer | url : String, tokenKey : Maybe String } -> Dict String MapService -> List ( String, String ) -> String
buildUrl layer ags params =
    let
        pstr =
            "?"
                ++ (List.map
                        (\( key, value ) ->
                            key ++ "=" ++ value
                        )
                        (params
                            ++ (case tokenService of
                                    Just (Secured _ service) ->
                                        [ ( "token", service.token ) ]

                                    _ ->
                                        []
                               )
                        )
                        |> String.join "&"
                   )

        tokenService =
            --TokenService.get refLayer.tokenKey ags
            layer.tokenKey
                |> Maybe.map (dictGet ags)
                |> Maybe.withDefault Nothing

        baseUrl =
            case tokenService of
                Just (Insecured service) ->
                    service.baseUrl

                Just (Secured service _) ->
                    service.baseUrl

                Nothing ->
                    ""
    in
    baseUrl ++ layer.url ++ pstr


noop : Model -> ( Model, Cmd Msg )
noop model =
    ( model, Cmd.none )


initold : Dict String MapService -> ( Model, Cmd Msg )
initold ags =
    ( { defaultModel
        | services = ags
      }
    , Cmd.none
    )

init : Dict String MapService -> ( Model, Cmd Msg )
init services =
    ( { defaultModel | services = services }
    , Cmd.none
    )

initWithoutFlags : ( Model, Cmd Msg )
initWithoutFlags =
    ( defaultModel
    , Cmd.none
    )


destruct : Model -> ( Model, Cmd Msg )
destruct model =
    ( model
    , hideReferenceLayerDisplay ()
    )


{-
   WFS Stuff Here
-}


port xmlToJs : E.Value -> Cmd msg


port xmlToJsResult : (D.Value -> msg) -> Sub msg


type XmlToJsType
    = X2J_WFS


type alias ToJsResult =
    { typ : XmlToJsType
    , xml : String
    , result : Maybe D.Value
    }


type alias WfsResult =
    { attributes : WfsAttributes
    , wfsMembers : List WfsMember
    }


type alias WfsAttributes =
    {}


type alias WfsMember =
    { typeName : String
    , properties : Dict String WFSProperty
    }


type WFSProperty
    = Nil
    | Text String


type alias Esri_Config =
    { url : String
    , tokenKey : Maybe String
    , layers : List String
    , propertyNames : List String
    , idField : String
    , nameField : String
    }



-- Decoders


decodeWFSMembers : Decoder (List WfsMember)
decodeWFSMembers =
    D.list
        (D.dict (D.dict decodeWFSProperty))
        |> D.andThen
            (\itms ->
                itms
                    |> List.map
                        (\collection ->
                            Dict.toList collection
                                |> List.head
                                |> Maybe.map
                                    (\( typeName, props ) ->
                                        WfsMember
                                            typeName
                                            props
                                    )
                        )
                    |> List.filterMap identity
                    |> D.succeed
            )


decodeWFSProperty : Decoder WFSProperty
decodeWFSProperty =
    D.oneOf
        [ D.succeed Text
            |> required "_text" D.string
        , D.succeed Nil
        ]


wfsResultDecoder : Decoder WfsResult
wfsResultDecoder =
    D.succeed WfsResult
        |> requiredAt [ "wfs:FeatureCollection", "_attributes" ] wfsAttributesDecoder
        |> requiredAt [ "wfs:FeatureCollection", "wfs:member" ] decodeWFSMembers


wfsAttributesDecoder : Decoder WfsAttributes
wfsAttributesDecoder =
    D.succeed WfsAttributes


xmlToJsDecoder : Decoder ToJsResult
xmlToJsDecoder =
    D.succeed ToJsResult
        |> required "type" decodeXmlToJsType
        |> required "xml" D.string
        |> required "result" (D.maybe D.value)


wmsWfsDecoder : Decoder ReferenceLayerConfig
wmsWfsDecoder =
    D.succeed WMS_WFS
        |> required "wms" wmsConfigDecoder
        |> required "wfs" wfsConfigDecoder


encodeWFSConfig : WFS_Config -> E.Value
encodeWFSConfig wfs =
    E.object
        [ ( "url", E.string wfs.url )
        , ( "tokenKey", Ex.maybe E.string wfs.tokenKey )
        , ( "typeNames", E.list E.string wfs.typeNames )
        , ( "propertyNames", E.list E.string wfs.propertyNames )

        --TODO Filter
        ]


encodeWMSConfig : WMS_Config -> E.Value
encodeWMSConfig wms =
    E.object
        [ ( "url", E.string wms.url )
        , ( "tokenKey", Ex.maybe E.string wms.tokenKey )
        , ( "layers", E.list E.string wms.layers )
        ]


wmsConfigDecoder : Decoder WMS_Config
wmsConfigDecoder =
    D.succeed WMS_Config
        |> required "url" D.string
        |> optional "tokenKey" (D.maybe D.string) Nothing
        |> required "layers" (D.list D.string)


filterDecoder : Decoder Filter
filterDecoder =
    D.oneOf
        [ equalToDecoder
        ]


equalToDecoder : Decoder Filter
equalToDecoder =
    D.succeed EqualTo
        |> required "field" D.string
        |> required "equals" D.string


wfsConfigDecoder : Decoder WFS_Config
wfsConfigDecoder =
    D.succeed WFS_Config
        |> required "url" D.string
        |> optional "tokenKey" (D.maybe D.string) Nothing
        |> required "typeNames" (D.list D.string)
        |> optional "propertyNames" (D.list D.string) []
        |> optional "filter" (D.maybe filterDecoder) Nothing
        |> optional "mode" wfsResponseModeDecoder XML


wfsResponseModeDecoder : Decoder WFSResponseMode
wfsResponseModeDecoder =
    D.string
        |> D.andThen
            (\a ->
                if a == "JSON" then
                    D.succeed JSON

                else
                    D.succeed XML
            )



-- Conversion functions


xmlToJsTypeToString : XmlToJsType -> String
xmlToJsTypeToString typ =
    case typ of
        X2J_WFS ->
            "WFS"


decodeXmlToJsType : Decoder XmlToJsType
decodeXmlToJsType =
    D.string
        |> D.andThen
            (\val ->
                if val == "WFS" then
                    D.succeed X2J_WFS

                else
                    D.fail <| "Unknown XmlToJSType " ++ val
            )


xmlToJsProcess : E.Value -> Msg
xmlToJsProcess result =
    case D.decodeValue xmlToJsDecoder result of
        Ok val ->
            case ( val.typ, val.result ) of
                ( X2J_WFS, Just json ) ->
                    let
                        res =
                            D.decodeValue wfsResultDecoder json
                    in
                    case res of
                        Ok wfsResult ->
                            FeaturesLoaded <| wfsResultToFeatures wfsResult

                        --TODO: We need to update our model so dont call FeaturesLoaded here
                        {-

                           | trackingRequests = 0
                           , trackingFeatures = Loaded allFeats
                        -}
                        Err err ->
                            Internal NoOp

                ( _, _ ) ->
                    Internal NoOp

        Err err ->
            Internal NoOp


wfsResultToFeatures : WfsResult -> List Feature
wfsResultToFeatures result =
    List.map
        wfsMemberToFeature
        result.wfsMembers


wfsMemberToFeature : WfsMember -> Feature
wfsMemberToFeature member =
    { id =
        Dict.get
            "utwrap_PAM:id"
            member.properties
            |> Maybe.map
                (\val ->
                    case val of
                        Text i ->
                            String.toInt i
                                |> Maybe.withDefault -1

                        _ ->
                            -1
                )
            |> Maybe.withDefault -1
    , olid = -1
    , name =
        Dict.get
            "utwrap_PAM:name"
            member.properties
            |> Maybe.map
                (\val ->
                    case val of
                        Text i ->
                            i

                        _ ->
                            "Unknown"
                )
            |> Maybe.withDefault "Unknown"
    , geometry = Nothing --TODO
    , acres = Nothing
    }


buildFilterXml : Maybe Filter -> List ( String, String )
buildFilterXml mFilter =
    [ ( "filter", "<Filter></Filter>" ) ]


wfsRequest : Dict String MapService -> WFS_Config -> Cmd Msg
wfsRequest services config =
    let
        params =
            [ ( "service", "WFS" )
            , ( "version", "2.0.0" )
            , ( "request", "GetFeature" )
            ]
                ++ (if List.length config.typeNames > 0 then
                        [ ( "typeNames", String.join "," config.typeNames ) ]

                    else
                        []
                            ++ (if List.length config.propertyNames > 0 then
                                    [ ( "propertyName", String.join "," config.propertyNames ) ]

                                else
                                    []
                                        ++ buildFilterXml config.filter
                               )
                   )
    in
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url =
            buildUrl config services params
        , body =
            Http.emptyBody
        , expect =
            Http.expectString
                handleWfsRequestResponse
        , timeout = Nothing
        , tracker = Nothing
        }


wfsRequestById : Dict String MapService -> Int -> WFS_Config -> Cmd Msg
wfsRequestById services id config =
    let
        params =
            [ ( "service", "WFS" )
            , ( "version", "2.0.0" )
            , ( "request", "GetFeature" )
            ]
                ++ (if List.length config.typeNames > 0 then
                        [ ( "typeNames", String.join "," config.typeNames ) ]

                    else
                        []
                            ++ (if List.length config.propertyNames > 0 then
                                    [ ( "propertyName", String.join "," config.propertyNames ) ]

                                else
                                    []
                                        ++ buildFilterXml
                                            (case config.filter of
                                                Just filter ->
                                                    Just <| And filter (EqualTo "id" (String.fromInt id))

                                                --TODO: This needs to come from the config
                                                Nothing ->
                                                    Just <| EqualTo "id" (String.fromInt id)
                                             --TODO: This needs to come from the config
                                            )
                               )
                   )
    in
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url =
            buildUrl config services params
        , body =
            Http.emptyBody
        , expect =
            Http.expectString
                handleWfsRequestByIdResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleWfsRequestResponse : Result Http.Error String -> Msg
handleWfsRequestResponse result =
    Internal <| WFSXmlResult SetLayer result


handleWfsRequestByIdResponse : Result Http.Error String -> Msg
handleWfsRequestByIdResponse result =
    Internal <| WFSXmlResult Select result



{-
   ESRI Stuff Here
-}


port normalizeEsri : E.Value -> Cmd msg


port normalizeEsriResult : (D.Value -> msg) -> Sub msg


type alias WMS_Config =
    { url : String
    , tokenKey : Maybe String
    , layers : List String
    }


type alias WFS_Config =
    { url : String
    , tokenKey : Maybe String
    , typeNames : List String
    , propertyNames : List String
    , filter : Maybe Filter
    , responseMode : WFSResponseMode
    }


type WFSResponseMode
    = XML
    | JSON


type Filter
    = EqualTo String String
    | Like String String
    | And Filter Filter
    | Or Filter Filter


type alias NormalizeEsriRequest =
    { from : ResponseFrom
    , esri : String
    , value : Maybe EsriResponse
    , showGeometry : Bool
    }


type alias EsriResponse =
    { features : List EsriFeature
    }


type alias EsriFeature =
    { properties : Dict String EsriProperty
    , geometry : Maybe String
    }


type EsriProperty
    = Esri_String String
    | Esri_Float Float
    | Esri_Int Int
    | Esri_Nil


normalizeEsriProcess : E.Value -> Msg
normalizeEsriProcess value =
    let
        ret =
            D.decodeValue decodeNormalizeEsri value
    in
    case ret of
        Ok resp ->
            Internal <| EsriRespondedConvert resp

        Err err ->
            Internal <| EsriRespondedConvertError "normalize"


esriDecoder : Decoder ReferenceLayerConfig
esriDecoder =
    D.succeed Esri
        |> required "esri" esriConfigDecoder


esriConfigDecoder : Decoder Esri_Config
esriConfigDecoder =
    D.succeed Esri_Config
        |> required "url" D.string
        |> optional "tokenKey" (D.maybe D.string) Nothing
        |> optional "layers" (D.list D.string) []
        |> optional "propertyNames" (D.list D.string) []
        |> optional "idField" D.string "id"
        |> optional "nameField" D.string "name"


encodeEsriConfig : Esri_Config -> E.Value
encodeEsriConfig conf =
    E.object
        [ ( "url", E.string conf.url )
        , ( "tokenKey", conf.tokenKey |> Maybe.map E.string |> Maybe.withDefault E.null )
        , ( "layers", E.list E.string conf.layers )
        , ( "propertyNames", E.list E.string conf.propertyNames )
        , ( "idField", E.string conf.idField )
        , ( "nameField", E.string conf.nameField )
        ]


encodeNormalizeEsri : NormalizeEsriRequest -> E.Value
encodeNormalizeEsri request =
    E.object
        [ ( "from", E.string <| responseFromToString request.from )
        , ( "esri", request.esri |> E.string )
        , ( "showGeometry", E.bool request.showGeometry )
        ]


decodeNormalizeEsri : Decoder NormalizeEsriRequest
decodeNormalizeEsri =
    D.succeed NormalizeEsriRequest
        |> required "from" decodeResponseFrom
        |> required "esri" D.string
        |> required "value" (D.maybe esriResponseDecoder)
        |> required "showGeometry" D.bool


esriRequestById : Dict String MapService -> Int -> Esri_Config -> Cmd Msg
esriRequestById services id config =
    let
        params =
            [ ( "outSR", "3857" )
            , ( "where", config.idField ++ "=" ++ String.fromInt id ) --TODO: This needs to come from the config

            --, ( "f", "pjson" )
            , ( "returnGeometry", "true" )
            , ( "f", "geojson" )
            ]
                ++ (if List.length config.propertyNames > 0 then
                        [ ( "outFields", String.join "," config.propertyNames ) ]

                    else
                        []
                   )
    in
    Cmd.batch <|
        List.map
            (\lyr ->
                Http.riskyRequest
                    { method = "GET"
                    , headers = []
                    , url =
                        buildUrl { config | url = config.url ++ lyr ++ "/query" } services params
                    , body =
                        Http.emptyBody
                    , expect =
                        Http.expectString
                            handleEsriRequestByIdResponse

                    --TODO : Change this to by string to shim the geometry to WKT
                    --esriResponseDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    }
            )
            config.layers


esriRequest : Dict String MapService -> Esri_Config -> Cmd Msg
esriRequest services config =
    let
        params =
            [ ( "outSR", "3857" )
            , ( "where", "1=1" )
            , ( "f", "pjson" )
            , ( "returnGeometry", "false" )
            ]
                ++ (if List.length config.propertyNames > 0 then
                        [ ( "outFields", String.join "," config.propertyNames ) ]

                    else
                        []
                   )
    in
    Cmd.batch <|
        List.map
            (\lyr ->
                Http.riskyRequest
                    { method = "GET"
                    , headers = []
                    , url =
                        buildUrl { config | url = config.url ++ lyr ++ "/query" } services params
                    , body =
                        Http.emptyBody
                    , expect =
                        Http.expectString
                            handleEsriRequestResponse
                    , timeout = Nothing
                    , tracker = Nothing
                    }
            )
            config.layers


handleEsriRequestResponse : Result Http.Error String -> Msg
handleEsriRequestResponse result =
    case result of
        Ok esriResponse ->
            Internal <| EsriResponded SetLayer esriResponse

        -- <| esriFeaturesToFeatures esriResponse.features
        Err err ->
            Internal <| EsriRespondedError "SetLayer"


handleEsriRequestByIdResponse : Result Http.Error String -> Msg
handleEsriRequestByIdResponse result =
    case result of
        Ok esriResponse ->
            Internal <| EsriResponded Select esriResponse

        --<| esriFeaturesToFeatures esriResponse.features
        Err err ->
            Internal <| EsriRespondedError "Select"



--Internal NoOp


esriFeatureToFeature : Maybe ReferenceLayer -> EsriFeature -> Feature
esriFeatureToFeature lyr feat =
    let
        ( idField, nameField ) =
            lyr
                |> Maybe.map
                    (\l ->
                        case l.config of
                            Esri config ->
                                ( config.idField, config.nameField )

                            _ ->
                                ( "id", "name" )
                    )
                |> Maybe.withDefault ( "id", "name" )
    in
    { id =
        Dict.get
            idField
            feat.properties
            |> Maybe.map
                (\itm ->
                    case itm of
                        Esri_String str ->
                            str |> String.toInt |> Maybe.withDefault -1

                        Esri_Int int ->
                            int

                        Esri_Float float ->
                            -1

                        Esri_Nil ->
                            -1
                )
            |> Maybe.withDefault -1
    , olid = -1
    , name =
        Dict.get
            nameField
            --TODO: Get this from config
            feat.properties
            |> Maybe.map
                (\itm ->
                    case itm of
                        Esri_String str ->
                            str

                        Esri_Int int ->
                            int |> String.fromInt

                        Esri_Float float ->
                            float |> String.fromFloat

                        Esri_Nil ->
                            "Unknown"
                )
            |> Maybe.withDefault "Unknown"
    , geometry = feat.geometry --TODO
    , acres =
        Dict.get
            "acres"
            feat.properties
            |> Maybe.map
                (\itm ->
                    case itm of
                        Esri_String str ->
                            str |> String.toFloat |> Maybe.withDefault -1

                        Esri_Float float ->
                            float

                        Esri_Int int ->
                            toFloat int

                        Esri_Nil ->
                            -1
                )
    }


esriFeaturesToFeatures : Maybe ReferenceLayer -> List EsriFeature -> List Feature
esriFeaturesToFeatures ref feats =
    List.map
        (esriFeatureToFeature ref)
        feats


esriResponseDecoder : Decoder EsriResponse
esriResponseDecoder =
    D.succeed EsriResponse
        |> required "features" decodeEsriFeatures


decodeEsriFeatures : Decoder (List EsriFeature)
decodeEsriFeatures =
    D.list decodeEsriFeature


decodeEsriFeature : Decoder EsriFeature
decodeEsriFeature =
    D.succeed EsriFeature
        |> required "properties" (D.dict decodeEsriProperty)
        |> optional "geometry" (D.maybe D.string) Nothing


decodeEsriProperty : Decoder EsriProperty
decodeEsriProperty =
    D.oneOf
        [ D.string
            |> D.andThen (\v -> D.succeed <| Esri_String v)
        , D.int
            |> D.andThen (\v -> D.succeed <| Esri_Int v)
        , D.float
            |> D.andThen (D.succeed << Esri_Float)
        , D.succeed Esri_Nil
        ]


hideReferenceLayer : Model -> ( Model, Cmd Msg )
hideReferenceLayer model =
    ( model
    , hideReferenceLayerDisplay ()
    )

clear : Model -> ( Model, Cmd Msg )
clear model =
    ( model
    , clearReferenceFeatures ()
    )

addToMap : Model -> ( Model, Cmd Msg )
addToMap model =
    ( model
    , addReferenceItems ()
    )