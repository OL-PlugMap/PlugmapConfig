port module PlugMap.Plugins.Drawing exposing (..)

import Html
    exposing
        ( Html
        , div
        , text
        , i
        , button
        , input
        , span
        , select
        , option
        )
import Html.Attributes exposing (id, class, title, src, value, type_, classList)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D exposing (Value, Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode exposing (map)
import Json.Encode as E
import Json.Encode.Extra as Ex
import Maybe.Extra exposing (unwrap)



{- 
    This port will tell the drawing plugin to start a new draw.
    Any draw in progress will be canceled
    This requires a shape and an ID
-}
port startDrawingWithMode : E.Value -> Cmd msg

port enableModify : () -> Cmd msg

port disableModify : () -> Cmd msg

port disableDraw : () -> Cmd msg

port selectById : Int -> Cmd msg

port deleteFeatureById : Int -> Cmd msg

port recalcEnded : (D.Value -> msg) -> Sub msg

port setExtentByFeatueId : E.Value -> Cmd msg

port hideDrawingCmd : () -> Cmd msg

port flattened : (D.Value -> msg) -> Sub msg


{-
    The ports below are mostly copied from CWS. We will need to clean these up and get rid of a bunch later
-}
port initDrawingCmd : E.Value -> Cmd msg


port enableDrawingCmd : () -> Cmd msg


port disableDrawingCmd : () -> Cmd msg


port unloadDrawingPlugin : () -> Cmd msg


port setWKT : E.Value -> Cmd msg

port setWKTs : E.Value -> Cmd msg


port setBuffer : E.Value -> Cmd msg


port setModeCmd : E.Value -> Cmd msg



--Get WKTs


port getFeatureWKTs : () -> Cmd msg


port getFeatureWKTsResult : (List String -> msg) -> Sub msg



--Flatten and get WKT


port getFeatureWKTsFlat : () -> Cmd msg


port getFeatureWKTsFlatResult : (List String -> msg) -> Sub msg



--State Messages


port drawingStarted : (() -> msg) -> Sub msg

--When the drawing has been finished first we call the drawingEnd followed by the drawingEnded when it is calculated
port drawingEnd : (() -> msg) -> Sub msg

port drawingEnded : (D.Value -> msg) -> Sub msg

port modifyEnd : (D.Value -> msg) -> Sub msg

port addWKTsToMap : (E.Value) -> Cmd msg

port addReferenceItems : () -> Cmd msg

--Calculate area


port plugmap_drawing_getArea : () -> Cmd msg


port plugmap_drawing_getAreaResponse : (Float -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ drawingStarted (always DrawingStart)
        , drawingEnded handleDrawingEnded
        , flattened handleFlattened
        , drawingEnd  (always (Internal NoOp)) --handleDrawingEnd
        , modifyEnd handleModifyEnd
        , recalcEnded handleRecalcEnd
        ]

handleDrawingEnd : Msg
handleDrawingEnd =
    Internal NoOp --TODO

handleDrawingEnded : D.Value -> Msg
handleDrawingEnded val =
    case D.decodeValue spatialDecoder val of
        Ok spat ->
            DrawingEnd spat
        Err err ->
            Internal NoOp


handleFlattened : D.Value -> Msg
handleFlattened val =
    case D.decodeValue (D.maybe spatialDecoder) val of
        Ok flat ->
            ReceivedFlat flat
        Err err ->
            Internal NoOp

handleRecalcEnd : D.Value -> Msg
handleRecalcEnd val =
    case D.decodeValue (D.maybe flatRecalcDecoder) val of
        Ok flat ->
            RecalculatedFlat flat
        Err err ->
            Internal NoOp

type alias FlatRecalc =
    { flat : String
    , flatArea : Float
    }

flatRecalcDecoder : Decoder FlatRecalc
flatRecalcDecoder =
    D.succeed FlatRecalc
        |> required "flat" D.string
        |> required "flatArea" D.float


spatialDecoder : Decoder ISpatial
spatialDecoder =
    D.succeed ISpatial
        |> required "type_" D.string
        |> required "wkt" D.string
        |> required "acres" D.float
        |> required "id" (D.maybe D.int)


type alias AddSpatial =
    { wkt : String
    , acres : Float
    }

addSpatialEncoder : AddSpatial -> E.Value
addSpatialEncoder spatial =
    E.object
        [ ( "wkt", E.string spatial.wkt )
        , ( "acres", E.float spatial.acres )
        ]


type alias Model =
    { wkts : List String
    , flatWKTs : List String
    , hasChanges : Bool
    , editing : Bool
    , enabled : Bool
    , controlEnabled : Bool
    , buffer : Float
    , mode : Mode
    , options : Options
    , area : CalculatedArea
    , requestedFlatWKTs : Bool
    , id : String
    , initialized : Bool
    , nextExpectedId : Maybe Int
    }


type CalculatedArea
    = NotCalculated
    | Calculating
    | Calculated Float


type Mode
    = Polygon
    | Point
    | Line
    | Circle
    | Modify


type OMsg
    = GotFeatures (List String)
    | GotFlatFeatures (List String)
    | ToggleControlClicked
    | BufferChanged Float
    | ModeChanged Mode
    | Clear
    | GotArea Float

type Msg
    = Internal InternalMsg
    | DrawingStart
    | DrawingEnd ISpatial
    | ModifyEnd ModifiedSpatial
    | RecalculatedFlat (Maybe FlatRecalc)
    | ReceivedFlat (Maybe ISpatial)

type InternalMsg
    = NoOp


type OutMsg
    = Changed
    | WKTsAvailable (List String)
    | FlatWKTsAvailable (List String) Bool
    | AreaChanged Float


getArea : Model -> Float
getArea model =
    case model.area of
        Calculated area ->
            area
        _ ->
            0 --TODO: Return nothing instead

requestAreaFlat : Model -> ( Model, Cmd Msg)
requestAreaFlat model =
    ( model
    , plugmap_drawing_getArea ()
    )

modeToText : Mode -> String
modeToText mode =
    case mode of
        Polygon ->
            "Polygon"

        Point ->
            "Point"

        Line ->
            "Line"

        Circle ->
            "Circle"

        Modify ->
            "Modify"


textToMode : String -> Mode
textToMode text =
    let
        lower =
            (String.toLower text)
    in
        if lower == "polygon" then
            Polygon
        else if lower == "point" then
            Point
        else if lower == "line" then
            Line
        else if lower == "circle" then
            Circle
        else if lower == "modify" then
            Modify
        else
            Polygon


type alias Options =
    { enableMultishape : Bool
    }


defaultOptions : Options
defaultOptions =
    { enableMultishape = False
    }


defaultModel : Model
defaultModel =
    Model
        []
        []
        False
        False
        False
        True
        5.0
        Polygon
        defaultOptions
        NotCalculated
        False
        ""
        False
        Nothing


hasChanges : Model -> Bool
hasChanges model =
    model.hasChanges


isEditing : Model -> Bool
isEditing model =
    model.editing


requestNewWKTs : Model -> ( Model, Cmd Msg )
requestNewWKTs model =
    ( model
    , Cmd.batch
        [ getFeatureWKTs ()
        , getFeatureWKTsFlat ()
        ]
    )


requestNewWKTsFlat : Model -> ( Model, Cmd Msg )
requestNewWKTsFlat model =
    ( { model | requestedFlatWKTs = True }
    , getFeatureWKTsFlat ()
    )


getWKTs : Model -> List String
getWKTs model =
    model.wkts


getFlatWKTs : Model -> List String
getFlatWKTs model =
    model.flatWKTs


setWKTAsSource : Model -> Int -> String -> ( Model, Cmd Msg )
setWKTAsSource model id wkt =
    ( { model
        | wkts = [ wkt ]
        , hasChanges = True
      }
    , setWKT
        <| E.object
            [ ( "id", E.int id )
            , ( "wkt", E.string wkt )
            ]
    )

encodeWKTSrc : { id : Int, wkt : String } -> E.Value
encodeWKTSrc src =
    E.object
        [ ( "id", E.int src.id )
        , ( "wkt", E.string src.wkt )
        ]


setWKTsAsSource : Model -> List { id : Int, wkt : String } -> Bool -> ( Model, Cmd Msg )
setWKTsAsSource model wkts clr =
    ( { model
        | wkts = wkts |> List.map (.wkt)
        , hasChanges = False
      }
    , setWKTs
        <| E.object
            [ ("clear", E.bool clr )
            , ("wkts", E.list encodeWKTSrc wkts)
            ]
    )

clear : Model -> Cmd Msg
clear model =
    setWKTAsSource model 0 ""
    |> (\(a,b) -> b)




saved : Model -> ( Model, Cmd Msg )
saved model =
    ( { model
        | hasChanges = False
      }
    , Cmd.none
    )


initWithWKT : String -> String -> Int -> Bool -> Bool -> ( Model, Cmd Msg )
initWithWKT id wkt trackingID startEnabled hasPriorChanges =
    let
        enableCmd =
            if startEnabled then
                enableDrawingCmd ()
            else
                disableDrawingCmd ()
    in
        ( { defaultModel
            | wkts = [ wkt ]
            , controlEnabled = startEnabled
            , hasChanges = hasPriorChanges
            , id = id
        }
        , Cmd.batch
            [ initDrawingCmd (wrapId id <| encodeOptions <| defaultOptions)
            , setWKT
                <| E.object
                    [ ( "id", E.int trackingID )
                    , ( "wkt", E.string wkt )
                    ]
            , enableCmd
            ]
        )

init : ( Model, Cmd Msg )
init =
    ( defaultModel
    , Cmd.batch
        [ initDrawingCmd (wrapId "moop" <| encodeOptions <| defaultOptions)
        ]
    )

wrapId : String -> E.Value -> E.Value
wrapId id val =
    E.object
        [ ("id", E.string id)
        , ("data", val)
        ]


encodeOptions : Options -> E.Value
encodeOptions options =
    E.object
        [ ( "enableMultishape", E.bool options.enableMultishape )
        ]


encodeWkts : List String -> E.Value
encodeWkts wkts =
    E.list (E.string) wkts


denit : Model -> ( Model, Cmd Msg )
denit model =
    ( { model | initialized = False }
    , Cmd.batch
        [ disableDrawingCmd ()
        , unloadDrawingPlugin ()
        ]
    )


update : Model -> Msg -> ( Model, Cmd Msg, Maybe OutMsg )
update model msg =
    case msg of
        _ ->
            ( model, Cmd.none, Nothing )


handleStartCalculatingArea : Model -> ( Model, Cmd Msg, Maybe OutMsg )
handleStartCalculatingArea model =
    ( { model
        | area = Calculating
      }
    , Cmd.none
    , Nothing
    )


handleGotArea : Model -> Float -> ( Model, Cmd Msg, Maybe OutMsg )
handleGotArea model newArea =
    ( { model
        | area = Calculated newArea
      }
    , Cmd.none
    , Just (AreaChanged newArea)
    )




handleModeChanged : Model -> Mode -> ( Model, Cmd Msg, Maybe OutMsg )
handleModeChanged model mode =
    ( { model | mode = mode }
    , setModeCmd (encodeModeChange mode)
    , Nothing
    )


encodeModeChange : Mode -> E.Value
encodeModeChange mode =
    E.object
        [ ( "mode", E.string (modeToText mode) )
        ]


handleBufferChanged : Model -> Float -> ( Model, Cmd Msg, Maybe OutMsg )
handleBufferChanged model newValue =
    let
        setValue =
            if newValue < 5 then
                5.0
            else
                newValue

        changeCmd =
            if setValue /= model.buffer then
                setBuffer (encodeBuffer setValue)
            else
                Cmd.none
    in
        ( { model | buffer = setValue }
        , changeCmd
        , Nothing
        )


encodeBuffer : Float -> E.Value
encodeBuffer value =
    E.object
        [ ( "buffer", E.float value )
        ]


handleToggleClicked : Model -> ( Model, Cmd Msg, Maybe OutMsg )
handleToggleClicked model =
    let
        newEnabled =
            if model.controlEnabled then
                not model.enabled
            else
                False

        newCommands =
            if newEnabled then
                enableDrawingCmd ()
            else
                disableDrawingCmd ()
    in
        ( { model
            | enabled = newEnabled
          }
        , newCommands
        , Nothing
        )


enableTool : Model -> (Model, Cmd Msg)
enableTool model =
    if not model.enabled then
        ( 
            { model
            | enabled = True
            }
        , enableDrawingCmd ()
        )
    else
        ( model
        , Cmd.none
        )

disableTool : Model -> (Model, Cmd Msg)
disableTool model =
    if model.enabled then
        ( 
            { model
            | enabled = False
            }
        , disableDrawingCmd ()
        )
    else
        ( model
        , Cmd.none
        )

handleDrawingStart : Model -> ( Model, Cmd Msg, Maybe OutMsg )
handleDrawingStart model =
    ( { model
        | editing = True
      }
    , Cmd.none
    , Nothing
    )





handleGotFeatures : Model -> List String -> ( Model, Cmd Msg, Maybe OutMsg )
handleGotFeatures model features =
    ( { model | wkts = features }
    , Cmd.none
    , Just (WKTsAvailable features)
    )


handleGotFlatFeatures : Model -> List String -> ( Model, Cmd Msg, Maybe OutMsg )
handleGotFlatFeatures model features =
    ( { model | flatWKTs = features, requestedFlatWKTs = False }
    , Cmd.none
    , Just (FlatWKTsAvailable features model.requestedFlatWKTs)
    )


controlView : Model -> msg -> Bool -> Html msg
controlView model clickMsg visible =
    div [ class "control-bar-tool" ]
        [ div [ class "cwms-control" ]
            [ button
                [ onClick clickMsg 
                , title "Toggle Drawing Tools"
                , classList
                    [ ( "disabled", not model.controlEnabled )
                    ]
                ]
                [ i
                    [ class
                        (if visible then
                            "fa fa-times-circle"
                        else
                            "fa fa-pencil"
                        )
                    ]
                    []
                ]

            ]
        ]

optionView : Model -> Html OMsg
optionView model =
        subControlView model

enableableAction : Msg -> Bool -> Msg
enableableAction msg enabled =
    if enabled then
        msg
    else
        Internal NoOp


subControlView : Model -> Html OMsg
subControlView model =
    div [ class "options" ]
        [ div 
            [ class "flex_row" ]
            [ span [] [ text "Mode" ]
            ]
        , div 
            [ class "flex_row" ]
            [ (modeControlView model)
            ]
        , div 
            [ class "flex_row" ]
            [ ( if model.mode == Point || model.mode == Line then
                    span [] [ text "Buffer (ft)" ]
                else
                    div [] []
              )
            ]
        , div 
            [ class "flex_row" ]
            [ ( if model.mode == Point || model.mode == Line then
                    bufferControlView model
                else
                    div [] [] 
              )
            ]
        , clearView model
        ]
        


clearView : Model -> Html OMsg
clearView model =
    div
        [ class "flex_row" ]
        [ button
            [ onClick Clear
            , title "Clear"
            ]
            [ i [ class "fa fa-times" ] []
            ]
        ]


bufferControlView : Model -> Html OMsg
bufferControlView model =
    input
        [ type_ "number"
        , value (String.fromFloat model.buffer)
        , onInput
            (\x ->
                BufferChanged
                    (valueToFloatLimit x 5 10000)
            )
        ]
        []
        


modeToOption : Mode -> Html OMsg
modeToOption mode =
    option
        [ value (modeToText mode) ]
        [ text (modeToText mode) ]


modeControlView : Model -> Html OMsg
modeControlView model =
    select
        [ --onInput (\x -> (ModeChanged (textToMode x))) ]
            Html.Events.on "change" 
            (Json.Decode.map 
                (\newValue -> 
                    (ModeChanged (textToMode newValue))
                )
                Html.Events.targetValue
            )
        ]
        [ modeToOption Polygon
        , modeToOption Circle
        , modeToOption Point
        , modeToOption Line
        ]
        


valueToFloatLimit : String -> Float -> Float -> Float
valueToFloatLimit input minLimit maxLimit =
    let
        parsed =
            String.join "" (String.split "," input)

        valu =
            (case (String.toFloat parsed) of
                Just val ->
                    if val < maxLimit then
                        if val > minLimit then
                            val
                        else
                            minLimit
                    else
                        maxLimit

                Nothing ->
                    minLimit
            )
    in
        valu


stopAndUnload : Model -> ( Model, Cmd Msg)
stopAndUnload model =
    ( { model | enabled = False }
    , disableDrawingCmd ()
    )
    --TODO
    -- ( model
    -- , Cmd.none
    -- )

hide : Model -> ( Model, Cmd Msg )
hide model =
    ( { model | enabled = False }
    , hideDrawingCmd ()
    )

initIfNot : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initIfNot (model, cmd) =
    if not model.initialized then
        ( { model
            | initialized = True
            }
        , Cmd.batch 
            [ cmd
            , initDrawingCmd (wrapId model.id <| encodeOptions <| defaultOptions)
            ]
        )
    else
        (model, cmd)

enable : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
enable (model, cmd) =
    if not model.enabled then
        ( { model
            | enabled = True
            }
        , Cmd.batch 
            [ cmd
            , enableDrawingCmd ()
            ]
        )
    else
        (model, cmd)

setMode : Mode -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
setMode mode (model, cmd) =
    if model.mode /= mode then
        ( { model | mode = mode }
        , Cmd.batch 
            [ cmd
            , setModeCmd <| encodeModeChange mode
            ]
        )
    else
        ( model, cmd )

drawPoly : Model -> Int -> ( Model, Cmd Msg)
drawPoly model id =
    (   { model 
        | mode = Polygon
        , nextExpectedId = Just id
        }
    , startDrawingWithMode <| encodeStartDrawingWithModeOptions Polygon id
    )
    -- enable
    --     <| setMode Polygon
    --     <| initIfNot
    --     <| ( model, Cmd.none )

encodeStartDrawingWithModeOptions : Mode -> Int -> E.Value
encodeStartDrawingWithModeOptions mode id =
    E.object
        [ ( "mode", encodeMode mode )
        , ( "id", E.int id)
        ]

encodeMode : Mode -> E.Value
encodeMode mode =
    E.string
        <| modeToText mode



drawLine : Model -> Int -> ( Model, Cmd Msg )
drawLine model id =
    (   { model 
        | mode = Line
        , nextExpectedId = Just id
        }
    , startDrawingWithMode <| encodeStartDrawingWithModeOptions Line id
    )


drawPoint : Model -> Int -> ( Model, Cmd Msg )
drawPoint model id =
    (   { model 
        | mode = Point
        , nextExpectedId = Just id
        }
    , startDrawingWithMode <| encodeStartDrawingWithModeOptions Point id
    )


enableModifyDrawn : Model -> ( Model, Cmd Msg )
enableModifyDrawn model =
    ( model
    , enableModify ()
    )

disableModifyDrawn : Model -> ( Model, Cmd Msg )
disableModifyDrawn model =
    ( model
    , disableModify ()
    )

disableDrawDrawn : Model -> ( Model, Cmd Msg )
disableDrawDrawn model =
    ( model
    , disableDraw ()
    )
















{-
    This is how outside code will interact with our plugin and how we interact with ourselves
        Internal messages are to be used for talking to ourselves
        onNewColor is called when we get a new color. THis should be hooked up outside as we dont really care about the color in here ... yet
-}
type alias API msg =
    { onInternalMessage : Msg -> msg 
    , onDrawingStart : msg
    , onDrawingEnd : ISpatial -> msg
    , onModified : ModifiedSpatial -> msg
    , onRecalculatedFlat : Maybe FlatRecalc -> msg
    , onReceivedFlat : Maybe ISpatial -> msg
    }


type alias APITranslator parentMsg =
    Msg -> parentMsg


apiTranslator : API parentMsg -> APITranslator parentMsg
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage <| Internal m
        DrawingStart ->
            td.onDrawingStart
        DrawingEnd spatial ->
            td.onDrawingEnd spatial
        ModifyEnd spatial ->
            td.onModified spatial
        RecalculatedFlat flat ->
            td.onRecalculatedFlat flat
        ReceivedFlat flat ->
            td.onReceivedFlat flat



type alias ISpatial =
  { type_ : String
  , wkt : String
  , acres : Float
  , id : Maybe Int
  }


type alias ModifiedSpatial =
    { modifiedShapes : List SimpleSpatial
    }

type alias SimpleSpatial =
    { type_ : String
    , wkt : String
    , acres : Float
    , id : Maybe Int
    , ol_uid : Maybe Int
    }


simpleSpatialDecoder : Decoder SimpleSpatial
simpleSpatialDecoder =
    D.succeed SimpleSpatial
        |> required "type_" D.string
        |> required "wkt" D.string
        |> required "acres" D.float
        |> required "id" (D.maybe D.int)
        |> required "ol_uid" (D.maybe D.int)

modifiedSpatialDecoder : Decoder ModifiedSpatial
modifiedSpatialDecoder =
    D.succeed ModifiedSpatial
        |> required "modifiedShapes" (D.list simpleSpatialDecoder)



handleModifyEnd : D.Value -> Msg
handleModifyEnd val =
    case D.decodeValue modifiedSpatialDecoder val of
        Ok spat ->
            ModifyEnd spat
        Err err ->
            Internal NoOp

lockModifyById : Model -> Int -> ( Model, Cmd Msg )
lockModifyById model id =
    ( model
    , selectById id
    )

deleteById : Model -> Int -> ( Model, Cmd Msg )
deleteById model id =
    ( model
    , deleteFeatureById id
    )

zoomToFeatureById : Model -> Int -> Int -> Int -> ( Model, Cmd Msg )
zoomToFeatureById model id leftPadding bottomPadding =
    ( model
    , setExtentByFeatueId 
        <| E.object 
            [ ( "id", E.int id )
            , ( "left", E.int leftPadding )
            , ( "bottom", E.int bottomPadding )
            ]
    )


addWKTs : Model -> List AddSpatial -> ( Model, Cmd Msg )
addWKTs model wkts =
    --TODO
    ( model
    , addWKTsToMap <| E.list addSpatialEncoder wkts
    )

addSelectedReferenceItems : Model -> List AddSpatial -> ( Model, Cmd Msg )
addSelectedReferenceItems model wkts =
    ( model
    , addReferenceItems ()
    )