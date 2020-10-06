port module PlugMap.Plugins.Upload exposing (..)

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
        , label
        )
import Html.Attributes exposing (id, class, title, src, value, type_, for, classList)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D exposing (Value, Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as E
import Json.Encode.Extra as Ex
import Maybe.Extra exposing (unwrap)
import Task

port openChooseFile : () -> Cmd msg

port fileUploaded : (D.Value -> msg ) -> Sub msg

port statusUpdate : (D.Value -> msg ) -> Sub msg

type Msg
    = Internal InternalMsg
    | FeaturesUploaded (List Feature)

type InternalMsg 
    = NoOp
    | UpdateStatus StatusUpdate
    | InternalFeaturesUploaded (List Feature)


type alias Model =
    { status : Status
    }

type alias Feature =
    { wkt: String
    , acres: Float
    }

type Status 
    = Idle
    | SelectingFile
    | Opening
    | Processing
    | CollectingFeatures
    | Converting
    | Unkinking
    | Calculating
    | Done
    | Error (List String)

uploadableStatus : Model -> Bool
uploadableStatus model =
    case model.status of
        Idle -> True
        SelectingFile -> True
        Done -> True
        Error _ -> True
        _ -> False

getStatusAsText : { status : Status } -> List String
getStatusAsText { status } =
    case status of
        Idle ->
            []
        SelectingFile ->
            ["Select a file to continue."]
        Opening -> 
            ["Opening your file."]
        Processing ->
            ["Processing your file."]
        CollectingFeatures ->
            ["Collecting features."]
        Converting ->
            ["Converting features."]
        Unkinking ->
            ["Processing features."]
        Calculating ->
            ["Gathering some more information on the features."]
        Done ->
            ["All processed. The features should appear shortly."]
        Error val ->
            [ "We ran into a problem with this file."
            ]
            ++ val


{-
    This is how outside code will interact with our plugin and how we interact with ourselves
        Internal messages are to be used for talking to ourselves
        onNewColor is called when we get a new color. THis should be hooked up outside as we dont really care about the color in here ... yet
-}
type alias API msg =
    { onInternalMessage : InternalMsg -> msg 
    , onFeaturesUploaded : List Feature -> msg
    }

type alias APITranslator parentMsg =
    Msg -> parentMsg


apiTranslator : API parentMsg -> APITranslator parentMsg
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage m
        FeaturesUploaded feats ->
            td.onFeaturesUploaded feats


chooseFile : Model -> ( Model, Cmd Msg)
chooseFile model =
    ( { model | status = SelectingFile }
    , openChooseFile ()
    )




defaultModel : Model
defaultModel =
    { status = Idle
    }



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fileUploaded fileUploadedProcess
        , statusUpdate processStatusUpdate
        ]


processStatusUpdate : D.Value -> Msg
processStatusUpdate value =
    case D.decodeValue statusUpdateDecoder value of
        Ok status ->
            Internal <| UpdateStatus <| status
        Err error ->
            Internal <| UpdateStatus <| { status = Error [ "Cannot parse status update" ], log = [] }


type alias StatusUpdate =
    { status : Status
    , log : List String
    }


statusUpdateDecoder : Decoder StatusUpdate
statusUpdateDecoder =
    D.succeed StatusUpdate
        |> required "status" statusDecoder
        |> optional "log" (D.list D.string) []

statusDecoder : Decoder Status
statusDecoder =
    D.oneOf
        [ D.string
            |> D.andThen 
                (\stringValue ->
                    ( case stringValue of
                        "SelectingFile" ->
                            D.succeed SelectingFile
                        "Opening" ->
                            D.succeed Opening
                        "Processing" ->
                            D.succeed Processing
                        "CollectingFeatures" ->
                            D.succeed CollectingFeatures
                        "Unkinking" ->
                            D.succeed Unkinking
                        "Converting" ->
                            D.succeed Converting
                        "Calculating" ->
                            D.succeed Calculating
                        "Done" ->
                            D.succeed Done
                        _ ->
                            D.fail "Unable to parse status"
                    )
                )
        , D.field "error" (D.list D.string)
            |> D.andThen
                (\stringValue ->
                    D.succeed <| Error stringValue
                )
        , D.succeed <| Error [ "Could not parse status" ]
        ]

fileUploadedProcess : D.Value -> Msg
fileUploadedProcess val =
    FeaturesUploaded []
    -- case D.decodeValue (D.list featureDecoder) val of
    --     Ok feats ->
    --         Internal <| InternalFeaturesUploaded feats
    --     Err err ->
    --         Internal NoOp



featureDecoder : Decoder Feature
featureDecoder =
    D.succeed Feature
        |> required "wkt" D.string
        |> required "acres" D.float


update : InternalMsg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        InternalFeaturesUploaded feats ->
            ( { model | status = Done}
            , Task.perform
                (\dontCareAboutThisValue ->
                    FeaturesUploaded
                        feats
                )
                <|
                    Task.succeed "Hello"
            )
        UpdateStatus status ->
            ( { model | status = status.status }
            , Cmd.none
            )
        _ ->
            noop model


noop : Model -> ( Model, Cmd Msg )
noop model =
    ( model
    , Cmd.none
    )

-- port initUpload : E.Value -> Cmd msg


-- port clearUploadLayer : () -> Cmd msg


-- port manualUpload : E.Value -> Cmd msg


-- port enableUpload : () -> Cmd msg


-- port disableUpload : () -> Cmd msg


-- port unloadUpload : () -> Cmd msg


-- port getUploadedFeatureWKTs : () -> Cmd msg


-- port shapeUploaded : (() -> msg) -> Sub msg


-- port getUploadedFeatureWKTsResult : (List String -> msg) -> Sub msg


-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.batch
--         [ shapeUploaded ShapeUploaded
--         , getUploadedFeatureWKTsResult GotFeatures
--         ]


-- type alias Model =
--     { wkts : List String
--     , hasChanges : Bool
--     , visible : Bool
--     , options : Options
--     , controlEnabled : Bool
--     }


-- type Msg
--     = NoOp
--     | ShapeUploaded ()
--     | GotFeatures (List String)
--     | ToggleControlClicked
--     | FileSelected String


-- type alias Options =
--     {}


-- defaultOptions : Options
-- defaultOptions =
--     {}


-- defaultModel : Model
-- defaultModel =
--     Model
--         []
--         False
--         False
--         defaultOptions
--         True


-- hasChanges : Model -> Bool
-- hasChanges model =
--     model.hasChanges


-- getWKTs : Model -> List String
-- getWKTs model =
--     model.wkts


-- clearWKTs : Model -> ( Model, Cmd Msg )
-- clearWKTs model =
--     ( { model
--         | wkts = []
--       }
--     , clearUploadLayer ()
--     )


-- init : List String -> Bool -> ( Model, Cmd Msg )
-- init wkts startEnabled =
--     let
--         enableCmd =
--             if startEnabled then
--                 enableUpload ()
--             else
--                 disableUpload ()
--     in
--         ( { defaultModel
--             | wkts = wkts
--             , controlEnabled = startEnabled
--           }
--         , Cmd.batch
--             [ initUpload (encodeOptions defaultOptions)
--             , enableCmd
--             ]
--         )


-- enableUploadPlugin : Model -> ( Model, Cmd Msg )
-- enableUploadPlugin model =
--     ( model, enableUpload () )


-- encodeOptions : Options -> E.Value
-- encodeOptions options =
--     E.object
--         []


-- encodeWkts : List String -> E.Value
-- encodeWkts wkts =
--     E.list (List.map (E.string) wkts)


-- denit : Model -> ( Model, Cmd Msg )
-- denit model =
--     ( model
--     , Cmd.batch
--         [ unloadUpload ()
--         ]
--     )


-- update : Model -> Msg -> ( Model, Cmd Msg, Bool )
-- update model msg =
--     case msg of
--         ShapeUploaded _ ->
--             handleShapeUploaded model

--         GotFeatures wkts ->
--             handleGotFeatures model wkts

--         ToggleControlClicked ->
--             handleToggleClicked model

--         FileSelected _ ->
--             handleFileSelected model

--         _ ->
--             ( model, Cmd.none, False )


-- handleFileSelected : Model -> ( Model, Cmd Msg, Bool )
-- handleFileSelected model =
--     ( model
--     , manualUpload (E.string "manualUpload")
--     , False
--     )


-- handleShapeUploaded : Model -> ( Model, Cmd Msg, Bool )
-- handleShapeUploaded model =
--     ( model
--     , getUploadedFeatureWKTs ()
--     , False
--     )


-- handleGotFeatures : Model -> List String -> ( Model, Cmd Msg, Bool )
-- handleGotFeatures model wkts =
--     ( { model
--         | hasChanges = True
--         , wkts = wkts
--       }
--     , Cmd.none
--     , True
--     )


-- handleToggleClicked : Model -> ( Model, Cmd Msg, Bool )
-- handleToggleClicked model =
--     ( { model
--         | visible = (not model.visible)
--       }
--     , Cmd.none
--     , False
--     )


-- enableableAction : Msg -> Bool -> Msg
-- enableableAction msg enabled =
--     if enabled then
--         msg
--     else
--         NoOp


-- controlView : Model -> Html Msg
-- controlView model =
--     div [ class "control-bar-tool" ]
--         [ div
--             [ class "cwms-control" ]
--             [ label
--                 [ for "manualUpload"
--                 , title "Upload Shapefile"
--                 , classList
--                     [ ( "disabled", not model.controlEnabled )
--                     ]
--                 ]
--                 [ i
--                     [ class "fa fa-upload" ]
--                     []
--                 , (if model.controlEnabled then
--                     input
--                         [ type_ "file"
--                         , onInput FileSelected
--                         , id "manualUpload"
--                         ]
--                         []
--                    else
--                     div [] []
--                   )
--                 ]
--             ]
--         ]
