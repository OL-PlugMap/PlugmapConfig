port module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Browser.Navigation as Nav

import Element as El exposing (..)
import Element.Keyed as Keyed
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region


import Html
import Html.Keyed
import Html.Attributes


import Json.Decode as D exposing (Decoder, Value, dict, errorToString, int, string, Error)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E exposing (Value)
import Json.Encode.Extra as EEx

import Url exposing (Protocol(..), Url)

import Process
import Task



import PlugMap.Core.Options as MapOptions
import PlugMap.Plugins.Themes as Themes

import Dict

port logError : (List String) -> Cmd msg

---- MODEL ----


type alias Model =
    { state : State
    , key : Nav.Key
    , editorMode : EditingMode
    }

type State 
    = Loading
    | MainMenu
    | LoadConfig LoadMode LoadStep
    | Editing Config
    | Export Config
    | Error


type LoadMode
    = FromText
    | FromPAM

type LoadStep
    = New String
    | Parse String
    | ErrorParsing String
    | LoadOk Config
    | BeepBoop


type alias Config = 
    { options : MapOptions.Options
    , themes : Themes.Model
    }



init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init params url key =
    ( { state = MainMenu, key = key, editorMode = Hierarchical }
    , Process.sleep 500 
            |> Task.perform ( always <| Init )
    )



---- UPDATE ----


type Msg
    = NoOp
    | Init
    | Menu MenuAction
    | Load LoadMode LoadStep
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

    | StartParsing String
    | ParseIt String
    | StartEditing EditingMode Config
    
    | UpdateConfig Config
    | UpdateThemes Themes.Model

    | ExportClick Config

type EditingMode
    = Hierarchical
    | Flat
    | SequenceLayers


type MenuAction
    = Back
    | ConfigFromText
    | ConfigFromPAM

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            ( { model | state = MainMenu }, Cmd.none )
        Menu action ->
            case action of
                ConfigFromPAM ->
                    ( { model | state = 
                        LoadConfig 
                            FromPAM
                            <| New ""
                        }
                    , Cmd.none 
                    )
                ConfigFromText ->
                    ( { model | state = 
                        LoadConfig
                            FromText
                            <| New ""
                        }
                    , Cmd.none
                    )
                _ ->
                    ( model, Cmd.none )

        StartParsing text ->
            ( { model 
                | state = 
                    LoadConfig
                        FromText
                        <| Parse text
                }
            , Process.sleep 1000
                |> Task.perform ( always <| ParseIt text )  
            )
        ParseIt text ->
            let
                mop =
                    D.decodeString (MapOptions.optionsFromConfigDecoder) text

                mt =
                    D.decodeString (Themes.themesDecoder) text
       
            in
            case ( mop, mt ) of
                (Ok options, Ok thms ) ->
                    let
                        no = 
                            Config
                                options
                                thms
                        ns = LoadConfig FromText <| LoadOk no
                    in
                    ( { model
                        | state = ns
                        }
                    , Cmd.none
                    )
                (Ok options, Err err) ->
                    ( model
                    , logError 
                        [ "Error Decoding Themes"
                        , D.errorToString err
                        ] 
                    )
                ( Err err, Ok themes) ->
                    ( model
                    , logError 
                        [ "Error Decoding Options"
                        , D.errorToString err
                        ] 
                    )
                ( Err err, Err err2 ) ->
                    ( model
                    , logError 
                        [ "Error Decoding Options"
                        , D.errorToString err
                        , "Error Decoding Themes"
                        , D.errorToString err
                        ] 
                    )

        Load mode step ->
            ( { model | state = LoadConfig mode step }, Cmd.none )

        StartEditing mode cfg ->
            ( { model 
                | state = Editing cfg 
                , editorMode = mode
                }
            , Cmd.none
            )

        UpdateConfig nCfg ->
            ( { model | state = Editing nCfg }
            , Cmd.none
            )

        UpdateThemes nThms ->
            case model.state of
                Editing cfg ->
                    let
                        nCfg = { cfg | themes = nThms }
                    in
                    ( { model | state = Editing nCfg }
                    , Cmd.none
                    )
                _ -> ( model, Cmd.none )

        ExportClick cfg ->
            (   { model 
                | state = Export cfg 
                }
            , Cmd.none
            )


        _ ->
            ( model, Cmd.none )


menuOptions : List MenuAction
menuOptions =
    [ ConfigFromText
    , ConfigFromPAM
    ]








---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Plugmap Config Editing Tool"
    , body =
        [ layout 
            [ height fill
            , width fill
            , Font.family
                [ Font.typeface "Lato"
                , Font.sansSerif
                ] 
            ] 
            <| switchView model 
            
        ]
    }

switchView : Model -> Element Msg
switchView model =
    case model.state of
        Loading -> 
            text "Loading"
        MainMenu ->
            menuView
        LoadConfig mode step ->
            loadView mode step
        Editing cfg ->
            configEditorView model.editorMode cfg
        Export cfg ->
            exportView cfg
        Error ->
            el [] <|
                paragraph []
                    [ el [ Font.bold ] (text "Oh no!")
                    , text "We've encountered a problem loading the Workflow."
                    ]

encodeConfigDB : Config -> E.Value
encodeConfigDB config =
    E.object
        [ ( "layerCategories", E.list Themes.encodeLayerCategoryDB config.themes.layerCategories )
        , ( "layerGroups", E.list Themes.encodeLayerGroupDB <| Dict.values config.themes.layerGroupRepo )
        , ( "layers", E.list Themes.encodeLayerDB <| Dict.values config.themes.layerRepo )
        , ( "options", MapOptions.encodeOptions config.options )
        ]


exportView : Config -> Element Msg
exportView config =
    let
        txtVal = 
            E.encode 4 
            <| encodeConfigDB config
    in
    column
        [ width fill
        ]
        [ row
            [ width fill
            ]
            [ text "Config Export"
            , el [ alignRight ] <| appButton "< Back To Editor" ( StartEditing Hierarchical config ) True
            , el [ alignRight ] <| appButton "Start Over" ( Init ) True
            ]
        , Input.multiline
            [ width fill
            ]
            { onChange = (\t -> NoOp)
            , text = txtVal
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Database Config"
            , spellcheck = False
            }
        , text "Map direct config is todo"
        ]


configEditorView : EditingMode -> Config -> Element Msg
configEditorView mode cfg =
    column
        [ centerX
        , spacing 25
        , width fill
        , height fill
        , padding 5
        ]
        [ editHeader mode cfg
        , spacer
        , column
            [ centerX
            , spacing 25
            , width fill
            , height fill
            , padding 5
            , scrollbarY
            ]
            [ mapOptionsView cfg.options
                |> El.map 
                    (\v -> 
                        UpdateConfig { cfg | options = v } 
                    )
            , spacer
            , case mode of
                Hierarchical ->
                    hierarchicalMapThemesView cfg.themes
                Flat ->
                    text "TODO"
                SequenceLayers -> 
                    text "TODO"
            ]
        ]


--TODO: These buttons will probably change if/when we add other modes
editHeader : EditingMode -> Config ->  Element Msg
editHeader mode config =
    let 
        othermode =
            case mode of
                Hierarchical -> Flat
                Flat -> SequenceLayers
                SequenceLayers -> Hierarchical
    in
    row
        [ width fill
        , padding 5
        ]
        [ text "Config Editor"
        , el [ centerX ] <| appButton "Start Over" Init True
        , el [ centerX ] <| appButton ("Switch to " ++ (editingModeToString othermode)) ( StartEditing othermode config ) True
        , el [ alignRight ] <| appButton "Export >" ( ExportClick config ) True
        ]

editingModeToString : EditingMode -> String
editingModeToString mode =
    case mode of
        Hierarchical -> "Hierarchical"
        Flat -> "Flat"
        SequenceLayers -> "Sequencer"

spacer : Element msg
spacer = 
    el 
        [ width fill
        , Border.widthEach 
            { top = 0
            , left = 0
            , right = 0
            , bottom = 1 
            }
        ]
        none




mapOptionsView : MapOptions.Options -> Element MapOptions.Options
mapOptionsView opts =
    let
        (x,y) = opts.center
    in
    column
        [ spacing 15
        , width fill
        , padding 5
        ]
        [ text "Map Options"
        , row 
            [ spacing 15
            , width fill
            ]
            [ el [ width <| px 125 ] <| text "Center (x,y)"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            nv = String.toFloat text
                        in
                            case nv of
                                Just i ->
                                    { opts | center = (i, y) }
                                _ ->
                                    opts
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "X"
                , text = 
                    String.fromFloat x
                }
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            nv = String.toFloat text
                        in
                            case nv of
                                Just i ->
                                    { opts | center = (x, i ) }
                                _ ->
                                    opts
                    )
                , placeholder = Nothing
                , label = Input.labelHidden  <|  "y"
                , text = 
                    String.fromFloat y
                }
            ]
        , row 
            [ spacing 15
            ]
            [ el [ width <| px 125 ] <| text "Max Zoom"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            nv = String.toInt text
                        in
                            { opts | maxZoom = nv }
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Max Zoom"
                , text = 
                    Maybe.map String.fromInt opts.maxZoom |> Maybe.withDefault ""
                }
            ]
        , row 
            [ spacing 15
            ]
            [ el [ width <| px 125 ] <| text "Min Zoom"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            nv = String.toInt text
                        in
                            { opts | minZoom = nv }
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Min Zoom"
                , text = 
                    Maybe.map String.fromInt opts.minZoom |> Maybe.withDefault ""
                }
            ]
        , row 
            [ spacing 15
            ]
            [ el [ width <| px 125 ] <| text "Target"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { opts | target = text }
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Target"
                , text = 
                    opts.target
                }
            ]
        , row 
            [ spacing 15
            ]
            [ el [ width <| px 125 ] <| text "Zoom"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            nv = String.toInt text
                        in
                            case nv of
                                Just i ->
                                    { opts | zoom = i }
                                _ ->
                                    opts
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Zoom"
                , text = 
                    String.fromInt opts.zoom
                }
            ]
        ]

hierarchicalMapThemesView : Themes.Model -> Element Msg
hierarchicalMapThemesView themes =
    column
        [ spacing 15
        , width fill
        , Border.width 1
        , padding 5
        ]
        [ text "Themes"
        , column
            [ spacing 15
            , width fill
            ]
            <| List.intersperse spacer
            <| List.indexedMap (mapThemesCategoryView themes) themes.layerCategories 
        ]

makeLabel : String -> Element msg
makeLabel val =
    el 
        [ width <| px 150
        ]
        <| text val

mapThemesCategoryView : Themes.Model -> Int -> Themes.LayerCategory -> Element Msg
mapThemesCategoryView themes index category =
    let
        before = List.take index themes.layerCategories
        after = List.drop (index + 1) themes.layerCategories
        updoot newCat =
            { themes
            | layerCategories = before ++ [ newCat ] ++ after
            }
            |> UpdateThemes
    in
    column
        [ spacing 5 
        , width fill
        ]
        [ row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Key"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            newCat = 
                                { category | key = Themes.stringToCategoryKey text }
                        in
                        updoot newCat
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Key"
                , text = 
                    Themes.categoryKeyToString category.key
                }
            ]
        , row 
            [ spacing 10 
            ]
            [ makeLabel "Name" 
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            newCat = 
                                { category | name = text }
                        in
                        updoot newCat
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Name"
                , text = 
                    category.name
                }
            ]
        , row
            [ spacing 10 
            ]
            [ makeLabel "Polyselective"
            , Input.checkbox
                []
                { onChange = 
                    (\val ->
                        let
                            newCat = 
                                if val then
                                    case category.selection of
                                        Themes.Monoselection kv ->
                                            { category 
                                            | selection = 
                                                Themes.Polyselection 
                                                    <| case kv of
                                                        Just k -> [k]
                                                        Nothing -> []
                                            }
                                        Themes.EnforcedMonoselection k ->
                                            { category 
                                            | selection = 
                                                Themes.Polyselection 
                                                    [k]
                                            }
                                        _ ->
                                            { category 
                                            | selection = 
                                                Themes.Polyselection []
                                            }
                                else
                                    case category.selection of
                                        Themes.Polyselection kv ->
                                            { category 
                                            | selection = 
                                                Themes.Monoselection <| List.head kv
                                            }
                                        Themes.EnforcedMonoselection k ->
                                            { category 
                                            | selection = 
                                                Themes.Monoselection <| Just k
                                            }
                                        _ ->
                                            { category 
                                            | selection = 
                                                Themes.Monoselection Nothing
                                            }

                        in
                        updoot newCat
                    )
                , icon = Input.defaultCheckbox
                , label = Input.labelHidden <|  "Hidden"
                , checked = 
                    case category.selection of
                        Themes.Polyselection _ -> True
                        _ -> False
                }
            ]
        , row
            [ spacing 10 
            ]
            [ makeLabel "Hidden"
            , Input.checkbox
                []
                { onChange = 
                    (\val ->
                        let
                            newCat = 
                                { category | hidden = val }
                        in
                        updoot newCat
                    )
                , icon = Input.defaultCheckbox
                , label = Input.labelHidden <|  "Hidden"
                , checked = 
                    category.hidden
                }
            ]
        , row
            [ spacing 10 
            ]
            [ makeLabel "Multiphasic"
            , Input.checkbox
                []
                { onChange = 
                    (\val ->
                        let
                            newCat = 
                                { category | multiphasic = val }
                        in
                        updoot newCat
                    )
                , icon = Input.defaultCheckbox
                , label = Input.labelHidden <|  "Multiphasic"
                , checked = 
                    category.multiphasic
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Transparency"
            , row
                [ width fill
                , spacing 5
                ]
                [ makeLabel <| ( String.fromInt ( floor ( category.transparency * 100 ) ) ) ++ "%"
                , Input.slider
                    [ width fill
                    , Bg.color <| rgb 0.9 0.9 0.9
                    ]
                    { onChange = 
                        (\val ->
                            let
                                newCat = 
                                    { category | transparency = val }
                            in
                            updoot newCat
                        )
                    , label = Input.labelHidden <|  "Multiphasic"
                    , min = 0
                    , max = 1
                    , step = Just 0.01
                    , value = category.transparency
                    , thumb = Input.defaultThumb
                    }
                ]
            ]
        , row
            [ spacing 10 
            ]
            [ makeLabel "Open By Default"
            , Input.checkbox
                []
                { onChange = 
                    (\val ->
                        let
                            newCat = 
                                { category 
                                | openness = 
                                    if val then
                                        Themes.Open
                                    else
                                       Themes.Closed
                                }
                        in
                        updoot newCat
                    )
                , icon = Input.defaultCheckbox
                , label = Input.labelHidden <|  "Hidden"
                , checked = 
                    if category.openness == Themes.Open then
                        True
                    else
                        False
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Groups"
            , column
                [ width fill
                ]
                <| List.map 
                    ( renderGroup themes category index
                    )
                    category.layerGroups
            ]
            
        ]

renderGroup : Themes.Model -> Themes.LayerCategory -> Int -> Themes.GroupKey -> Element Msg
renderGroup themes category index group =
    let
        grp = Themes.getGroupByKey themes.layerGroupRepo group
    in
    column
        [ spacing 5
        , Border.width 1
        , width fill
        , padding 5
        ]
        [ text <| Themes.groupKeyToString group
        , case grp of
            Just g ->
                groupRenderer themes category index g
            Nothing ->
                text "No Group Found!"
        ]





groupRenderer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.LayerGroup -> Element Msg
groupRenderer themes category index group =
    let 
        updoot grp =
            Themes.updateGroup themes grp |> UpdateThemes
    in
    column
        [ width fill
        , spacing 3
        ]
        [ row 
            [ spacing 10
            , width fill 
            ]
            [ makeLabel "Key" 
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            newCat = 
                                { group 
                                | key = 
                                    Themes.stringToGroupKey text
                                }
                        in
                        updoot newCat
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Key"
                , text = 
                    group.key |> Themes.groupKeyToString
                }
            ]
        , row 
            [ spacing 10 
            , width fill 
            ]
            [ makeLabel "Name" 
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            newCat = 
                                { group 
                                | name = 
                                    if String.length text > 0 then
                                        Just text
                                    else
                                        Nothing
                                }
                        in
                        updoot newCat
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Name"
                , text = 
                    group.name |> Maybe.withDefault ""
                }
            ]
        , row
            [ spacing 10 
            ]
            [ makeLabel "Open By Default"
            , Input.checkbox
                []
                { onChange = 
                    (\val ->
                        let
                            newCat = 
                                { group 
                                | openness = 
                                    if val then
                                        Themes.Open
                                    else
                                       Themes.Closed
                                }
                        in
                        updoot newCat
                    )
                , icon = Input.defaultCheckbox
                , label = Input.labelHidden <|  "Hidden"
                , checked = 
                    if group.openness == Themes.Open then
                        True
                    else
                        False
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ column
                []
                [ makeLabel "Layers"
                , appButton 
                    "Add Layer" 
                    (
                        let
                            newKey = Themes.stringToLayerKey "lyr_change_me"

                            newLayer = 
                                { key = newKey
                                , name = ""
                                , opacity = Nothing
                                , config = Themes.UnknownLayer
                                , legend = Nothing
                                , identify = Nothing
                                }
                            
                            newGroup =
                                { group 
                                | layers = group.layers ++ [ newKey ]
                                }

                            newThm = 
                                Themes.updateGroup
                                    ( Themes.insertLayer newLayer themes)
                                    newGroup
                        in
                        UpdateThemes newThm

                    )
                    True
                ]
            , column
                [ width fill
                , spacing 5
                ]
                <| List.map 
                    ( renderLayer themes category index group
                    )
                    group.layers
            ]
        ]

renderLayer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.LayerGroup -> Themes.LayerKey -> Element Msg
renderLayer themes category index group layer  =
    let
        lyr = Themes.getLayerByKey themes.layerRepo layer
    in
    column
        [ spacing 5
        , Border.width 1
        , width fill
        , padding 5
        ]
        [ text <| Themes.layerKeyToString layer
        , case lyr of
            Just g ->
                layerRenderer themes category index group g
            Nothing ->
                text "No Layer Found!"
        ]

layerRenderer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.LayerGroup -> Themes.Layer -> Element Msg
layerRenderer themes category index group layer =
    let 
        updoot lyr =
            updootLay lyr
            |> UpdateThemes

        updootLay lyr =
            Themes.updateLayer themes lyr
        
        before = List.take index themes.layerCategories
        after = List.drop (index + 1) themes.layerCategories

        updootCat newCat =
            { themes
            | layerCategories = before ++ [ newCat ] ++ after
            }
            |> UpdateThemes

        
        isSelected =
            case category.selection of
                Themes.Monoselection (Just key) ->
                    key == layer.key
                Themes.Polyselection lis ->
                    List.member layer.key lis
                _ ->
                    False
        updootGroop newGrp t =
            Themes.updateGroup t newGrp
    in
    column
        [ width fill
        , spacing 3
        ]
        [ row 
            [ spacing 10 
            ]
            [ makeLabel "Key" 
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            newCat = 
                                { layer 
                                | key = 
                                    Themes.stringToLayerKey text
                                }

                            newGroup =
                                { group
                                | layers = 
                                    group.layers
                                    |> List.map
                                        (\l ->
                                            if l == layer.key then
                                                Themes.stringToLayerKey text
                                            else
                                                l
                                        )
                                }

                            newThm thm =
                                if isSelected then
                                    Themes.toggleLayerSelection layer.key category thm
                                    |> Themes.toggleLayerSelection (Themes.stringToLayerKey text) category
                                else
                                    thm

                        in
                        updootLay newCat
                        |> updootGroop newGroup
                        |> newThm
                        |> UpdateThemes
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Key"
                , text = 
                    layer.key |> Themes.layerKeyToString
                }
            ]
        , row 
            [ spacing 10 
            ]
            [ makeLabel "Name" 
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            newCat = 
                                { layer 
                                | name = text
                                }
                        in
                        updoot newCat
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Name"
                , text = 
                    layer.name
                }
            ]
        , row
            [ spacing 10 
            ]
            [ makeLabel "Selected"
            , Input.checkbox
                []
                { onChange = 
                    (\val ->
                        Themes.toggleLayerSelection layer.key category themes
                        |> UpdateThemes
                    )
                , icon = Input.defaultCheckbox
                , label = Input.labelHidden <|  "Hidden"
                , checked = 
                    isSelected
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Opacity"
            , row
                [ width fill
                , spacing 5
                ]
                [ makeLabel 
                    <| ( String.fromInt ( floor ( (layer.opacity |> Maybe.withDefault 1) * 100 ) ) ) ++ "%"
                , Input.slider
                    [ width fill
                    , Bg.color <| rgb 0.9 0.9 0.9
                    ]
                    { onChange = 
                        (\val ->
                            let
                                newCat = 
                                    { layer | opacity = Just val }
                            in
                            updoot newCat
                        )
                    , label = Input.labelHidden <|  "Multiphasic"
                    , min = 0
                    , max = 1
                    , step = Just 0.01
                    , value = layer.opacity |> Maybe.withDefault 1
                    , thumb = Input.defaultThumb
                    }
                ]
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Config"
            , layerConfigRenderer themes category index layer
            ]
        ]

emptyExtent = [0,0,0,0]

layerConfigRenderer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.Layer -> Element Msg
layerConfigRenderer themes category index layer =
    let
        textualValue =
            case layer.config of
                Themes.XYZLayer _ -> "XYZ"
                Themes.WMTSLayer _ -> "WMTS"
                Themes.WMSLayer _ -> "WMS"
                Themes.EsriExportLayer _ -> "ESRI MapService"
                Themes.MapboxVectorTile _ -> "MVT"
                Themes.UnknownLayer -> "Unknown"
    in
    column
        [ width fill
        , spacing 10
        , Border.width 1
        , padding 5
        ]
        [ text 
            <| textualValue
        , Input.radio
            [ padding 10
            , spacing 20
            ]
            { onChange = 
                (\newOpt ->
                    let
                        newConf =
                            case newOpt of
                                "XYZ" -> 
                                    Themes.XYZLayer
                                        { endpoints  = []
                                        , maxZoom = 22
                                        , minZoom = 3
                                        }
                                "WMTS" -> 
                                    Themes.WMTSLayer
                                        { endpoints = []
                                        , extent = emptyExtent
                                        }
                                "WMS" ->
                                    Themes.WMSLayer
                                        { endpoints = []
                                        , extent = emptyExtent
                                        }
                                "ESRI MapService" -> 
                                    Themes.EsriExportLayer
                                    { endpoints = []
                                    , extent = emptyExtent 
                                    , layerDefs = Nothing
                                    }
                                "MVT" -> 
                                    Themes.MapboxVectorTile
                                        { endpoints = []
                                        }
                                _ ->
                                    Themes.UnknownLayer
                    in
                        { layer 
                        | config = newConf
                        }
                        |> Themes.updateLayer themes
                        |> UpdateThemes
                )
            , selected = Just textualValue
            , label = Input.labelHidden "Type"
            , options =
                [ Input.option "XYZ" (text "XYZ")
                , Input.option "WMTS" (text "WMTS")
                , Input.option "WMS" (text "WMS")
                , Input.option "ESRI MapService" (text "ESRI MapService")
                , Input.option "MVT" (text "MVT")
                , Input.option "Unknown" (text "Unknown")
                ]
            }
        , case layer.config of
            Themes.XYZLayer _ -> text <| "XYZ"
            Themes.WMTSLayer _ -> text <| "WMTS"
            Themes.WMSLayer _ -> text <| "WMS"
            Themes.EsriExportLayer esri -> renderESRIMapService themes layer esri
            Themes.MapboxVectorTile mvt -> renderMVTConfig themes layer mvt
            Themes.UnknownLayer -> text <| "Unknown type or error parsing"
        ]

renderESRIMapService : Themes.Model -> Themes.Layer -> Themes.EsriExportConfig -> Element Msg
renderESRIMapService themes layer config =
    let
        
        updootLay cfg =
            { layer
            | config = Themes.EsriExportLayer cfg
            }
        
        updootThm lay =
            Themes.updateLayer themes lay
            |> UpdateThemes

    in
    column 
        [ width fill
        , spacing 5
        , padding 5
        ]
        [ row 
            [ spacing 10 
            ]
            [ makeLabel "Layer Definitions" 
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let
                            newCat = 
                                { config 
                                | layerDefs = 
                                    if String.length text > 0 then
                                        Just text
                                    else
                                        Nothing
                                }
                        in
                        newCat |> updootLay |> updootThm
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Layer Definitions"
                , text = 
                    config.layerDefs |> Maybe.withDefault ""
                }
            ]
        , row 
            [ spacing 10 
            ]
            [ makeLabel "Extent" 
            , row
                [ width fill
                ]
                <| List.indexedMap
                    (\idx -> \itm ->
                        let 
                            before = List.take idx config.extent
                            after = List.drop (idx + 1) config.extent
                            upd v =
                                { config 
                                | extent = before ++ [ v ] ++ after
                                }
                        in
                        Input.text
                            []
                            { onChange = 
                                (\text ->
                                    text |> String.toFloat |> Maybe.map upd |> Maybe.withDefault config |> updootLay |> updootThm
                                )
                            , placeholder = Nothing
                            , label = Input.labelHidden <|  "Extent"
                            , text = 
                                itm |> String.fromFloat
                            }
                    )
                    config.extent
            ]
        , row 
            [ spacing 10 
            , width fill
            ]
            [ column
                []
                [ makeLabel "Endpoints" 
                , appButton 
                    "Add Endpoint" 
                    (
                        { config
                        | endpoints = 
                            config.endpoints ++ 
                            [   { url = ""
                                , zIndex = 0
                                , tokenKey = Nothing
                                , layersToShow = Nothing
                                , layerDefs = Nothing
                                , bbox = Nothing
                                }
                            ] 
                        } |> updootLay |> updootThm
                    )
                    True
                ]
            , column 
                [ width fill
                , spacing 5
                ]
                <| List.intersperse spacer
                <| List.indexedMap (renderEsriMapServiceEndpoint themes layer config) config.endpoints
            ]
        ]

renderEsriMapServiceEndpoint : Themes.Model -> Themes.Layer -> Themes.EsriExportConfig -> Int -> Themes.Endpoint -> Element Msg
renderEsriMapServiceEndpoint themes layer config index endpoint =
    let
        epB4 = List.take index config.endpoints
        epA = List.drop (index + 1) config.endpoints

        updootEP ep =
            { config
            | endpoints = epB4 ++ [ ep ] ++ epA 
            }
        
        updootLay cfg =
            { layer
            | config = Themes.EsriExportLayer cfg
            }
        
        updootThm lay =
            Themes.updateLayer themes lay
            |> UpdateThemes

        updoot x =
            x |> updootEP |> updootLay |> updootThm
    in
    column
        [ width fill
        , spacing 3
        ]
        [ row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Token Key"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint 
                        | tokenKey = 
                            if String.length text > 0 then
                                Just text
                            else
                                Nothing
                        } 
                        |> updoot
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Token Key"
                , text = 
                    endpoint.tokenKey |> Maybe.withDefault ""
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "URL"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint | url = text } |> updoot
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "URL"
                , text = 
                    endpoint.url
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Z-Index"
            , row
                [ width fill
                , spacing 5
                ]
                [ endpoint.zIndex |> String.fromInt |> makeLabel
                , Input.slider
                    [ width fill
                    , Bg.color <| rgb 0.9 0.9 0.9
                    ]
                    { onChange = 
                        (\val ->
                            { endpoint 
                            | zIndex = floor val
                            }
                            |> updoot
                        )
                    , label = Input.labelHidden <|  "Multiphasic"
                    , min = 0
                    , max = 1000
                    , step = Just 1
                    , value = endpoint.zIndex |> toFloat
                    , thumb = Input.defaultThumb
                    }
                ]
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Layers To Show"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint 
                        | layersToShow = 
                            if String.length text > 0 then
                                Just text
                            else
                                Nothing
                        } |> updootEP |> updootLay |> updootThm
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Layers To Show"
                , text = 
                    endpoint.layersToShow |> Maybe.withDefault ""
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Layer Definitions"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint 
                        | layerDefs = 
                            if String.length text > 0 then
                                Just text
                            else
                                Nothing
                        } |> updootEP |> updootLay |> updootThm
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Layers To Show"
                , text = 
                    endpoint.layerDefs |> Maybe.withDefault ""
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Bounding Box"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint 
                        | bbox = 
                            if String.length text > 0 then
                                Just text
                            else
                                Nothing
                        } |> updootEP |> updootLay |> updootThm
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Layers To Show"
                , text = 
                    endpoint.bbox |> Maybe.withDefault ""
                }
            ]
        ]


renderMVTConfig : Themes.Model -> Themes.Layer -> Themes.MVTConfig -> Element Msg
renderMVTConfig themes layer config =
    let
        updootLay cfg =
            { layer
            | config = Themes.MapboxVectorTile cfg
            }
        
        updootThm lay =
            Themes.updateLayer themes lay
            |> UpdateThemes
    in
     row 
            [ spacing 10 
            , width fill
            ]
            [ column
                []
                [ makeLabel "Endpoints" 
                , appButton 
                    "Add Endpoint" 
                    (
                        { config
                        | endpoints = 
                            config.endpoints ++ 
                            [   { url = ""
                                , zIndex = Nothing
                                , tokenKey = Nothing
                                , style = 
                                    Themes.StaticStyle
                                    { strokeColor = Nothing
                                    , strokeWidth = Nothing
                                    , fillColor = Nothing
                                    }
                                , filter = Nothing
                                }
                            ] 
                        } |> updootLay |> updootThm
                    )
                    True
                ]
            
            , column 
                [ width fill
                , spacing 5
                ]
                <| List.intersperse spacer
                <| List.indexedMap (renderMVTEndpoint themes layer config) config.endpoints
            ]


renderMVTEndpoint : Themes.Model -> Themes.Layer -> Themes.MVTConfig -> Int -> Themes.MVTEndpoint -> Element Msg
renderMVTEndpoint themes layer config index endpoint =
    let
        epB4 = List.take index config.endpoints
        epA = List.drop (index + 1) config.endpoints

        updootEP ep =
            { config
            | endpoints = epB4 ++ [ ep ] ++ epA 
            }
        
        updootLay cfg =
            { layer
            | config = Themes.MapboxVectorTile cfg
            }
        
        updootThm lay =
            Themes.updateLayer themes lay
            |> UpdateThemes

        updoot x =
            x |> updootEP |> updootLay |> updootThm
    in
    column
        [ width fill
        , spacing 3
        ]
        [ row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Token Key"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint 
                        | tokenKey = 
                            if String.length text > 0 then
                                Just text
                            else
                                Nothing
                        } 
                        |> updoot
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Token Key"
                , text = 
                    endpoint.tokenKey |> Maybe.withDefault ""
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "URL"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint | url = text } |> updoot
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "URL"
                , text = 
                    endpoint.url
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Z-Index"
            , row
                [ width fill
                , spacing 5
                ]
                [ endpoint.zIndex |> Maybe.map String.fromInt |> Maybe.withDefault "Unset" |> makeLabel
                , Input.slider
                    [ width fill
                    , Bg.color <| rgb 0.9 0.9 0.9
                    ]
                    { onChange = 
                        (\val ->
                            { endpoint 
                            | zIndex = Just <| floor val
                            }
                            |> updoot
                        )
                    , label = Input.labelHidden <|  "Multiphasic"
                    , min = 0
                    , max = 1000
                    , step = Just 1
                    , value = endpoint.zIndex |> Maybe.withDefault 0 |> toFloat
                    , thumb = Input.defaultThumb
                    }
                ]
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Style"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint | url = text } |> updootEP |> updootLay |> updootThm
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "URL"
                , text = 
                    endpoint.url
                }
            ]
        , row
            [ spacing 10 
            , width fill
            ]
            [ makeLabel "Filter"
            , Input.text
                []
                { onChange = 
                    (\text ->
                        { endpoint | url = text } |> updootEP |> updootLay |> updootThm
                    )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "URL"
                , text = 
                    endpoint.url
                }
            ]
        ]


loadView : LoadMode -> LoadStep -> Element Msg
loadView mode step =
    case mode of
        FromPAM ->
            text "Enter PAM infos"
        FromText ->
            loadFromTextView step


loadFromTextView : LoadStep -> Element Msg
loadFromTextView step =
    case step of
        New txt ->
            column
                [ spacing 10
                , centerX
                ]
                [ text "Enter the config text below and click Load!"
                , Input.multiline
                    [ height <| px 400 
                    ]
                    { onChange = (\t -> Load FromText <| New t)
                    , text = txt
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text "Config text"
                    , spellcheck = False
                    }
                , appButton
                    "Load"
                    (StartParsing txt)
                    (String.length txt > 0)

                ]
        Parse txt ->
            text "Parsamooting..."
        ErrorParsing err ->
            column
                [ spacing 10
                , centerX
                ]
                [ text "Error Parsamooting"
                , text err
                ]
        LoadOk cfg ->
            column
                [ spacing 20
                , centerX
                , padding 10
                ]
                <| List.intersperse spacer
                [ text "Configuration parsed successfully."
                , column 
                    [ spacing 2
                    ]
                    [ text "Config stats"
                    , spacer
                    , text <| (String.fromInt <| List.length <| Dict.values <| cfg.themes.layerRepo) ++ " layers" 
                    , text <| (String.fromInt <| List.length <| Dict.values <| cfg.themes.layerGroupRepo) ++ " groups" 
                    , text <| (String.fromInt <| List.length <| cfg.themes.layerCategories) ++ " categories" 
                    ]
                , column 
                    [ spacing 2
                    ]
                    [ appButton
                        "Hierarchical Editor"
                        (StartEditing Hierarchical cfg)
                        (True)
                    , appButton
                        "Flat Editor"
                        (StartEditing Flat cfg)
                        (True)
                    , appButton
                        "Layer Sequencer"
                        (StartEditing SequenceLayers cfg)
                        (True)
                    ]
                , column 
                    []
                    [ appButton
                        "Start Over"
                        (Init)
                        (True)
                    ]
                ]
            
        _ ->
            text "Not implementooted"

menuView : Element Msg
menuView =
    column
        [ spacing 5 
        , centerX
        ]
    <| List.map
        (\itm ->
            appButton
                ( case itm of
                    Back ->
                        "Back"
                    ConfigFromText ->
                        "Load From Text"
                    ConfigFromPAM ->
                        "Load From PAM"
                )
                ( Menu itm )
                True
        )
    <| menuOptions


appButton : String -> msg -> Bool -> Element msg
appButton label action enabled =
    Input.button
        [ width <| px 300
        , height <| px 50
        , Border.rounded 1
        , Border.width 1
        , centerX
        ]
        { label =
            text label
        , onPress =
            if enabled then
                Just action
            else
                Nothing
        }



---- PROGRAM ----


main : Program D.Value Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

subscriptions : Model -> Sub Msg
subscriptions app =
    Sub.none