module Main exposing (..)

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


import Json.Decode as D exposing (Decoder, Value, dict, errorToString, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E exposing (Value)
import Json.Encode.Extra as EEx

import Url exposing (Protocol(..), Url)

import Process
import Task



import PlugMap.Core.Options as MapOptions
import PlugMap.Plugins.Themes as Themes


---- MODEL ----


type alias Model =
    { state : State
    , key : Nav.Key
    }

type State 
    = Loading
    | MainMenu
    | LoadConfig LoadMode LoadStep
    | Editing Config
    | Error


type LoadMode
    = FromText
    | FromPAM

type LoadStep
    = New String
    | Parse String
    | ErrorParsing String
    | Ok Config
    | BeepBoop


type alias Config = 
    { options : MapOptions.Options
    , themes : Themes.Model
    }



init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init params url key =
    ( { state = MainMenu, key = key }
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
    | StartEditing Config
    | UpdateConfig Config



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
                mop : Maybe MapOptions.Options
                mop =
                    D.decodeString (MapOptions.optionsFromConfigDecoder) text |> Result.toMaybe

                mt : Maybe Themes.Model
                mt =
                    D.decodeString (Themes.themesDecoder) text |> Result.toMaybe
       
            in
            case ( mop, mt ) of
                (Just options, Just t ) ->
                    let
                        no = 
                            Config
                                options
                                t
                        ns = LoadConfig FromText <| Ok no
                    in
                    ( { model
                        | state = ns
                        }
                    , Cmd.none
                    )
                (_,_) ->
                    ( model, Cmd.none )
        Load mode step ->
            ( { model | state = LoadConfig mode step }, Cmd.none )

        StartEditing cfg ->
            ( { model | state = Editing cfg }
            , Cmd.none
            )

        UpdateConfig nCfg ->
            ( { model | state = Editing nCfg }
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
    { title = "Problem loading"
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
            configEditorView cfg
        Error ->
            el [] <|
                paragraph []
                    [ el [ Font.bold ] (text "Oh no!")
                    , text "We've encountered a problem loading the Workflow."
                    ]



configEditorView : Config -> Element Msg
configEditorView cfg =
    column
        [ centerX
        , spacing 25
        ]
        [ text "Config Editor"
        , spacer
        , mapOptionsView cfg.options
            |> El.map 
                (\v -> 
                    UpdateConfig { cfg | options = v } 
                )
        , spacer
        , mapThemesView cfg.themes
            |> El.map 
                (\v -> 
                    UpdateConfig { cfg | themes = v } 
                )
        ]

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
        ]
        [ text "Map Options"
        , row 
            [ spacing 15
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

mapThemesView : Themes.Model -> Element Themes.Model
mapThemesView themes =
    column
        [ spacing 15
        ]
        [ text "Themes"
        , column
            [ spacing 15
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

mapThemesCategoryView : Themes.Model -> Int -> Themes.LayerCategory -> Element Themes.Model
mapThemesCategoryView themes index category =
    let
        before = List.take index themes.layerCategories
        after = List.drop (index + 1) themes.layerCategories
        updoot newCat =
            { themes
            | layerCategories = before ++ [ newCat ] ++ after
            }
    in
    column
        [ spacing 5 
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
--TODO

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

renderGroup : Themes.Model -> Themes.LayerCategory -> Int -> Themes.GroupKey -> Element Themes.Model
renderGroup themes category index group =
    let
        grp = Themes.getGroupByKey themes.layerGroupRepo group
    in
    column
        [ spacing 5
        , Border.width 1
        , width fill
        ]
        [ text <| Themes.groupKeyToString group
        , case grp of
            Just g ->
                groupRenderer themes category index g
            Nothing ->
                text "No Group Found!"
        ]





groupRenderer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.LayerGroup -> Element Themes.Model
groupRenderer themes category index group =
    let 
        updoot grp =
            Themes.updateGroup themes grp
    in
    column
        []
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
            [ makeLabel "Layers"
            , column
                [ width fill
                ]
                <| List.map 
                    ( renderLayer themes category index
                    )
                    group.layers
            ]
        ]

renderLayer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.LayerKey -> Element Themes.Model
renderLayer themes category index layer =
    let
        lyr = Themes.getLayerByKey themes.layerRepo layer
    in
    column
        [ spacing 5
        , Border.width 1
        , width fill
        ]
        [ text <| Themes.layerKeyToString layer
        , case lyr of
            Just g ->
                layerRenderer themes category index g
            Nothing ->
                text "No Layer Found!"
        ]

layerRenderer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.Layer -> Element Themes.Model
layerRenderer themes category index layer =
    let 
        updoot lyr =
            Themes.updateLayer themes lyr

        
        before = List.take index themes.layerCategories
        after = List.drop (index + 1) themes.layerCategories

        updootCat newCat =
            { themes
            | layerCategories = before ++ [ newCat ] ++ after
            }

        
        isSelected =
            case category.selection of
                Themes.Monoselection (Just key) ->
                    key == layer.key
                Themes.Polyselection lis ->
                    List.member layer.key lis
                _ ->
                    False
    in
    column
        []
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
                        in
                        updoot newCat
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


layerConfigRenderer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.Layer -> Element Themes.Model
layerConfigRenderer themes category index { config } =
    column
        []
        [ text 
            <| case config of
                Themes.XYZLayer _ -> "XYZ"
                Themes.WMTSLayer _ -> "WMTS"
                Themes.WMSLayer _ -> "WMS"
                Themes.EsriExportLayer _ -> "ESRI MapService"
                Themes.MapboxVectorTile _ -> "MVT"
                Themes.UnknownLayer -> "Unknown type or error parsing"
        , case config of
            Themes.XYZLayer _ -> text <| "XYZ"
            Themes.WMTSLayer _ -> text <| "WMTS"
            Themes.WMSLayer _ -> text <| "WMS"
            Themes.EsriExportLayer _ -> text <| "ESRI MapService"
            Themes.MapboxVectorTile _ -> text <| "MVT"
            Themes.UnknownLayer -> text <| "Unknown type or error parsing"
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
        Ok cfg ->
            column
                [ spacing 10
                , centerX
                ]
                [ text "We did it"
                , appButton
                    "Next"
                    (StartEditing cfg)
                    (True)
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


appButton : String -> Msg -> Bool -> Element Msg
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