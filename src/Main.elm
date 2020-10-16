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
    , hiddenCats : List String
    , hiddenGrps : List String
    , hiddenLays : List String
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
    (   { state = MainMenu
        , key = key
        , editorMode = Hierarchical 
        , hiddenCats = []
        , hiddenGrps = []
        , hiddenLays = []
        }
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

    | HideCat String
    | ShowCat String

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

        HideCat key ->
            ( { model 
                | hiddenCats = key :: model.hiddenCats
                }
            , Cmd.none
            )

        ShowCat key ->
            ( { model 
                | hiddenCats = List.filter (\i -> i /= key ) model.hiddenCats
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
            , Font.size 15
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
            configEditorView model model.editorMode cfg
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


configEditorView : Model -> EditingMode -> Config -> Element Msg
configEditorView model mode cfg =
    column
        [ centerX
        , spacing 5
        , width fill
        , height fill
        , padding 5
        ]
        [ editHeader mode cfg
        , spacer
        , column
            [ centerX
            , spacing 5
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
                    hierarchicalMapThemesView model cfg.themes
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
        [ spacing 5
        , width fill
        , padding 5
        ]
        [ text "Map Options"
        , row 
            [ spacing 5
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
            [ spacing 5
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
            [ spacing 5
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
            [spacing 5
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
            [spacing 5
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

hierarchicalMapThemesView : Model -> Themes.Model -> Element Msg
hierarchicalMapThemesView model themes =
    column
        [spacing 5
        , width fill
        , Border.width 1
        , padding 5
        ]
        [ text "Themes"
        , column
            [spacing 5
            , width fill
            ]
                <| List.indexedMap
                    ( \idxx -> \element ->
                        column
                        [ width fill
                        , spacing 10
                        ]
                        [ spacer
                        , row 
                            [ width fill
                            , spacing 5
                            ] 
                            [ el [ alignLeft ] 
                                <| simpleAppButton "Delete"
                                    (
                                        let
                                            be4 =
                                                List.take idxx themes.layerCategories
                                            aft =
                                                List.drop (idxx+1) themes.layerCategories
                                            
                                        in
                                        { themes
                                        | layerCategories =  
                                                be4 ++ aft
                                        }
                                        |> UpdateThemes
                                            
                                    )
                            , el [ alignLeft ] 
                                <| simpleAppButton "Up"
                                    (
                                        let
                                            be4 =
                                                List.take (idxx-1) themes.layerCategories
                                            thiscat =
                                                List.drop (idxx-1) themes.layerCategories
                                                |> List.take 2
                                                |> List.reverse

                                            aft =
                                                List.drop (idxx+1) themes.layerCategories
                                            
                                        in
                                        { themes
                                        | layerCategories =  
                                                be4 ++ thiscat ++ aft
                                        }
                                        |> UpdateThemes
                                            
                                    )
                            , el [ alignLeft ] 
                                <| simpleAppButton "Dwn"
                                    (
                                        let
                                            be4 =
                                                List.take (idxx-1) themes.layerCategories
                                            thiscat =
                                                List.drop (idxx) themes.layerCategories
                                                |> List.take 2
                                                |> List.reverse
                                            aft =
                                                List.drop (idxx+2) themes.layerCategories
                                            
                                        in
                                        { themes
                                        | layerCategories =  
                                                be4 ++ thiscat ++ aft
                                        }
                                        |> UpdateThemes
                                            
                                    )
                            , appButton
                                "Add Category Before"
                                (
                                    let
                                        be4 =
                                            List.take idxx themes.layerCategories
                                        aft =
                                            List.drop (idxx) themes.layerCategories
                                        newc =
                                            { key = Themes.stringToCategoryKey "cat_changeme"
                                            , name = ""
                                            , selection = Themes.Monoselection Nothing
                                            , multiphasic = True
                                            , transparency = 1
                                            , openness = Themes.Closed
                                            , usesRasterLegend = False
                                            , activeIcon = Themes.Icon ""
                                            , infoIcon = Themes.Icon ""    
                                            , layerGroups = []
                                            , hidden = False
                                            }
                                    in
                                    { themes
                                    | layerCategories =  
                                            be4 ++ [ newc ] ++ aft
                                    }
                                    |> UpdateThemes
                                        
                                )
                                True
                            ]
                        , element
                        , appButton
                            "Add Category After"
                            (
                                let
                                    be4 =
                                        List.take (idxx+1) themes.layerCategories
                                    aft =
                                        List.drop (idxx+1) themes.layerCategories
                                    newc =
                                        { key = Themes.stringToCategoryKey "cat_changeme"
                                        , name = ""
                                        , selection = Themes.Monoselection Nothing
                                        , multiphasic = True
                                        , transparency = 1
                                        , openness = Themes.Closed
                                        , usesRasterLegend = False
                                        , activeIcon = Themes.Icon ""
                                        , infoIcon = Themes.Icon ""    
                                        , layerGroups = []
                                        , hidden = False
                                        }
                                in
                                { themes
                                | layerCategories =  
                                        be4 ++ [ newc ] ++ aft
                                }
                                |> UpdateThemes
                                    
                            )
                            True
                        , spacer
                        ]
                    )
            <| List.indexedMap (mapThemesCategoryView model themes) themes.layerCategories 
        , el [ centerX] 
            <| appButton 
                "Add Category After" 
                ( { themes
                    | layerCategories = themes.layerCategories ++ 
                        [   { key = Themes.stringToCategoryKey "cat_changeme"
                            , name = ""
                            , selection = Themes.Monoselection Nothing
                            , multiphasic = True
                            , transparency = 1
                            , openness = Themes.Closed
                            , usesRasterLegend = False
                            , activeIcon = Themes.Icon ""
                            , infoIcon = Themes.Icon ""    
                            , layerGroups = []
                            , hidden = False
                            }
                        ]
                    }
                    |> UpdateThemes
                )
                True
        ]

makeLabel : String -> Element msg
makeLabel val =
    el 
        [ width <| px 150
        ]
        <| text val

mapThemesCategoryView : Model -> Themes.Model -> Int -> Themes.LayerCategory -> Element Msg
mapThemesCategoryView model themes index category =
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
        <| if List.member (Themes.categoryKeyToString category.key) model.hiddenCats then
            [ row
                [ width fill
                ]
                [ text <| Themes.categoryKeyToString category.key
                , simpleAppButton
                    "Show"
                    (ShowCat <| Themes.categoryKeyToString category.key)
                ]
            ]
        else
            [ row
                [spacing 6 
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
                , simpleAppButton
                    "Hide"
                    (HideCat <| Themes.categoryKeyToString category.key)
                ]
            , row 
                [spacing 6 
                , width fill
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
                [spacing 6 
                , width fill
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
                [spacing 6 
                , width fill
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
                [spacing 6 
                , width fill
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
                [spacing 6 
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
                [spacing 6 
                , width fill
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
                [spacing 6 
                , width fill
                ]
                [ column 
                    []
                    [ makeLabel "Groups"
                    , appButton "Add Group" 
                        ( 
                            let
                                newKey = Themes.stringToGroupKey "grp_changeme"
                                newCat =
                                    { category
                                    | layerGroups = category.layerGroups ++
                                        [ newKey
                                        ]
                                    }
                                newGrp = 
                                    { key = newKey
                                    , name = Nothing
                                    , openness = Themes.Closed
                                    , layers = []
                                    }
                                newt = 
                                    { themes
                                    | layerCategories = before ++ [ newCat ] ++ after
                                    }
                
                            in
                            Themes.updateGroup newt newGrp
                            |> UpdateThemes

                        )
                        True
                    ]
                , column
                    [ width fill
                    ]
                    <| List.indexedMap
                        ( \idxx -> \element ->
                            column
                            [ width fill
                            , spacing 10
                            ]
                            [ spacer
                            , appButton
                                "Add Group Before"
                                (
                                    let
                                        be4 =
                                            List.take idxx category.layerGroups
                                        aft =
                                            List.drop (idxx) category.layerGroups
                                        newKey = Themes.stringToGroupKey <| "grp_changeme_" ++ ( String.fromInt <| List.length <| Dict.values themes.layerGroupRepo)
                                        newCat =
                                            { category
                                            | layerGroups = be4 ++
                                                [ newKey
                                                ] ++ aft
                                            }
                                        newGrp = 
                                            { key = newKey
                                            , name = Nothing
                                            , openness = Themes.Closed
                                            , layers = []
                                            }
                                        

                                        newThm = 
                                            Themes.updateGroup
                                                themes
                                                newGrp
                                        newt = 
                                            { newThm
                                            | layerCategories = before ++ [ newCat ] ++ after
                                            }
                                    in
                                    UpdateThemes newt
                                        
                                )
                                True
                            , element
                            , appButton
                                "Add Group After"
                                (
                                    let
                                        be4 =
                                            List.take (idxx+1) category.layerGroups
                                        aft =
                                            List.drop (idxx+1) category.layerGroups
                                        newKey = Themes.stringToGroupKey <| "grp_changeme_" ++ ( String.fromInt <| List.length <| Dict.values themes.layerGroupRepo)
                                        newCat =
                                            { category
                                            | layerGroups = be4 ++
                                                [ newKey
                                                ] ++ aft
                                            }
                                        newGrp = 
                                            { key = newKey
                                            , name = Nothing
                                            , openness = Themes.Closed
                                            , layers = []
                                            }
                                        

                                        newThm = 
                                            Themes.updateGroup
                                                themes
                                                newGrp
                                        newt = 
                                            { newThm
                                            | layerCategories = before ++ [ newCat ] ++ after
                                            }
                                    in
                                    UpdateThemes newt
                                        
                                )
                                True
                            , spacer
                            ]
                        )
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
            [spacing 6
            , width fill 
            ]
            [ makeLabel "Key" 
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let

                            newCat =
                                { category
                                | layerGroups = 
                                    category.layerGroups
                                    |> List.map
                                        (\l ->
                                            if l == group.key then
                                                Themes.stringToGroupKey text
                                            else
                                                l
                                        )
                                }
                            newGroup =
                                { group 
                                | key = Themes.stringToGroupKey text 
                                }
                                
                            updootGroop newGrp thms =
                                Themes.updateGroup thms newGrp

                            catb4 =
                                List.take index themes.layerCategories
                            catA =
                                List.drop (index + 1) themes.layerCategories

                            updootCat newc t =
                                { t 
                                | layerCategories = catb4 ++ [ newc ] ++ catA
                                }

                        in
                        Themes.removeGroup themes group.key
                        |> updootGroop newGroup
                        |> updootCat newCat
                        |> UpdateThemes
                    )

                    -- (\text ->
                    --     let
                    --         newCat = 
                    --             { group 
                    --             | key = 
                    --                 Themes.stringToGroupKey text
                    --             }
                    --     in
                    --     updoot newCat
                    -- )
                , placeholder = Nothing
                , label = Input.labelHidden <|  "Key"
                , text = 
                    group.key |> Themes.groupKeyToString
                }
            ]
        , row 
            [spacing 6 
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
            [spacing 6 
            , width fill
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
            [spacing 6 
            , width fill
            ]
            [ column
                []
                [ makeLabel "Layers"
                , appButton 
                    "Add Layer" 
                    (
                        let
                            newKey = Themes.stringToLayerKey <| "lyr_change_me_" ++ String.fromInt( List.length <| Dict.values <| themes.layerRepo )

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
                , spacing 25
                ]
                <| List.indexedMap
                    ( \idxx -> \element ->
                        column
                        [ width fill
                        , spacing 10
                        ]
                        [ spacer
                        , appButton
                            "Add Layer Before"
                            (
                                let
                                    be4 =
                                        List.take idxx group.layers
                                    aft =
                                        List.drop (idxx) group.layers
                                    newKey = Themes.stringToLayerKey <| "lyr_change_me_" ++ String.fromInt( List.length <| Dict.values <| themes.layerRepo )
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
                                        | layers = be4 ++ [ newKey ] ++ aft
                                        }

                                    newThm = 
                                        Themes.updateGroup
                                            ( Themes.insertLayer newLayer themes)
                                            newGroup
                                in
                                UpdateThemes
                                    newThm
                            )
                            True
                        , element
                        , appButton
                            "Add Layer After"
                            (
                                let
                                    be4 =
                                        List.take (idxx+1) group.layers
                                    aft =
                                        List.drop (idxx+1) group.layers
                                    newKey = Themes.stringToLayerKey <| "lyr_change_me_" ++ String.fromInt( List.length <| Dict.values <| themes.layerRepo )
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
                                        | layers = be4 ++ [ newKey ] ++ aft
                                        }

                                    newThm = 
                                        Themes.updateGroup
                                            ( Themes.insertLayer newLayer themes)
                                            newGroup
                                in
                                UpdateThemes
                                    newThm
                            )
                            True
                        , spacer
                        ]
                    )
                <| List.indexedMap 
                    ( renderLayer themes category index group
                    )
                    group.layers
            ]
        ]

renderLayer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.LayerGroup -> Int -> Themes.LayerKey -> Element Msg
renderLayer themes category index group layerIndex layer  =
    let
        lyr = Themes.getLayerByKey themes.layerRepo layer

    in
    column
        [ spacing 5
        , Border.width 1
        , width fill
        , padding 5
        ]
        [ row
            []
            [ text <| Themes.layerKeyToString layer
            , simpleAppButton
                "Remove"
                (
                    let
                        beforeLayers = List.take layerIndex group.layers
                        afterLayers = List.drop (layerIndex + 1) group.layers
                    in
                    Themes.updateGroup themes
                        { group
                        | layers = beforeLayers ++ afterLayers
                        }
                    |> UpdateThemes
                )
            ]
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
                Themes.Monoselection (key) ->
                    key == Just layer.key
                Themes.Polyselection lis ->
                    List.member layer.key lis
                Themes.EnforcedPolyselection a lis ->
                    a == layer.key || List.member layer.key lis 
                Themes.EnforcedMonoselection key ->
                    layer.key == key
        updootGroop newGrp t =
            Themes.updateGroup t newGrp
    in
    column
        [ width fill
        , spacing 3
        ]
        [ row 
            [ spacing 6 
            , width fill
            ]
            [ makeLabel "Key" 
            , Input.text
                []
                { onChange = 
                    (\text ->
                        let

                            newKey = Themes.stringToLayerKey text 
                            newCat = 
                                { layer 
                                | key = 
                                    newKey
                                }

                            newGroup =
                                { group
                                | layers = 
                                    group.layers
                                    |> List.map
                                        (\l ->
                                            if l == layer.key then
                                                newKey
                                            else
                                                l
                                        )
                                }

                            newThm thm =
                                if isSelected then
                                    let
                                        nc = Themes.toggleLayerSelectionOff layer.key category thm
                                        nnc = Themes.toggleLayerSelectionOn (newKey) nc thm

                                        newcats = Themes.replaceCategory nnc thm.layerCategories

                                    in
                                    { thm | layerCategories = newcats }
                                    
                                else
                                    thm

                            removeLay l t =
                                Themes.removeLayer t l

                        in
                        updootLay newCat
                        |> removeLay layer.key
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
            [spacing 6
            , width fill 
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
            [spacing 6 
            , width fill
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
            [spacing 6 
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
            [ spacing 6 
            , width fill
            ]
            [ makeLabel "Config"
            , layerConfigRenderer themes category index layer
            ]
        , row
            [ spacing 6 
            , width fill
            ]
            [ makeLabel "Identify"
            , identifyRenderer themes category index layer layer.identify
            ]
        , row
            [ spacing 6 
            , width fill
            ]
            [ makeLabel "Legend"
            , legendRenderer themes category index layer layer.legend
            ]
        ]





legendRenderer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.Layer -> Maybe Themes.Legend -> Element Msg
legendRenderer themes category index layer legend =
    let
        updateLayerIdent newIdent =
            { layer
            | identify = Just newIdent
            }
            |> Themes.updateLayer themes
            |> UpdateThemes
    in
    column
        [ Border.width 1
        , padding 3
        , width fill
        ]
    <| case legend of
        Nothing ->
            [ appButton 
                "Add" 
                (   let
                        newLayer =
                            { layer
                            | legend = Just []
                            }
                    in
                    Themes.updateLayer themes newLayer
                    |> UpdateThemes
                )
                True
            ]
        Just ident ->
            [ text "todo"
            ]






identifyRenderer : Themes.Model -> Themes.LayerCategory -> Int -> Themes.Layer -> Maybe Themes.Identify -> Element Msg
identifyRenderer themes category index layer identify =
    let
        updateLayerIdent newIdent =
            { layer
            | identify = Just newIdent
            }
            |> Themes.updateLayer themes
            |> UpdateThemes
    in
    column
        [ Border.width 1
        , padding 3
        , width fill
        ]
    <| case identify of
        Nothing ->
            [ appButton 
                "Add" 
                (   let
                        newDataMapping =
                            { title =
                                { name = "Name"
                                , fieldType = Themes.Query "Name"
                                }
                            , id =
                                { name = "ID"
                                , fieldType = Themes.Query "ID"
                                }
                            , fields = []
                            }
                        newIdent = 
                            { enabled = True
                            , dataMapping = newDataMapping
                            , canViewDetails = False
                            , idTransform = 0
                            , typeKey = "CHANGEME"
                            , queryLayerIds = Nothing
                            , tolerance = Nothing
                            }
                        newLayer =
                            { layer
                            | identify = Just newIdent
                            }
                    in
                    Themes.updateLayer themes newLayer
                    |> UpdateThemes
                )
                True
            ]
        Just ident ->
            [ row
                [spacing 6 
                ]
                [ makeLabel "Enabled"
                , Input.checkbox
                    []
                    { onChange = 
                        (\val ->
                            let
                                newIdent = 
                                    { ident 
                                    | enabled = val
                                    }
                                newLayer =
                                    { layer
                                    | identify = Just newIdent
                                    }
                            in
                            Themes.updateLayer themes newLayer
                            |> UpdateThemes
                        )
                    , icon = Input.defaultCheckbox
                    , label = Input.labelHidden <|  "Hidden"
                    , checked = 
                        ident.enabled
                    }
                ]
            , row
                [spacing 6 
                ]
                [ makeLabel "View Details"
                , Input.checkbox
                    []
                    { onChange = 
                        (\val ->
                            let
                                newIdent = 
                                    { ident 
                                    | enabled = val
                                    }
                                newLayer =
                                    { layer
                                    | identify = Just newIdent
                                    }
                            in
                            Themes.updateLayer themes newLayer
                            |> UpdateThemes
                        )
                    , icon = Input.defaultCheckbox
                    , label = Input.labelHidden <|  "Hidden"
                    , checked = 
                        ident.enabled
                    }
                ]
            , row 
                [spacing 6 
                , width fill 
                ]
                [ makeLabel "Query Layer IDs" 
                , Input.text
                    []
                    { onChange = 
                        (\text ->
                            let
                                idss =
                                    String.split "," text

                                ids =
                                    List.filterMap String.toInt idss
                                newIdent = 
                                    { ident 
                                    | queryLayerIds = 
                                        if List.length ids > 0 then
                                            Just ids
                                        else
                                            Nothing
                                    }
                                newLayer =
                                    { layer
                                    | identify = Just newIdent
                                    }
                            in
                            Themes.updateLayer themes newLayer
                            |> UpdateThemes
                        )
                    , placeholder = Nothing
                    , label = Input.labelHidden <|  "Query Layer IDs"
                    , text = 
                        ident.queryLayerIds |> Maybe.map (List.map String.fromInt >> String.join ",") |> Maybe.withDefault ""
                    }
                ]
            , row
                [ spacing 6 
                , width fill
                ]
                [ makeLabel "Data Mapping"
                , column
                    [ width fill
                    , Border.width 1
                    , padding 3
                    , spacing 5
                    ]
                    ( ( [ row
                        [spacing 6 
                        , width fill
                        ]
                        [ makeLabel "Title"
                        , identifyFieldView ident.dataMapping.title 
                            |> El.map 
                                ( \iif -> 
                                    let

                                        newIdent = 
                                            { ident 
                                            | dataMapping = newDataMapping
                                            }
                                        odm = ident.dataMapping
                                        newDataMapping =
                                            { odm 
                                            | title = iif
                                            }
                                    in 
                                    updateLayerIdent newIdent
                                )
                        ]
                    , row
                        [spacing 6 
                        , width fill
                        ]
                        [ makeLabel "ID"
                        , identifyFieldView ident.dataMapping.id 
                            |> El.map 
                                ( \iif -> 
                                    let

                                        newIdent = 
                                            { ident 
                                            | dataMapping = newDataMapping
                                            }
                                        odm = ident.dataMapping
                                        newDataMapping =
                                            { odm 
                                            | id = iif
                                            }
                                    in 
                                    updateLayerIdent newIdent
                                )
                        ]
                    , spacer
                    , el [ centerX ] <| text "Additional Fields"
                    ] )
                    ++
                    ( List.indexedMap
                        ( \idx -> \field ->
                            let
                                before = List.take idx ident.dataMapping.fields
                                after = List.drop (idx + 1) ident.dataMapping.fields
                            in
                            identifyFieldView field 
                            |> El.map 
                                ( \iif -> 
                                    let

                                        newIdent = 
                                            { ident 
                                            | dataMapping = newDataMapping
                                            }
                                        odm = ident.dataMapping
                                        newDataMapping =
                                            { odm 
                                            | fields = before ++ [ iif ] ++ after
                                            }
                                    in 
                                    updateLayerIdent newIdent
                                )
                        ) 
                        ident.dataMapping.fields
                    ) 
                    ++
                    ( [ appButton "Add Field"
                        ( let
                                newIdent = 
                                    { ident 
                                    | dataMapping = newDataMapping
                                    }
                                odm = ident.dataMapping
                                newf =
                                    { name = ""
                                    , fieldType = Themes.Query ""
                                    }
                                newDataMapping =
                                    { odm 
                                    | fields = odm.fields ++ [ newf ]
                                    }
                            in 
                            updateLayerIdent newIdent
                        )
                        True
                        ]
                    )
                    )
                ]
            ]



identifyFieldView : Themes.IdentifyField -> Element Themes.IdentifyField
identifyFieldView field =
    row
        [ width fill
        , spacing 10
        ]
        [ row 
                [spacing 6 
                , width fill 
                ]
                [ makeLabel "Name" 
                , Input.text
                    []
                    { onChange = 
                        (\text ->
                           { field 
                           | name = text
                           }
                        )
                    , placeholder = Nothing
                    , label = Input.labelHidden <|  "Name"
                    , text = 
                        field.name
                    }
                ]
        , row 
            [spacing 6 
            , width fill 
            ]
            [ makeLabel "Field Type" 
            , column
                [ width fill
                ]
                [ Input.radioRow
                    [ padding 10
                    , spacing 20
                    ]
                    { onChange = 
                        (\newOpt ->
                            let
                                newConf =
                                    case newOpt of
                                        "Query" -> 
                                            Themes.Query
                                                ""
                                        "Static" -> 
                                            Themes.Static
                                                ""
                                        "Map" ->
                                            Themes.Map
                                                { query = ""
                                                , values = Dict.empty
                                                }
                                        _ ->
                                            Themes.Static
                                                ""
                            in
                                { field 
                                | fieldType = newConf
                                }
                        )
                    , selected = 
                        Just 
                        <| case field.fieldType of
                            Themes.Query _ -> "Query"
                            Themes.Static _ -> "Static"
                            Themes.Map _ -> "Map"

                    , label = Input.labelHidden "Field Type"
                    , options =
                        [ Input.option "Query" (text "Query")
                        , Input.option "Static" (text "Static")
                        , Input.option "Map" (text "Map")
                        ]
                    }
                , case field.fieldType of
                    Themes.Query ql -> 
                        Input.text
                        []
                        { onChange = 
                            (\text ->
                                { field 
                                | fieldType = Themes.Query text
                                }
                            )
                        , placeholder = Nothing
                        , label = Input.labelHidden <|  "Query"
                        , text = 
                            ql
                        }
                    Themes.Static ql -> 
                        Input.text
                        []
                        { onChange = 
                            (\text ->
                                { field 
                                | fieldType = Themes.Static text
                                }
                            )
                        , placeholder = Nothing
                        , label = Input.labelHidden <|  "Static"
                        , text = 
                            ql
                        }
                    Themes.Map _ -> text "TODO"
                ]
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
                Themes.EsriMapServiceLayer _ -> "ESRI MapService"
                Themes.EsriFeatureServiceLayer _ -> "ESRI FeatureService"
                Themes.MapboxVectorTile _ -> "MVT"
                Themes.UnknownLayer -> "Unknown"
    in
    column
        [ width fill
        ,spacing 6
        , Border.width 1
        , padding 5
        ]
        [ text 
            <| textualValue
        , Input.radioRow
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
                                    case layer.config of
                                        Themes.EsriFeatureServiceLayer conf ->
                                            Themes.EsriMapServiceLayer conf
                                        _ ->
                                            Themes.EsriMapServiceLayer
                                            { endpoints = []
                                            , extent = emptyExtent 
                                            , layerDefs = Nothing
                                            }
                                "ESRI FeatureService" -> 
                                    case layer.config of
                                        Themes.EsriMapServiceLayer conf ->
                                            Themes.EsriFeatureServiceLayer conf
                                        _ ->
                                            Themes.EsriFeatureServiceLayer
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
                , Input.option "ESRI FeatureService" (text "ESRI FeatureService")
                , Input.option "MVT" (text "MVT")
                , Input.option "Unknown" (text "Unknown")
                ]
            }
        , case layer.config of
            Themes.XYZLayer _ -> text <| "XYZ"
            Themes.WMTSLayer _ -> text <| "WMTS"
            Themes.WMSLayer _ -> text <| "WMS"
            Themes.EsriMapServiceLayer esri -> renderESRIMapService themes layer esri
            Themes.EsriFeatureServiceLayer esri -> renderESRIFeatureService themes layer esri
            Themes.MapboxVectorTile mvt -> renderMVTConfig themes layer mvt
            Themes.UnknownLayer -> text <| "Unknown type or error parsing"
        ]

renderESRIFeatureService : Themes.Model -> Themes.Layer -> Themes.EsriConfig -> Element Msg
renderESRIFeatureService themes layer config =
    renderESRIConfig themes layer config (Themes.EsriFeatureServiceLayer)

renderESRIMapService : Themes.Model -> Themes.Layer -> Themes.EsriConfig -> Element Msg
renderESRIMapService themes layer config =
    renderESRIConfig themes layer config (Themes.EsriMapServiceLayer)


renderESRIConfig : Themes.Model -> Themes.Layer -> Themes.EsriConfig -> (Themes.EsriConfig -> Themes.LayerConfig) -> Element Msg
renderESRIConfig themes layer config xform =
    let
        
        updootLay cfg =
            { layer
            | config = xform cfg
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
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
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
                <| List.indexedMap (renderEsriMapServiceEndpoint themes layer config xform) config.endpoints
            ]
        ]

renderEsriMapServiceEndpoint : Themes.Model -> Themes.Layer -> Themes.EsriConfig -> (Themes.EsriConfig -> Themes.LayerConfig) -> Int -> Themes.Endpoint -> Element Msg
renderEsriMapServiceEndpoint themes layer config xform index endpoint =
    let
        epB4 = List.take index config.endpoints
        epA = List.drop (index + 1) config.endpoints

        updootEP ep =
            { config
            | endpoints = epB4 ++ [ ep ] ++ epA 
            }
        
        updootLay cfg =
            { layer
            | config = xform cfg
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
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
            , width fill
            ]
            [ makeLabel "Z-Index"
            , row
                [ width fill
                , spacing 5
                ]
                [ endpoint.zIndex |> String.fromInt |> makeLabel
                , simpleAppButton
                    "-100"
                    (
                        { endpoint 
                        | zIndex = endpoint.zIndex - 100
                        }
                        |> updoot
                    )
                , simpleAppButton
                    "-10"
                    (
                        { endpoint 
                        | zIndex = endpoint.zIndex - 10
                        }
                        |> updoot
                    )
                , simpleAppButton
                    "-1"
                    (
                        { endpoint 
                        | zIndex = endpoint.zIndex - 1
                        }
                        |> updoot
                    )
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
                    , label = Input.labelHidden <|  "Z-Index"
                    , min = 0
                    , max = 1000
                    , step = Just 1
                    , value = endpoint.zIndex |> toFloat
                    , thumb = Input.defaultThumb
                    }
                , simpleAppButton
                    "+1"
                    (
                        { endpoint 
                        | zIndex = endpoint.zIndex + 1
                        }
                        |> updoot
                    )
                , simpleAppButton
                    "+10"
                    (
                        { endpoint 
                        | zIndex = endpoint.zIndex + 10
                        }
                        |> updoot
                    )
                , simpleAppButton
                    "+100"
                    (
                        { endpoint 
                        | zIndex = endpoint.zIndex + 100
                        }
                        |> updoot
                    )
                ]
            ]
        , row
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
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
            [spacing 6 
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
                [spacing 6
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
                [spacing 6
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

simpleAppButton : String -> msg -> Element msg
simpleAppButton label action =
    Input.button
        [ padding 10
        , height <| px 50
        , Border.rounded 1
        , Border.width 1
        , centerX
        ]
        { label =
            text label
        , onPress =
                Just action
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