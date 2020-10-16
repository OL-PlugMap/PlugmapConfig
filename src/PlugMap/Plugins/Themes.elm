port module PlugMap.Plugins.Themes exposing (..)

import Color as Color exposing (Color)
import Color.Accessibility exposing (maximumContrast)
import Dict exposing (Dict)
import Html exposing (Html, button, div, footer, i, img, main_, section, text)
import Html.Attributes exposing (class, id, src, title)
import Html.Events exposing (onClick)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as E
import Json.Encode.Extra as EEx
import List.Extra as LEx
import Maybe.Extra as MEx
import Json.Decode.Extra as DEx


port addThemesCmd : E.Value -> Cmd msg


type alias Model =
    { layerRepo : LayerRepo
    , layerGroupRepo : LayerGroupRepo
    , layerCategories : List LayerCategory
    }


defaultModel : Model
defaultModel =
    { layerRepo = Dict.empty
    , layerGroupRepo = Dict.empty
    , layerCategories = []
    }


type CategoryKey
    = CategoryKey String


type alias LayerCategory =
    { key : CategoryKey
    , name : String
    , selection : Selection
    , multiphasic : Multiphasic
    , transparency : Transparency
    , openness : Openness
    , usesRasterLegend : Bool
    , activeIcon : LinkImage
    , infoIcon : LinkImage    
    , layerGroups : List GroupKey
    , hidden : Bool
    }


type alias LayerGroupRepo =
    Dict String LayerGroup


type GroupKey
    = GroupKey String


type alias LayerGroup =
    { key : GroupKey
    , name : Maybe String
    , openness : Openness
    , layers : List LayerKey
    }


type alias LayerRepo =
    Dict String Layer


type LayerKey
    = LayerKey String


type alias Layer =
    { key : LayerKey
    , name : String
    , opacity : Maybe Float
    , config : LayerConfig
    , legend : Maybe Legend
    , identify : Maybe Identify
    }


type Selection
    = Monoselection (Maybe LayerKey)
    | Polyselection (List LayerKey)
    | EnforcedMonoselection LayerKey
    | EnforcedPolyselection LayerKey (List LayerKey)


type Selectiveness
    = Monoselective
    | Polyselective


type Openness
    = Open
    | Closed


type alias Multiphasic =
    Bool


type alias Transparency =
    Float


type LinkImage
    = NoImage
    | Img String
    | Icon String


type LayerConfig
    = XYZLayer XYZConfig
    | WMTSLayer WMTSConfig
    | WMSLayer WMSConfig
    | EsriMapServiceLayer EsriConfig
    | EsriFeatureServiceLayer EsriConfig
    | MapboxVectorTile MVTConfig
    | UnknownLayer

type alias MVTConfig =
    { endpoints : MVTEndpoints
    }

type alias MVTEndpoints =
    List MVTEndpoint

type alias MVTEndpoint =
    { url : String
    , zIndex : Maybe Int
    , tokenKey : Maybe String
    , style : MVTStyle
    , filter : Maybe MVTFilter
    }

type alias MVTFilter =
    { field : String
    , map : Dict String String
    , mode : MVTFilterMode
    }

type MVTFilterMode
    = AND 
    | OR



type MVTStyle
    = StaticStyle StaticMVTStyle
    | DynamicStyle DynamicMVTStyle

type alias StaticMVTStyle =
    { strokeColor : Maybe String
    , strokeWidth : Maybe Int
    , fillColor : Maybe String
    }

type alias DynamicMVTStyle =
    { field : String
    , map : Dict String StaticMVTStyle
    }


encodeMapboxConfig : MVTConfig -> E.Value
encodeMapboxConfig config =
    E.object
        [ ("endpoints", E.list encodeMVTEndpoint config.endpoints ) 
        ]

encodeMVTEndpoint : MVTEndpoint -> E.Value
encodeMVTEndpoint endpoint =
    E.object
        [ ( "url", E.string endpoint.url )
        , ( "zIndex", EEx.maybe E.int endpoint.zIndex )
        , ( "tokenKey", EEx.maybe E.string endpoint.tokenKey )
        , ( "style", encodeMVTStyle endpoint.style )
        , ( "filter", EEx.maybe encodeMVTFilter endpoint.filter )
        ]

encodeMVTFilter : MVTFilter -> E.Value
encodeMVTFilter filter =
    E.object
        [ ( "field", E.string filter.field )
        , ( "map", E.dict (identity) E.string filter.map )
        , ( "mode", encodeFilterMode filter.mode )
        ]

encodeFilterMode : MVTFilterMode -> E.Value
encodeFilterMode mode =
    case mode of
        AND ->
            E.string "AND"
        OR ->
            E.string "OR"

encodeMVTStyle : MVTStyle -> E.Value
encodeMVTStyle style =
    case style of 
        StaticStyle s ->
            E.object
                [ ( "static", encodeStaticMVTStyle s )
                ]
        DynamicStyle s ->
            E.object
                [ ( "dynamic", encodeDynamicMVTStyle s )
                ]

encodeDynamicMVTStyle : DynamicMVTStyle -> E.Value
encodeDynamicMVTStyle style =
    E.object
        [ ( "field", E.string style.field )
        , ( "map", E.dict (identity) encodeStaticMVTStyle style.map )
        ]

dynamicMVTStyleDecoder : Decoder DynamicMVTStyle
dynamicMVTStyleDecoder =
    D.succeed DynamicMVTStyle
    |> required "field" D.string
    |> required "map" (D.dict staticMVTStyleDecoder)

staticMVTStyleDecoder : Decoder StaticMVTStyle
staticMVTStyleDecoder =
    D.succeed StaticMVTStyle
    |> optional "strokeColor" (D.maybe D.string) Nothing
    |> optional "strokeWidth" (D.maybe D.int) Nothing
    |> optional "fillColor" (D.maybe D.string) Nothing

encodeStaticMVTStyle : StaticMVTStyle -> E.Value
encodeStaticMVTStyle style =
    E.object
        [ ( "strokeColor", EEx.maybe E.string style.strokeColor )
        , ( "strokeWidth", EEx.maybe E.int style.strokeWidth )
        , ( "fillColor", EEx.maybe E.string style.fillColor )
        ]

decodeMVTEndpoint : Decoder MVTEndpoint
decodeMVTEndpoint =
    D.succeed MVTEndpoint
    |> required "url" D.string
    |> optional "zIndex" (D.maybe D.int) Nothing
    |> optional "tokenKey" (D.maybe D.string) Nothing
    |> optional "style" mvtStyleDecoder defaultStaticMVTStyle
    |> optional "filter" (D.maybe filterDecoder) Nothing

filterDecoder : Decoder MVTFilter
filterDecoder =
    D.succeed MVTFilter
    |> required "field" D.string
    |> required "map" (D.dict D.string)
    |> optional "mode" mvtFilterModeDecoder OR

mvtFilterModeDecoder : Decoder MVTFilterMode
mvtFilterModeDecoder =
    D.string
    |> D.andThen
        (\v ->
            case v of
                "AND" -> D.succeed AND
                "OR" -> D.succeed OR
                _ -> D.fail <| "Unrecognized value '" ++ v ++ "'. Must be one of [ 'AND', 'OR' ]"
        )

defaultStaticMVTStyle : MVTStyle
defaultStaticMVTStyle =
    StaticStyle
        { strokeColor = Just "rgb(0,0,0)"
        , fillColor = Just "rgb(255,255,255)"
        , strokeWidth = Just 4
        }

mvtStyleDecoder : Decoder MVTStyle
mvtStyleDecoder =
    D.oneOf
        [ D.field "static" staticMVTStyleDecoder |> D.map StaticStyle 
        , D.field "dynamic" dynamicMVTStyleDecoder |> D.map DynamicStyle 
        ]

decodeMVTConfig : Decoder MVTConfig
decodeMVTConfig =
    D.succeed MVTConfig
    |> required "endpoints" (D.list decodeMVTEndpoint)

type alias XYZConfig =
    { endpoints : List Endpoint
    , maxZoom : Int
    , minZoom : Int
    }


type alias WMTSConfig =
    { endpoints : List Endpoint
    , extent : Extent
    }


type alias WMSConfig =
    { endpoints : List WMSEndpoint
    , extent : Extent
    }


type alias EsriConfig =
    { endpoints : List Endpoint
    , extent : Extent   
    , layerDefs : Maybe String 
    }


type alias WMSEndpoint =
    { url : String
    , zIndex : Int
    , tokenKey : String
    , layers : List String
    , propertyNames : List String
    , nameField : String
    , idField : String
    }


type alias Endpoint =
    { url : String
    , zIndex : Int
    , tokenKey : Maybe String
    , layersToShow : Maybe String
    , layerDefs : Maybe String
    , bbox : Maybe String
    }


type alias Extent =
    List Float


type alias Legend =
    List LegendEntry


type alias LegendEntry =
    { name : String
    , color : Color
    , fontColor : Color
    }


type alias Identify =
    { enabled : Bool
    , dataMapping : IdentifyDataMapping
    , canViewDetails : Bool
    , idTransform : Int --For remote data we need to add this to the id to link things up!
    , typeKey : String
    , queryLayerIds : Maybe (List Int)
    , tolerance : Maybe (Int)
    }


type alias IdentifyDataMapping =
    { title : IdentifyField
    , id : IdentifyField
    , fields : List IdentifyField
    }


type alias IdentifyField =
    { name : String
    , fieldType : IdentifyFieldType
    }


type IdentifyFieldType
    = Query QueryIdentifyField
    | Static StaticIdentifyField
    | Map ValueMapperParam


type alias QueryIdentifyField =
    String


type alias StaticIdentifyField =
    String



--
-- INIT
--


init : D.Value -> ( Result String Model, Cmd Msg )
init flags =
    case D.decodeValue (D.field "mapConfig" themesDecoder) flags of
        Ok model ->
            ( Ok model
            , addThemesCmd <| encodeThemes model
            )

        Err err ->
            ( Err <| "PlugMap.Plugins.Themes.init :: " ++ (errorToString err)
            , Cmd.none
            )            


denit : Model -> ( Model, Cmd Msg )
denit model =
    ( model
    , Cmd.none
    )



--
-- UPDATE
--


type Msg
    = Noop
    | ThemesUpdated D.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



--
-- SUBSCRIPTIONS
--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--
-- API
--

getAllCategories : Model -> List LayerCategory
getAllCategories model =
    model.layerCategories

getCategoryByKey : Model -> CategoryKey -> List LayerCategory
getCategoryByKey model key =
    List.filter ( \a -> a.key == key) model.layerCategories

getCategoryFromLayerKey : Model -> LayerKey -> List LayerCategory
getCategoryFromLayerKey model key =
    List.filter (\c -> categoryHasLayer key c model) model.layerCategories

setCategoryTransparency : Float -> LayerCategory -> Model -> ( LayerCategory, Model )
setCategoryTransparency t category model =
    let
        newCategory =
            { category | transparency = clamp 0.0 1.0 t }

        newCategories =
            replaceCategory newCategory model.layerCategories
    in
    ( newCategory
    , { model | layerCategories = newCategories }
    )


toggleCategoryTransparency : LayerCategory -> Model -> ( LayerCategory, Model )
toggleCategoryTransparency category model =
    let
        newCategory =
            if category.transparency == 0.0 then
                { category | transparency = 1.0 }

            else
                { category | transparency = 0.0 }

        newCategories = 
            replaceCategory newCategory model.layerCategories
    in
    ( newCategory
    , { model | layerCategories = newCategories }
    )


isCategoryFullyTransparent : LayerCategory -> Bool
isCategoryFullyTransparent category =
    category.transparency == 0.0


toggleCategoryOpenness : LayerCategory -> Model -> Model
toggleCategoryOpenness category model =
    let
        newCategory =
            case category.openness of
                Open ->
                    { category | openness = Closed }

                Closed ->
                    { category | openness = Open }

        newCategories =
            replaceCategory newCategory model.layerCategories
    in
    { model | layerCategories = newCategories }


toggleGroupOpenness : LayerGroup -> Model -> Model
toggleGroupOpenness group model =
    let
        newGroup =
            case group.openness of
                Open ->
                    { group | openness = Closed }

                Closed ->
                    { group | openness = Open }

        newGroups =
            model.layerGroupRepo
                |> Dict.update (groupKeyToString group.key) (always <| Just newGroup)
    in
    { model | layerGroupRepo = newGroups }




toggleLayerSelection : LayerKey -> LayerCategory -> Model -> Model
toggleLayerSelection key category model =
    if categoryHasLayer key category model then
        let
            newSelection =
                updateSelection key category.selection

            newCategory =
                { category | selection = newSelection }

            newCategories = 
                replaceCategory newCategory model.layerCategories
        in
        { model | layerCategories = newCategories }

    else
        model




toggleLayerSelectionOn : LayerKey -> LayerCategory -> Model -> LayerCategory
toggleLayerSelectionOn key category model =
        let
            newSelection =
                updateSelectionOn key category.selection

            newCategory =
                { category | selection = newSelection }

            newCategories = 
                replaceCategory newCategory model.layerCategories
        in
        newCategory


updateSelectionOn : LayerKey -> Selection -> Selection
updateSelectionOn layerKey selection =
    if not ( isLayerSelected layerKey selection ) then
        -- remove layer key
        case selection of
            Monoselection _ ->
                Monoselection (Just layerKey)

            Polyselection keys ->
                Polyselection (layerKey :: keys)

            EnforcedMonoselection key ->
                -- cannot remove
                EnforcedMonoselection layerKey

            EnforcedPolyselection key [] ->
                EnforcedPolyselection layerKey [ key ]

            EnforcedPolyselection key (v) ->
                EnforcedPolyselection layerKey v
    else  
        selection


toggleLayerSelectionOff : LayerKey -> LayerCategory -> Model -> LayerCategory
toggleLayerSelectionOff key category model =
        let
            newSelection =
                updateSelectionOff key category.selection

            newCategory =
                { category | selection = newSelection }

            newCategories = 
                replaceCategory newCategory model.layerCategories
        in
        newCategory



updateSelectionOff : LayerKey -> Selection -> Selection
updateSelectionOff layerKey selection =
    if isLayerSelected layerKey selection then
        -- remove layer key
        case selection of
            Monoselection _ ->
                Monoselection Nothing

            Polyselection keys ->
                let
                    ok = keys
                    nk = 
                        keys
                        |> List.filter (\key -> key /= layerKey)
                        |> Polyselection

                in
                nk

            EnforcedMonoselection key ->
                -- cannot remove
                EnforcedMonoselection key

            EnforcedPolyselection key [] ->
                EnforcedPolyselection key []

            EnforcedPolyselection key (head :: tail) ->
                if key == layerKey then
                    EnforcedPolyselection head tail

                else
                    (head :: tail)
                        |> List.filter (\k -> k /= layerKey)
                        |> EnforcedPolyselection key 
    else  
        selection

resetCategory : CategoryKey -> Model -> Model
resetCategory key model =
    let
        meow = getCategoryByKey model key
        newSelection = deselectAll meow
        newCategories = 
                newSelection |> List.head |> Maybe.map (\cat -> replaceCategory cat model.layerCategories ) |> Maybe.withDefault model.layerCategories
    in
        { model | layerCategories = newCategories }

deselectAll : List LayerCategory -> List LayerCategory
deselectAll cats =
    List.map
        (\cat ->
            let 
                ns = 
                    case cat.selection of
                        Monoselection _ ->
                            Monoselection Nothing

                        Polyselection keys ->
                            [] |> Polyselection

                        EnforcedMonoselection key ->
                            -- cannot remove
                            EnforcedMonoselection key

                        EnforcedPolyselection key [] ->
                            EnforcedPolyselection key []

                        EnforcedPolyselection key (head :: tail) ->
                                []
                                |> EnforcedPolyselection key 
            in
                { cat | selection = ns }
        )
        cats

updateSelection : LayerKey -> Selection -> Selection
updateSelection layerKey selection =
    if isLayerSelected layerKey selection then
        -- remove layer key
        case selection of
            Monoselection _ ->
                Monoselection Nothing

            Polyselection keys ->
                keys
                    |> List.filter (\key -> key /= layerKey)
                    |> Polyselection

            EnforcedMonoselection key ->
                -- cannot remove
                EnforcedMonoselection key

            EnforcedPolyselection key [] ->
                EnforcedPolyselection key []

            EnforcedPolyselection key (head :: tail) ->
                if key == layerKey then
                    EnforcedPolyselection head tail

                else
                    (head :: tail)
                        |> List.filter (\k -> k /= layerKey)
                        |> EnforcedPolyselection key 
    else  
        -- add layer key
        case selection of
            Monoselection _ ->
                Monoselection <| Just layerKey

            Polyselection keys ->
                Polyselection (layerKey :: keys)

            EnforcedMonoselection _ ->
                EnforcedMonoselection layerKey

            EnforcedPolyselection key keys ->
                EnforcedPolyselection layerKey (key :: keys)


isLayerSelected : LayerKey -> Selection -> Bool
isLayerSelected layerKey selection =
    case selection of
        Monoselection (Just key) ->
            key == layerKey

        Polyselection (head :: tail) ->
            (head :: tail)
                |> List.any (\k -> k == layerKey)

        EnforcedMonoselection key ->
            key == layerKey

        EnforcedPolyselection key (head :: tail) ->
            (key :: head :: tail)
                |> List.any (\k -> k == layerKey)
    
        _ ->
            False


getRasterLayer : Model -> Maybe Layer
getRasterLayer model =
    let
        selection = 
            model.layerCategories
                |> categoriesWithLegend
                |> List.head
                |> Maybe.map .selection
    in
    case selection of
        Just (Monoselection (Just key)) ->
            getLayerByKey model.layerRepo key

        Just (Polyselection (head :: _)) ->
            getLayerByKey model.layerRepo head

        Just (EnforcedMonoselection key) ->
            getLayerByKey model.layerRepo key

        Just (EnforcedPolyselection head _) ->
            getLayerByKey model.layerRepo head

        _ ->
            Nothing


categoryKeyToString : CategoryKey -> String
categoryKeyToString (CategoryKey key) =
    key

stringToCategoryKey : String -> CategoryKey
stringToCategoryKey key =
    CategoryKey key


groupKeyToString : GroupKey -> String
groupKeyToString (GroupKey key) =
    key


stringToGroupKey : String -> GroupKey
stringToGroupKey key =
    GroupKey key


layerKeyToString : LayerKey -> String
layerKeyToString (LayerKey key) =
    key

stringToLayerKey : String -> LayerKey
stringToLayerKey key =
    LayerKey key


-- getReferenceLayers : Model -> List Layer
-- getReferenceLayers model =
--    []
    -- model.referenceLayers
    --     |> List.filterMap (getLayerByKey model.layerRepo)


getGroupsForCategory : LayerCategory -> LayerGroupRepo -> List LayerGroup
getGroupsForCategory category repo =
    category.layerGroups
        |> List.filterMap (getGroupByKey repo)


getLayersForGroup : LayerGroup -> LayerRepo -> List Layer
getLayersForGroup group repo =
    group.layers
        |> List.filterMap (getLayerByKey repo)

insertLayer : Layer -> Model -> Model
insertLayer layer model =
    updateLayer model layer


getLayerByKey : LayerRepo -> LayerKey -> Maybe Layer
getLayerByKey repo (LayerKey key) =
    Dict.get key repo


getGroupByKey : LayerGroupRepo -> GroupKey -> Maybe LayerGroup
getGroupByKey repo (GroupKey key) =
    Dict.get key repo


updateGroup : Model -> LayerGroup -> Model
updateGroup model group =
    { model 
    | layerGroupRepo = Dict.insert (groupKeyToString group.key) group model.layerGroupRepo
    }

removeGroup : Model -> GroupKey -> Model
removeGroup model grp =
    { model 
    | layerGroupRepo = Dict.remove (groupKeyToString grp) model.layerGroupRepo
    }

updateLayer : Model -> Layer -> Model
updateLayer model layer =
    { model 
    | layerRepo = Dict.insert (layerKeyToString layer.key) layer model.layerRepo
    }

removeLayer : Model -> LayerKey -> Model
removeLayer model layer =
    { model 
    | layerRepo = Dict.remove (layerKeyToString layer) model.layerRepo
    }

layersWithIdentify : LayerRepo -> List Layer
layersWithIdentify repo =
    repo
        |> Dict.filter (\_ lyr -> MEx.isJust lyr.identify)
        |> Dict.values


layersWithLegend : LayerRepo -> List Layer
layersWithLegend repo =
    repo
        |> Dict.filter (\_ lyr -> MEx.isJust lyr.legend)
        |> Dict.values


groupHasLayer : LayerKey -> LayerGroup -> Bool
groupHasLayer key group =
    List.member key group.layers


categoryHasLayer : LayerKey -> LayerCategory -> Model -> Bool
categoryHasLayer key category model =
    category.layerGroups
        |> List.filterMap (getGroupByKey model.layerGroupRepo)
        |> List.any (groupHasLayer key)
    


--
-- VIEW
--


view : Model -> Html Msg
view model =
    div [] []



--
-- INTERNAL
--


replaceCategory : LayerCategory -> List LayerCategory -> List LayerCategory
replaceCategory category categories =
    categories
        |> List.map
            (\cat ->
                if cat.key == category.key then
                    category

                else
                    cat
            )


categoriesWithLegend : List LayerCategory -> List LayerCategory
categoriesWithLegend categories =
    categories
        |> List.filter .usesRasterLegend


darkFont : Color
darkFont =
    Color.rgb255 36 36 36


lightFont : Color
lightFont =
    Color.white

--
-- DECODERS
--


opennessDecoder : Decoder Openness
opennessDecoder =
    D.string
        |> D.andThen
            (\openness ->
                case openness of
                    "open" ->
                        D.succeed Open

                    "closed" ->
                        D.succeed Closed

                    _ ->
                        D.fail "PlugMap.Plugins.Themes.opennessDecoder :: invalid configuration value. Must be either 'open' or 'closed'."
            )


selectivenessDecoder : Decoder Selectiveness
selectivenessDecoder =
    D.string
        |> D.andThen
            (\selectiveness ->
                case selectiveness of
                    "monoselective" ->
                        D.succeed Monoselective

                    "polyselective" ->
                        D.succeed Polyselective

                    _ ->
                        D.fail "PlugMap.Plugins.Themes.selectivenessDecoder :: invalid configuration value. Must be either 'monoselective' or 'polyselective'."
            )


transparencyDecoder : Decoder Transparency
transparencyDecoder =
    D.float
        |> D.andThen (D.succeed << clamp 0.0 1.0)


categoryKeyDecoder : Decoder CategoryKey
categoryKeyDecoder =
    D.string
        |> D.andThen (D.succeed << CategoryKey)


layerKeyDecoder : Decoder LayerKey
layerKeyDecoder =
    D.string
        |> D.andThen (D.succeed << LayerKey)


groupKeyDecoder : Decoder GroupKey
groupKeyDecoder =
    D.string
        |> D.andThen (D.succeed << GroupKey)


linkImageDecoder : Decoder LinkImage
linkImageDecoder =
    D.oneOf
        [ D.field "iconImg" (D.map Img D.string)
        , D.field "iconClass" (D.map Icon D.string)
        , D.succeed NoImage
        ]




themesDecoder : Decoder Model
themesDecoder =
    D.succeed Model
        |> required "layers" layerRepoDecoder
        |> required "layerGroups" layerGroupRepoDecoder
        |> required "layerCategories" layerCategoriesDecoder


layerCategoriesDecoder : Decoder (List LayerCategory)
layerCategoriesDecoder =
    D.list layerCategoryDecoder


layerCategoryDecoder : Decoder LayerCategory
layerCategoryDecoder =
    D.succeed LayerCategory
        |> required "key" categoryKeyDecoder
        |> required "name" D.string
        |> custom selectionDecoder
        |> required "multiphasic" D.bool
        |> required "transparency" transparencyDecoder
        |> required "openness" opennessDecoder
        |> required "usesRasterLegend" D.bool
        |> required "activeIcon" linkImageDecoder
        |> required "infoIcon" linkImageDecoder        
        |> required "layerGroups" (D.list groupKeyDecoder)
        |> optional "hidden" D.bool False


selectionDecoder : Decoder Selection
selectionDecoder = 
    let
        toSelection : Selectiveness -> Bool -> List LayerKey -> Result String Selection
        toSelection selectiveness mustHave defaults =
            let
                errMsg =
                    "PlugMap.Plugins.Themes.selectionDecoder :: invalid configuration value. If a Layer Category has 'mustHaveSelection=true' then it must also have a valid 'defaultSelection' set."
            in
            case ( selectiveness, mustHave ) of
                ( Monoselective, False ) ->
                    case defaults of
                        [] ->
                            Ok <| Monoselection Nothing

                        head :: _ ->
                            Ok <| Monoselection <| Just head

                ( Polyselective, False ) ->
                    Ok <| Polyselection defaults

                ( Monoselective, True ) ->
                    case defaults of
                        [] ->
                            Err errMsg

                        head :: _ ->
                            Ok <| EnforcedMonoselection head

                ( Polyselective, True ) ->
                    case defaults of
                        [] ->
                            Err errMsg

                        head :: tail ->
                            Ok <| EnforcedPolyselection head tail
    in
    D.map3 toSelection
        (D.field "selectiveness" selectivenessDecoder)
        (D.field "mustHaveSelection" D.bool)
        (D.field "defaultSelection" (D.list layerKeyDecoder)
            |> DEx.withDefault []
        )
        |> D.andThen DEx.fromResult


layerGroupRepoDecoder : Decoder LayerGroupRepo
layerGroupRepoDecoder =
    D.list layerGroupDecoder
        |> D.andThen (D.succeed << Dict.fromList)


layerGroupDecoder : Decoder ( String, LayerGroup )
layerGroupDecoder =
    D.succeed LayerGroup
        |> required "key" groupKeyDecoder
        |> optional "name" (D.maybe D.string) Nothing
        |> required "openness" opennessDecoder
        |> required "layers" (D.list layerKeyDecoder)
        |> D.andThen
            (\lg ->
                D.succeed <|
                    ( groupKeyToString lg.key
                    , lg
                    )
            )


layerRepoDecoder : Decoder LayerRepo
layerRepoDecoder =
    D.list layerDecoder
        |> D.andThen (D.succeed << Dict.fromList)


layerDecoder : Decoder ( String, Layer )
layerDecoder =
    D.succeed Layer
        |> required "key" layerKeyDecoder
        |> required "name" D.string
        |> optional "opacity" (D.maybe D.float) Nothing
        |> custom configDecoder
        |> optional "legend" (D.maybe legendDecoder) Nothing
        |> optional "identify" (D.maybe identifyDecoder) Nothing
        |> D.andThen
            (\ly ->
                D.succeed <|
                    ( layerKeyToString ly.key
                    , ly
                    )
            )


configDecoder : Decoder LayerConfig
configDecoder =
    D.oneOf
        [ xyzLayerDecoder
        , wmtsLayerDecoder
        , wmsLayerDecoder
        , esriMapServiceLayerDecoder
        , esriFeatureServiceLayerDecoder
        , mapboxVectorTileLayerDecoder
        , D.succeed UnknownLayer
        ]


xyzLayerDecoder : Decoder LayerConfig
xyzLayerDecoder =
    D.succeed XYZLayer
        |> required "xyz" xyzConfigDecoder        


mapboxVectorTileLayerDecoder : Decoder LayerConfig
mapboxVectorTileLayerDecoder =
    D.succeed MapboxVectorTile
        |> required "mvt" decodeMVTConfig   

xyzConfigDecoder : Decoder XYZConfig
xyzConfigDecoder =
    D.succeed XYZConfig
        |> required "endpoints" (D.list endpointDecoder)
        |> required "maxZoom" D.int
        |> required "minZoom" D.int




endpointDecoder : Decoder Endpoint
endpointDecoder =
    D.succeed Endpoint
        |> required "url" D.string
        |> custom ( D.oneOf [D.field "z-index" D.int, D.field "zIndex" D.int] )
        |> optional "tokenKey" (D.maybe D.string) Nothing
        |> optional "layersToShow" (D.maybe D.string) Nothing
        |> optional "layerDefs" (D.maybe D.string) Nothing
        |> optional "bbox" (D.maybe D.string) Nothing


wmsEndpointsDecoder : Decoder (List WMSEndpoint)
wmsEndpointsDecoder =
    D.list wmsEndpointDecoder


wmsEndpointDecoder : Decoder WMSEndpoint
wmsEndpointDecoder =
    D.succeed WMSEndpoint
        |> required "url" D.string
        |> optional "z-index" D.int 0
        |> required "tokenKey" D.string
        |> required "layers" (D.list D.string)
        |> required "propertyNames" (D.list D.string)
        |> required "nameField" D.string
        |> required "idField" D.string


extentDecoder : Decoder Extent
extentDecoder =
    D.list D.float


wmsLayerDecoder : Decoder LayerConfig
wmsLayerDecoder =
    D.succeed WMSLayer
        |> required "wms" wmsConfigDecoder

wmsConfigDecoder : Decoder WMSConfig
wmsConfigDecoder =
    D.succeed WMSConfig
        |> required "endpoints" wmsEndpointsDecoder
        |> required "extent" extentDecoder


wmtsLayerDecoder : Decoder LayerConfig
wmtsLayerDecoder =
    D.succeed WMTSLayer
        |> required "wmts" wmtsConfigDecoder


wmtsConfigDecoder : Decoder WMTSConfig
wmtsConfigDecoder =
    D.succeed WMTSConfig
        |> required "endpoints" (D.list endpointDecoder)
        |> required "extent" extentDecoder


esriMapServiceLayerDecoder : Decoder LayerConfig
esriMapServiceLayerDecoder =
    D.oneOf
    [ D.succeed EsriMapServiceLayer
        |> required "esriExport" esriConfigDecoder
    , D.succeed EsriMapServiceLayer
        |> required "esriMapServivce" esriConfigDecoder
    ]

esriFeatureServiceLayerDecoder : Decoder LayerConfig
esriFeatureServiceLayerDecoder =
    D.succeed EsriFeatureServiceLayer
        |> required "esriFeature" esriConfigDecoder

defaultExtent =
    [0,0,0,0]

esriConfigDecoder : Decoder EsriConfig
esriConfigDecoder =
    D.succeed EsriConfig
        |> required "endpoints"(D.list endpointDecoder)
        |> optional "extent" extentDecoder defaultExtent
        |> optional "layerDefs" (D.maybe D.string) Nothing      


identifyDecoder : D.Decoder Identify
identifyDecoder =
    D.succeed Identify
        |> required "enabled" D.bool
        |> required "dataMapping" identifyDataMappingDecoder
        |> required "canViewDetails" D.bool
        |> optional "idTransform" D.int 0
        |> optional "typeKey" D.string "Unknown"
        |> optional "queryLayerIds" (D.maybe (D.list D.int)) Nothing
        |> optional "tolerance" (D.maybe D.int) Nothing


identifyFieldDecoder : D.Decoder IdentifyField
identifyFieldDecoder =
    D.succeed IdentifyField
        |> required "name" D.string
        |> custom
            (D.oneOf
                [ D.succeed Query
                    |> required "query" D.string
                , D.succeed Static
                    |> required "static" D.string
                , D.succeed Map
                    |> required "map" mapDecoder
                ]
            )

type alias ValueMapperParam =
    { query : String
    , values : Dict String String
    }

mapDecoder : Decoder ValueMapperParam
mapDecoder =
    D.succeed ValueMapperParam
        |> required "query" D.string
        |> required "values" (D.dict D.string)

identifyDataMappingDecoder : D.Decoder IdentifyDataMapping
identifyDataMappingDecoder =
    D.succeed IdentifyDataMapping
        |> required "title" identifyFieldDecoder
        |> required "id" identifyFieldDecoder
        |> required "fields" (D.list identifyFieldDecoder)


colorDecoder : D.Decoder Color
colorDecoder =
    D.succeed Color.rgb255
        |> required "r" D.int
        |> required "g" D.int
        |> required "b" D.int


fontColorDecoder : D.Decoder Color
fontColorDecoder =
    D.at [ "color" ] colorDecoder
        |> D.andThen
            (\c ->
                maximumContrast c [ lightFont, darkFont ]
                    |> Maybe.withDefault darkFont
                    |> D.succeed
            )


legendDecoder : D.Decoder Legend
legendDecoder =
    D.list legendEntryDecoder


legendEntryDecoder : D.Decoder LegendEntry
legendEntryDecoder =
    D.succeed LegendEntry
        |> required "label" D.string
        |> required "color" colorDecoder
        |> custom fontColorDecoder



--
-- ENCODERS
--


encodeCategoryTransparency : LayerCategory -> E.Value
encodeCategoryTransparency category =
    E.object
        [ ("category_key", encodeCategoryKey category.key)
        , ("transparency", E.float category.transparency)
        ]


encodeSelectedThemes : Model -> E.Value
encodeSelectedThemes model =
    model.layerCategories
        |> List.filter (\cat -> not cat.hidden )
        |> E.list encodeCategorySelection


encodeCategorySelection : LayerCategory -> E.Value
encodeCategorySelection category =
    E.object
        [ ("category_key", encodeCategoryKey category.key)
        , ("selection", encodeSelection category.selection)
        ]


encodeThemes : Model -> E.Value
encodeThemes model =
    model.layerCategories
        |> E.list (encodeCategory model)


encodeThemesDB : Model -> E.Value
encodeThemesDB model =
    E.object
        [ ( "layerCategories", E.list encodeLayerCategoryDB model.layerCategories )
        , ( "layerGroups", E.list encodeLayerGroupDB <| Dict.values model.layerGroupRepo )
        , ( "layers", E.list encodeLayerDB <| Dict.values model.layerRepo )
        ]


encodeLayerGroupDB : LayerGroup -> E.Value
encodeLayerGroupDB group =
    E.object
        [ ( "key", encodeGroupKey group.key )
        , ( "name", EEx.maybe E.string group.name )
        , ( "openness", encodeOpenness group.openness )
        , ( "layers", E.list encodeLayerKey group.layers )
        ]

encodeLayerDB : Layer -> E.Value
encodeLayerDB layer =
    E.object
        [ ( "key", encodeLayerKey layer.key )
        , ( "name", E.string layer.name )
        , ( "opacity", EEx.maybe E.float layer.opacity )
        , ( encodeLayerConfigDB layer.config )
        , ( "legend", EEx.maybe encodeLegend layer.legend )
        , ( "identify", EEx.maybe encodeIdentify layer.identify )
        ]

encodeLayerConfigDB : LayerConfig -> (String, E.Value)
encodeLayerConfigDB config_ =
    case config_ of
        XYZLayer config ->
            ( "xyz", encodeXYZLayer config )

        WMTSLayer config ->
            ( "wmts", encodeWMTSLayer config )

        WMSLayer config ->
            ( "wms", encodeWMSLayer config )

        EsriMapServiceLayer config ->
            ( "esriExport", encodeEsriMapServiceLayer config )

        EsriFeatureServiceLayer config ->
            ( "esriFeature", encodeEsriMapServiceLayer config )

        MapboxVectorTile config ->
            ( "mvt", encodeMapboxConfig config )
        
        UnknownLayer ->
            ( "unknown", E.object [] )


encodeLegend : Legend -> E.Value
encodeLegend legend =
    E.list legendEntryEncoder legend


legendEntryEncoder : LegendEntry -> E.Value
legendEntryEncoder legend =
    E.object
        [ ( "name", E.string legend.name )
        , ( "color", colorEncoder legend.color )
        , ( "fontColor", colorEncoder legend.fontColor )
        ]

colorEncoder : Color -> E.Value
colorEncoder color =
    color |> Color.toRgba
    |> (\c ->
            E.object
                [ ("r", E.int (floor(c.red * 255 )))
                , ("g", E.int (floor(c.green * 255 )))
                , ("b", E.int (floor(c.blue * 255 )))
                ]
        )


encodeLayerCategoryDB : LayerCategory -> E.Value
encodeLayerCategoryDB category =
    E.object
        [ ("key", encodeCategoryKey category.key)
        , ("name", E.string category.name)
        , ("hidden", E.bool category.hidden)
        , ("infoIcon", encodeLinkImage category.infoIcon )
        , ("openness", encodeOpenness category.openness )
        , ("activeIcon", encodeLinkImage category.activeIcon )
        , ("layerGroups", E.list encodeGroupKey category.layerGroups )
        , ("multiphasic", E.bool category.multiphasic)
        , ("transparency", E.float category.transparency)
        , ("selectiveness", encodeSelectionType category.selection)
        , ("defaultSelection", E.list encodeLayerKey 
                <| case category.selection of
                    Monoselection (Just v) ->
                        [v]
                    EnforcedMonoselection v ->
                        [v]
                    Polyselection v ->
                        v
                    EnforcedPolyselection v x ->
                        v :: x
                    _ -> []
            )
        , ( "usesRasterLegend", E.bool category.usesRasterLegend )
        , ( "mustHaveSelection", E.bool False )
        ]

encodeLinkImage : LinkImage -> E.Value
encodeLinkImage img =
    case img of 
        Img v ->
            E.object [ ( "iconImg", E.string v ) ]
        Icon v ->
            E.object [ ( "iconClass", E.string v ) ]
        NoImage ->
            E.null

encodeSelectionType : Selection -> E.Value
encodeSelectionType selection =
    E.string 
    <| case selection of 
        Monoselection _ ->
            "monoselective"
        EnforcedMonoselection _ ->
            "monoselective"
        _ ->
            "polyselective"

encodeOpenness : Openness -> E.Value
encodeOpenness openness =
    E.string 
    <| case openness of
        Open -> "open"
        _ -> "closed"

encodeGroupKey : GroupKey -> E.Value
encodeGroupKey key =
    key |> groupKeyToString |> E.string

encodeSelectiveness : Selectiveness -> E.Value
encodeSelectiveness selectiveness =
    E.string 
    <| case selectiveness of 
        Polyselective -> "polyselective"
        _ -> "monoselective"

encodeCategoryKey : CategoryKey -> E.Value
encodeCategoryKey (CategoryKey key) =
    E.string key


encodeCategory : Model -> LayerCategory -> E.Value
encodeCategory model category =
    let
        layers : List Layer
        layers = 
            category.layerGroups
                |> List.filterMap (getGroupByKey model.layerGroupRepo)
                |> List.concatMap .layers
                |> List.filterMap (getLayerByKey model.layerRepo)
    in
    E.object
        [ ("category_key", encodeCategoryKey category.key)
        , ("opacity", E.float category.transparency)
        , ("layers", E.list encodeLayer layers)
        , ("selection", encodeSelection category.selection)
        ]


encodeSelection : Selection -> E.Value
encodeSelection selection =
    case selection of
        Monoselection maybeKey ->
            E.object
                [ ("selection_type", E.string "monoselection")
                , ("selection_key", EEx.maybe encodeLayerKey maybeKey)
                ]

        Polyselection keys ->
            E.object
                [ ("selection_type", E.string "polyselection")
                , ("selection_keys", E.list encodeLayerKey keys)
                ]

        EnforcedMonoselection key ->
            E.object
                [ ("selection_type", E.string "monoselection")
                , ("selection_key", encodeLayerKey key)
                ]

        EnforcedPolyselection headKey tailKeys ->
            E.object
                [ ("selection_type", E.string "polyselection")
                , ("selection_keys", E.list encodeLayerKey (headKey :: tailKeys))
                ]


encodeLayerKey : LayerKey -> E.Value
encodeLayerKey (LayerKey key) =
    E.string key


encodeLayer : Layer -> E.Value
encodeLayer layer =
    E.object
        [ ( "key", encodeLayerKey layer.key )
        , ( "name", E.string layer.name )
        , ( "opacity", EEx.maybe E.float layer.opacity )
        , ( "config", encodeLayerConfig layer.config )
        , ( "identify", layer.identify |> Maybe.map encodeIdentify |> Maybe.withDefault E.null )
        ]


encodeLayerConfig : LayerConfig -> E.Value
encodeLayerConfig config_ =
    let
        ( type_, encoder ) =
            case config_ of
                XYZLayer config ->
                    ( "xyz", encodeXYZLayer config )

                WMTSLayer config ->
                    ( "wmts", encodeWMTSLayer config )

                WMSLayer config ->
                    ( "wms", encodeWMSLayer config )

                EsriMapServiceLayer config ->
                    ( "esriExport", encodeEsriMapServiceLayer config )

                EsriFeatureServiceLayer config ->
                    ( "esriFeature", encodeEsriMapServiceLayer config )

                MapboxVectorTile config ->
                    ( "mvt", encodeMapboxConfig config )
                
                UnknownLayer ->
                    ( "unknown", E.object [] )
    in
    E.object
        [ ( "type", E.string type_ )
        , ( "value", encoder )
        ]


encodeXYZLayer : XYZConfig -> E.Value
encodeXYZLayer config =
    E.object
        [ ( "endpoints", E.list encodeEndpoint config.endpoints )
        , ( "maxZoom", E.int config.maxZoom )
        , ( "minZoom", E.int config.minZoom )
        ]


encodeWMTSLayer : WMTSConfig -> E.Value
encodeWMTSLayer config =
    E.object
        [ ( "endpoints", E.list encodeEndpoint config.endpoints )
        , ( "extent", encodeExtent config.extent )
        ]


encodeEsriMapServiceLayer : EsriConfig -> E.Value
encodeEsriMapServiceLayer config =
    E.object
        [ ( "endpoints", E.list encodeEndpoint config.endpoints )
        , ( "extent", encodeExtent config.extent )      
        , ( "layerDefs", EEx.maybe E.string config.layerDefs)  
        ]


encodeWMSLayer : WMSConfig -> E.Value
encodeWMSLayer config =
    E.object
        [ ("endpoints", encodeWMSEndpoints config.endpoints)
        , ("extent", encodeExtent config.extent)
        ]


encodeEndpoint : Endpoint -> E.Value
encodeEndpoint endpoint =
    E.object
        [ ( "url", E.string endpoint.url )
        , ( "zIndex", E.int endpoint.zIndex )
        , ( "tokenKey", EEx.maybe E.string endpoint.tokenKey )
        , ( "layersToShow", EEx.maybe E.string endpoint.layersToShow )
        , ( "layerDefs", EEx.maybe E.string endpoint.layerDefs )
        , ( "bbox", EEx.maybe E.string endpoint.bbox )
        ]


encodeWMSEndpoints : List WMSEndpoint -> E.Value
encodeWMSEndpoints endpoints =
    E.list encodeWMSEndpoint endpoints
        

encodeWMSEndpoint : WMSEndpoint -> E.Value
encodeWMSEndpoint endpoint =
    E.object
        [ ( "url", E.string endpoint.url )
        , ( "tokenKey", E.string endpoint.tokenKey )
        , ( "layers", (E.list E.string) endpoint.layers )
        , ( "propertyNames", (E.list E.string) endpoint.propertyNames )
        , ( "nameField", E.string endpoint.nameField )
        , ( "idField", E.string endpoint.idField )
        ]


encodeExtent : Extent -> E.Value
encodeExtent extent =
    E.list E.float extent


encodeIdentify : Identify -> E.Value
encodeIdentify id =
    E.object
        [ ( "enabled", E.bool id.enabled )
        , ( "dataMapping", encodeIdentifyDataMapping id.dataMapping )
        , ( "canViewDetails", E.bool id.canViewDetails )
        , ( "idTransform", E.int id.idTransform )
        , ( "typeKey", E.string id.typeKey )
        , ( "queryLayerIds", id.queryLayerIds |> Maybe.map (E.list E.int) |> Maybe.withDefault E.null )
        , ( "tolerance", EEx.maybe E.int id.tolerance )
        ]


encodeIdentifyDataMapping : IdentifyDataMapping -> E.Value
encodeIdentifyDataMapping idm =
    E.object
        [ ( "title", encodeIdentifyField idm.title )
        , ( "id", encodeIdentifyField idm.id )
        , ( "fields", E.list encodeIdentifyField idm.fields )
        ]


encodeIdentifyField : IdentifyField -> E.Value
encodeIdentifyField idf =
    E.object
        [ ( "name", E.string idf.name )
        , case idf.fieldType of
            Query param ->
                ( "query", E.string param )

            Static param ->
                ( "static", E.string param )

            Map param ->
                ( "map", encodeMap param )
        ]

encodeMap : ValueMapperParam -> E.Value
encodeMap param =
    E.object
        [ ("query", E.string param.query)
        , ("values", (E.dict (identity) (E.string)) param.values )
        ]
