port module PlugMap.Plugins.Search exposing (..)


import Json.Decode as D exposing (Value, Decoder, succeed)
import Json.Decode.Pipeline exposing (required, optional, optionalAt)
import Json.Decode exposing (map)
import Json.Decode.Extra as Dex
import Json.Encode as E
import Json.Encode.Extra as Ex
import Http 

import QS as Qs

import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData, WebData, isSuccess)

import Task

import PAM.Settings as Settings

import PAM.UI.Basic exposing (Openness(..), PanelDimensions)

-- PORTS --

port gotoSearchCandidates : E.Value -> Cmd msg
port gotoSearchCandidate : E.Value -> Cmd msg
port clearSearchCmd : () -> Cmd msg


--TODO: Remove this
type alias Url = String

type alias Model =
    { results : WebData SuggestionsOrCandidates
    }

defaultModel : Model 
defaultModel =
    { results = RemoteData.NotAsked
    }

clear : Model -> ( Model, Cmd Msg )
clear model =
    ( { model | results = RemoteData.NotAsked }
    , Cmd.batch 
        [ clearSearchCmd ()
        -- , Task.perform
        --     (always (Suggest []))
        --     <| Task.succeed ""
        ]
    )

type alias API parentMsg =
    { onInternalMessage : InternalMsg -> parentMsg
    , onSuggest : SuggestionsOrCandidates -> parentMsg
    }


type alias APITranslator parentMsg =
    Msg -> parentMsg



apiTranslator : API parentMsg -> APITranslator parentMsg
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage m
        Suggest m ->
            td.onSuggest m


type Msg 
    = Internal InternalMsg
    | Suggest SuggestionsOrCandidates

type InternalMsg 
    = NoOp
    | Response (WebData SuggestionsOrCandidates)


--TODO: Refactor this a bit
type SuggestionOrCandidate
    = SuggestionData Suggestion
    | CandidateData Candidate

type alias SuggestionsOrCandidates =
    List SuggestionOrCandidate
    -- = SuggestionsData Suggestions
    -- | CandidatesData Candidates






-- Entrypoint --

{- Here is how the outside interacts with this module -}

{- Call this on a text input to get a list of suggestions to give back to the search module -}
fetchSuggestions : Model -> Settings.SearchConfig -> { location | x : Float, y : Float } -> String -> ( Model, Cmd Msg )
fetchSuggestions model config location search =
    ( { model | results = RemoteData.NotAsked }
    , getSuggestionsUrl location search config
        |> getSuggestions  
    )

{- Call this when the search button is clicked -}
fetchcandidates : Settings.SearchConfig -> { location | x : Float, y : Float } ->  String -> Cmd Msg
fetchcandidates config location search =
    getUrlForPlainText location search config
        |> getCandidatesFor ForCandidate


{- Call this when a user clicks on a search suggestion -}
fetchCandidatesForSuggestions : Settings.SearchConfig -> { location | x : Float, y : Float } -> Suggestion -> Cmd Msg
fetchCandidatesForSuggestions config location data =
    getUrlForSuggestion location data config
        |> getCandidatesFor ForSuggestion

fetchFor : 
    Settings.SearchConfig -> 
    { location | x : Float, y : Float } -> 
    SuggestionOrCandidate -> 
    Cmd Msg
fetchFor config location result =
    case result of
        SuggestionData data ->
            fetchCandidatesForSuggestions config location data
        CandidateData data ->
            selectSearchCandidate 
                data
                -- ( if env.sidePanelOpenness == Open then 
                --     env.panelDimensions.leftSidebarWidth
                -- else
                --     0)
                -- (if env.bottomPanelOpenness == Open then
                --     env.panelDimensions.bottomDrawerHeaderHeight + env.panelDimensions.bottomDrawerHeight
                -- else 0
                -- )
                0 0 --TODO

update : 
    -- { env
    --     | panelDimensions : PanelDimensions
    --     , bottomPanelOpenness : Openness
    --     , sidePanelOpenness : Openness
    -- } -> 
    Model -> 
    InternalMsg -> 
    ( Model, Cmd Msg )
update model msg =
    case msg of
        Response response ->
            ( { model | results = response }
            , case response of
                RemoteData.Success suggestions ->
                    case suggestions of 
                        justOne :: [] ->
                            case justOne of
                                CandidateData candidate ->
                                    --Show The Candidate
                                    selectSearchCandidate 
                                        candidate
                                        -- (getLeftPadding env)
                                        -- (getBottomPadding env)
                                        0 0 --TODO
                                _ ->
                                    --Fetch the candidate
                                    Cmd.none
                        [] ->
                            Cmd.none
                        _ ->
                            Cmd.batch 
                                [ Task.perform
                                    (always (Suggest suggestions))
                                    <| Task.succeed ""
                                , showSuggestions suggestions
                                ]
                _ ->
                    Cmd.none
                
                
            )

        NoOp ->
            (model, Cmd.none)


selectSearchCandidates : Candidates -> Int -> Int -> Cmd Msg
selectSearchCandidates candidates paddingLeft paddingBottom =
    encodeCandidates candidates paddingLeft paddingBottom |> gotoSearchCandidates

selectSearchCandidate : Candidate -> Int -> Int -> Cmd Msg
selectSearchCandidate candidate paddingLeft paddingBottom =
    encodeCandidates [ candidate ] paddingLeft paddingBottom |> gotoSearchCandidates



showSuggestions : 
    -- { env
    --     | panelDimensions : PanelDimensions
    --     , bottomPanelOpenness : Openness
    --     , sidePanelOpenness : Openness
    -- } ->
    SuggestionsOrCandidates -> 
    Cmd Msg
showSuggestions suggestions =
    let
        tf = suggestions
            |> List.filterMap
                (\a -> case a of
                    CandidateData data ->
                        Just data
                    SuggestionData data ->
                        Nothing
                )
    in
        if List.length tf > 0 then
            selectSearchCandidates 
                tf
                -- (getLeftPadding env)
                -- (getBottomPadding env)
                0 0
        else
            Cmd.none


getLeftPadding : { env
        | panelDimensions : PanelDimensions
        , bottomPanelOpenness : Openness
        , sidePanelOpenness : Openness
    } -> Int
getLeftPadding env = 
    if env.sidePanelOpenness == Open then 
        env.panelDimensions.leftSidebarWidth
    else
        0

getBottomPadding : { env
        | panelDimensions : PanelDimensions
        , bottomPanelOpenness : Openness
        , sidePanelOpenness : Openness
    } -> Int
getBottomPadding env =
    if env.bottomPanelOpenness == Open then
        env.panelDimensions.bottomDrawerHeaderHeight + env.panelDimensions.bottomDrawerHeight
    else 
        0











-- Config --






-- ENCODERS




encodeLocationSrc : Location -> E.Value
encodeLocationSrc loc =
    E.string 
        ( (String.fromFloat loc.x) 
        ++ "," ++ 
          (String.fromFloat loc.y) 
        )


-- DECODERS








-- Sugegstions --

type alias MagicKey =
    String


type alias Suggestion =
    { text : String
    , magicKey : MagicKey
    , isCollection : Bool
    }


type alias Suggestions =
    List Suggestion


-- type Msg
--   = GotSuggestions (Result Http.Error Suggestions)


encodeSuggestion : Suggestion -> E.Value
encodeSuggestion s =
    E.object
        [ ( "text", E.string s.text )
        , ( "magicKey", E.string s.magicKey )
        , ( "isCollection", E.bool s.isCollection )
        ]


suggestionDecoder : Decoder Suggestion
suggestionDecoder =
    D.succeed Suggestion
        |> required "text" D.string
        |> required "magicKey" D.string
        |> required "isCollection" D.bool


suggestionsDecoder : Decoder Suggestions
suggestionsDecoder =
    D.at [ "suggestions" ] (D.list suggestionDecoder) |> Dex.withDefault []



-- REQUEST METHODS


getSuggestionsUrl : { location | x : Float, y : Float } -> String -> Settings.SearchConfig -> Url
getSuggestionsUrl location search config =
    let
        searchExtent =
            config.extent
                |> encodeSearchExtent
                |> E.encode 0
                
        query =
            Qs.empty
                |> Qs.set "text" (Qs.One <| Qs.Str search)
                |> Qs.set "f" (Qs.One <| Qs.Str  "json")
                |> Qs.set "searchExtent" (Qs.One <| Qs.Str searchExtent)
                |> Qs.set "maxSuggestions" (Qs.One <| Qs.Str (String.fromInt config.maxCandidates))
                |> Qs.set "location" (Qs.One <| Qs.Str  ((String.fromFloat location.x) ++ "," ++ (String.fromFloat location.y) ) )
                |> Qs.set "countryCode" (Qs.One <| Qs.Str  "USA")
            
        qs =
            Qs.serialize Qs.config query
    in
        config.suggestUrl ++ qs






-- Find Address Candidate --


type alias MapExtent =
    { xmin : Float
    , ymin : Float
    , xmax : Float
    , ymax : Float
    }


type alias Latitude =
    Float


type alias Longitude =
    Float


type alias Location =
    { x : Longitude
    , y : Latitude
    }


type alias ImageDisplay =
    { width : Int
    , height : Int
    , dpi : Int
    }


type alias CoordX =
    Float


type alias CoordY =
    Float


type alias Coordinate =
    { x : CoordX
    , y : CoordY
    }


type alias EsriPolygon =
    { spatialReference : SpatialReference
    , rings : List Coordinate
    }


type alias SpatialReference =
    { wkid : Wkid 
    }

type alias Wkid =
    Int

-- Does not include `attributes` b/c we don't really need


type alias GPServiceFormat =
    { spatialReference : SpatialReference
    , features : List EsriPolygon
    }



-- ENCODERS


encodeLatitude : Latitude -> E.Value
encodeLatitude lat =
    E.float lat


encodeLongitude : Longitude -> E.Value
encodeLongitude lon =
    E.float lon


encodeLocation : Location -> E.Value
encodeLocation loc =
    E.object
        [ ( "x", encodeLongitude loc.x )
        , ( "y", encodeLatitude loc.y )
        ]


encodeCoordX : CoordX -> E.Value
encodeCoordX x =
    E.float x


encodeCoordY : CoordY -> E.Value
encodeCoordY y =
    E.float y


encodeCoordinate : Coordinate -> E.Value
encodeCoordinate c =
    E.object
        [ ( "x", encodeCoordX c.x )
        , ( "y", encodeCoordY c.y )
        ]


encodeWkid : Wkid -> E.Value
encodeWkid wkid =
    E.int wkid


encodeSpatialReference : Int -> E.Value
encodeSpatialReference ref =
    E.object [ ( "wkid", encodeWkid ref ) ]


encodeSearchExtent : Settings.SearchExtent -> E.Value
encodeSearchExtent extent =
    E.object
        [ ( "xmin", E.float extent.xmin )
        , ( "ymin", E.float extent.ymin )
        , ( "xmax", E.float extent.xmax )
        , ( "ymax", E.float extent.ymax )
        , ( "spatialReference", Ex.maybe encodeSpatialReference extent.spatialReference )
        ]


encodeMapExtent : { extent | xmin : Float, ymin : Float, xmax : Float, ymax : Float } -> E.Value
encodeMapExtent extent =
    E.object
        [ ( "xmin", E.float extent.xmin )
        , ( "ymin", E.float extent.ymin )
        , ( "xmax", E.float extent.xmax )
        , ( "ymax", E.float extent.ymax )
        ]


encodeImageDisplay : ImageDisplay -> E.Value
encodeImageDisplay data =
    E.object
        [ ( "width", E.int data.width )
        , ( "height", E.int data.height )
        , ( "dpi", E.int data.dpi )
        ]



-- DECODERS


decodeLatitude : Decoder Latitude
decodeLatitude =
    D.float


decodeLongitude : Decoder Longitude
decodeLongitude =
    D.float


decodeLocation : Decoder Location
decodeLocation =
    D.succeed Location
        |> required "x" decodeLongitude
        |> required "y" decodeLatitude


decodeWkid : Decoder Wkid
decodeWkid =
    D.int


decodeSpatialReference : Decoder SpatialReference
decodeSpatialReference =
    D.succeed SpatialReference
        |> required "wkid" decodeWkid



decodeMapExtent : Decoder MapExtent
decodeMapExtent =
    D.succeed MapExtent
        |> required "xmin" D.float
        |> required "ymin" D.float
        |> required "xmax" D.float
        |> required "ymax" D.float


decodeImageDisplay : Decoder ImageDisplay
decodeImageDisplay =
    D.succeed ImageDisplay
        |> required "width" D.int
        |> required "height" D.int
        |> required "dpi" D.int



-- DEFAULTS


defaultWkid : Wkid
defaultWkid =
    4326


defaultSpatialReference : SpatialReference
defaultSpatialReference =
    { wkid = defaultWkid
    }


defaultSearchLocation : Location
defaultSearchLocation =
    { x = -111.654
    , y = 39.444
    }

defaultMapExtent : MapExtent
defaultMapExtent =
    { xmin = 36.5407867431641
    , ymin = -83.6754150390625
    , xmax = 39.4660148620605
    , ymax = -75.2312240600586
    }

type alias Candidate =
    { address : String
    , longLabel : Maybe String
    , location : Location
    , score : Float
    , extent : Settings.SearchExtent
    }


type alias Candidates =
    List Candidate


-- type Msg
--   = GotCandidates (Result Http.Error Candidates)


encodeCandidate : Candidate -> E.Value
encodeCandidate c =
    E.object
        [ ( "address", E.string c.address )
        , ( "location", encodeLocation c.location )
        , ( "score", E.float c.score )
        , ( "extent", encodeMapExtent c.extent )
        ]


encodeCandidates : Candidates -> Int -> Int -> E.Value
encodeCandidates candidates paddingLeft paddingBottom =
    E.object 
        [ ( "candidates", E.list encodeCandidate candidates )
        , ( "paddingLeft", E.int paddingLeft )
        , ( "paddingBottom", E.int paddingBottom )
        ]


candidateDecoder : Decoder Candidate
candidateDecoder =
    D.succeed Candidate
        |> required "address" D.string
        |> optionalAt [ "attributes", "LongLabel" ] (D.maybe D.string) Nothing
        |> required "location" decodeLocation
        |> required "score" D.float
        |> required "extent" searchExtentDecoder


searchExtentDecoder : Decoder Settings.SearchExtent
searchExtentDecoder =
    D.succeed Settings.SearchExtent
        |> required "xmin" D.float
        |> required "ymin" D.float
        |> required "xmax" D.float
        |> required "ymax" D.float
        |> optional "spatialReference" (D.field "wkid" (D.maybe D.int)) Nothing

suggestionOrCandidateDecoder : Decoder SuggestionOrCandidate
suggestionOrCandidateDecoder =
    D.oneOf 
        [ D.field "suggestions" suggestionDecoder |> D.map SuggestionData
        , candidateDecoder |> D.map CandidateData
        ]

suggestionsOrCandidatesDecoder : Decoder SuggestionsOrCandidates
suggestionsOrCandidatesDecoder =
    D.oneOf 
        [ D.field "suggestions" (D.list (suggestionDecoder |> D.map SuggestionData))
        , D.field "candidates"  (D.list (candidateDecoder  |> D.map CandidateData))
        ]

candidatesDecoder : Decoder Candidates
candidatesDecoder =
    D.at [ "candidates" ] (D.list candidateDecoder)



-- REQUEST FUNCTIONS

getUrlForPlainText : { location | x : Float, y : Float } -> String ->  Settings.SearchConfig -> Url
getUrlForPlainText location searchTerm config =
    config
        |> buildCommonQueryString location
        |> addPlainTextQueryParams searchTerm
        |> Qs.serialize Qs.config 
        |> (++) config.searchUrl


getUrlForSuggestion : { location | x : Float, y : Float } -> Suggestion -> Settings.SearchConfig -> Url
getUrlForSuggestion location suggestion config =
    config
        |> buildCommonQueryString location
        |> addSuggestionQueryParams suggestion
        |> Qs.serialize Qs.config 
        |> (++) config.searchUrl


buildCommonQueryString : { location | x : Float, y : Float } -> Settings.SearchConfig -> Qs.Query
buildCommonQueryString location config =
    let
        searchExtent =
            config.extent
                |> encodeSearchExtent
                |> E.encode 0
    in
        Qs.empty
            |> Qs.set "f" (Qs.One <| Qs.Str  "json")
            |> Qs.set "searchExtent" (Qs.One <| Qs.Str searchExtent)
            |> Qs.set "maxLocations" (Qs.One <| Qs.Str (String.fromInt config.maxCandidates))
            --TODO
            |> Qs.set "location" (Qs.One <| Qs.Str  ((String.fromFloat location.x) ++ "," ++ (String.fromFloat location.y) ) )
            |> Qs.set "countryCode" (Qs.One <| Qs.Str  "USA")
            |> Qs.set "sourceCountry" (Qs.One <| Qs.Str  "USA")
            |> Qs.set "outSR" (Qs.One <| Qs.Str "3857" ) --<| String.fromInt <| Maybe.withDefault 4326 <| config.extent.spatialReference )
            |> Qs.set "outFields" (Qs.One <| Qs.Str  "LongLabel")


addPlainTextQueryParams : String -> Qs.Query -> Qs.Query
addPlainTextQueryParams searchTerm qs =
    Qs.set "singleLine" (Qs.One <| Qs.Str searchTerm) qs


addSuggestionQueryParams : Suggestion -> Qs.Query -> Qs.Query
addSuggestionQueryParams suggestion qs =
    qs
        |> addPlainTextQueryParams suggestion.text
        |> Qs.set "magicKey" (Qs.One <| Qs.Str suggestion.magicKey)









type CandidateForResponseType
    = ForSuggestion
    | ForCandidate



-- API --
getCandidatesFor : CandidateForResponseType -> String -> Cmd Msg
getCandidatesFor candidateForResponseType url =
    let
        get : Url -> Http.Body -> Cmd Msg
        get theUrl body =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Accept" "application/json, text/javascript, */*; q=0.01" ]
                , url = theUrl
                , body = body
                , expect = Http.expectJson ( RemoteData.fromResult >> (Response >> Internal)) ( suggestionsOrCandidatesDecoder)
                , timeout = Nothing
                , tracker = Nothing
                }
    in
        get url Http.emptyBody


getSuggestions : String -> Cmd Msg
getSuggestions url =
    let
        get : Url -> Http.Body -> Cmd Msg
        get theUrl body =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Accept" "application/json, text/javascript, */*; q=0.01" ]
                , url = theUrl
                , body = body
                , expect = Http.expectJson (RemoteData.fromResult >> (Response >> Internal)) ( suggestionsOrCandidatesDecoder )
                , timeout = Nothing
                , tracker = Nothing
                }
    in
        get url Http.emptyBody


