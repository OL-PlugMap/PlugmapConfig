port module PlugMap.Core.Ports exposing (..)

import Json.Encode as E
import Json.Decode as D


-- COMMAND PORTS (OUTBOUND)


port renderMapCmd : E.Value -> Cmd msg


port destructMapCmd : () -> Cmd msg


port setServicesCmd : E.Value -> Cmd msg


port fitToWktCmd : E.Value -> Cmd msg


port highlightCmd : E.Value -> Cmd msg


port unhighlightCmd : E.Value -> Cmd msg


port clearHighlightLayerCmd : () -> Cmd msg


port updateLayersFiltersCmd : E.Value -> Cmd msg


port refreshMap : () -> Cmd msg



-- Inbound ports


port extentUpdated : (E.Value -> msg) -> Sub msg