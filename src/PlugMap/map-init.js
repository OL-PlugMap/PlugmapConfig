"use strict";

import { fromLonLat } from "ol/proj";
import Map from "ol/Map";
import View from "ol/View";
import { format } from "ol/coordinate";
import LayerGroup from "ol/layer/Group";
import FullScreen from "ol/control/FullScreen";
import ZoomToExtent from "ol/control/ZoomToExtent";
import ScaleLine from "ol/control/ScaleLine";
import OverviewMap from "ol/control/OverviewMap";
import MousePosition from "ol/control/MousePosition";
import DragRotateAndZoom from "ol/interaction/DragRotateAndZoom";
import {
  createFaLabel,
  newXYZ,
  newDynamic,
  newBing,
  newWMTS
} from "./map-helpers.js";

/**
 * Initialize new OpenLayers map for Pro and Public viewer.
 *
 * @param {Object} mapOptions - from yaml but run through Elm's type system first (probably unnecessary)
 * @param {Object} flags - from yaml, straight js object w/ feature config data etc
 * @returns MapData - {
 *  options - mapOptions,
 *  flags - flags,
 *  map - OL Map,
 *  view - OL View,
 *  extent - extent of map,
 *  layers - reference to map layers,
 *  controls - reference to map controls,
 *  interactions - reference to map interactions,
 *  targetLayerLegendName - used for layer legend,
 * }
 */
export function destructMap(data) {
  if (data.map) data.map.setTarget(null);
  data.map = null;
}

export function bootMap(mapOptions, flags) {
  let data = {
    options: mapOptions,
    flags: flags,
    view: null,
    map: null,
    extent: null,
    layers: null,
    controls: null,
    interactions: null,
    targetLayerLegendName: "",
    swipeHandler: null
  };

  // pre-render initializations
  _initView(data);
  //_initExtent(data);
  //_initLayers(data);
  _initMap(data);

  // wait til next frame to attempt rendering the map
  // ie: target div needs to exist before rendering
  // let requestAnimationFrame =
  //   window.requestAnimationFrame ||
  //   window.mozRequestAnimationFrame ||
  //   window.webkitRequestAnimationFrame ||
  //   window.msRequestAnimationFrame;
  // requestAnimationFrame(() => {
  //   if (document.getElementById(data.options.target) === null) {
  //     throw new Error(
  //       `Map target ${data.options.target} cannot be found. Render failed...`
  //     );
  //   }
  //   _renderMap(data);
  // });
  attemptRender(data);

  sessionStorage.setItem("mapOptions", JSON.stringify(mapOptions));
  sessionStorage.setItem("mapFlags", JSON.stringify(flags));

  return data;
}

function attemptRender(data)
{
  console.log("Attempting to render map")
  let requestAnimationFrame =
    window.requestAnimationFrame ||
    window.mozRequestAnimationFrame ||
    window.webkitRequestAnimationFrame ||
    window.msRequestAnimationFrame;
  requestAnimationFrame(() => {
    if (document.getElementById(data.options.target) === null) {
      console.warn("Element doesnt exist. Waiting longer");
      // throw new Error(
      //   `Map target ${data.options.target} cannot be found. Render failed...`
      // );
      setTimeout(attemptRender, 1000, data);
    }
    else
    {
      _renderMap(data);
      checkInterval = setInterval(mapCheck, 1000, data);
    }
  });
}

let rendered = false;
let checkInterval = undefined;

function mapCheck(data)
{
  //console.log("Checking if map is still there ...");
  if(rendered)
  {
    var ele = document.getElementById(data.options.target);
    if (ele === null || ele.children.length == 0) {
      console.log("Element is gone now ... restarting soon ...");
      console.log(document.getElementById(data.options.target));
      clearInterval(checkInterval);
      checkInterval = undefined;
      destructMap(data);
      setTimeout(bootMap, 1000, data.options, data.flags);
      rendered = false;
    }
    else
    {
      //console.log("... its still there");
      //console.log(document.getElementById(data.options.target));
    }
  }
  else
  {
    //console.log("Map not rendered ...");
  }
}

/**
 * Initializes a new OL Map View
 * @param {MapData} data
 * @returns MapData
 */
function _initView(data) {  
    var resolutionX =  window.screen.width * window.devicePixelRatio;
    var resolutionY =  window.screen.height * window.devicePixelRatio;
    var adjustedZoom = 0;
    if (resolutionX == 1280 && resolutionY == 720){
      adjustedZoom = -1;
    }
    
  data.view = new View({
    center: fromLonLat(data.options.center),
    rotation: 0,
    zoom: data.options.zoom + adjustedZoom,
    maxZoom: data.options.maxZoom,
    minZoom: data.options.minZoom
  });

  return data;
}

/**
 * Initializes the default extent for the map.
 * @param {MapData} data
 * @returns MapData
 */
function _initExtent(data) {
  if (data.flags.overrideExtents) {
    // in the case of the print page, we will override the configured extents
    data.extent = data.flags.overrideExtents;
  } else {
    // extents should be coming from south.themes.yaml
    let extents = data.flags.extents;
    // wrap.organizationName should match an extent key (ie: virginia) and is being set in Map/Index.cshtml
    // let targetExtent =
    //   extents[(wrap.organizationName || "default").toLowerCase()];
    // if (!targetExtent) targetExtent = extents["default"];
    data.extent = null; //targetExtent; //TODO: Pull this in from init
  }

  return data;
}

/**
 * Initializes basemaps, layers, and reference map themes
 * @param {MapData} data
 * @returns MapData
 */
function _initLayers(data) {
  // parse Basemaps, Layers, and Reference themes
  let { basemaps, layers, reference } = _parseThemes(data);

  // set up layer groups
  let layersLG = new LayerGroup({
    layers: layers
  });
  layersLG.set("name", "layers");

  let referenceLG = new LayerGroup({
    layers: reference
  });
  referenceLG.set("name", "reference");

  data.layers = []
    .concat(basemaps)
    .concat(layersLG)
    .concat(referenceLG);

  //data.layers = [basemaps];
  return data;
}

/**
 * Internal method to initialize a new OL map
 * @param {MapData} data
 * @returns MapData
 */
function _initMap(data) {
  // map controls are added after map gets a target
  let map = new Map({
    layers: data.layers,
    view: data.view,
    loadTilesWhileAnimating: true
  });

  // Add pointer move event handlers for pixel color data collection
  // Canvas + Layer pixel data do not always match, I think b/c of
  // some blending that the OL rendering engine does? Send both rgb
  // values to Elm to improve accuracy. If there's a better way...
  let getPixelData = evt => {
    // get mouse position
    let position = map.getEventPixel(evt.originalEvent);

    // tell elm which layer legend to show based on
    // swipe state and mouse position
    if (data.swipeHandler && data.swipeHandler.info) {
      let info = data.swipeHandler.info;
      switch (info.state) {
        case "inactive":
          map.set("targetlayerlegend", "theme");
          break;
        case "horizontal":
          if (position[1] >= info.data.position.y) {
            map.set("targetlayerlegend", "swipe");
          } else {
            map.set("targetlayerlegend", "theme");
          }
          break;
        case "vertical":
          if (position[0] >= info.data.position.x) {
            map.set("targetlayerlegend", "swipe");
          } else {
            map.set("targetlayerlegend", "theme");
          }
          break;
        case "keyhole":
          //On this one we need to see if the mouse position is within the radius of the center of the circle
          let CircleCenter = {
            x: info.data.position.x,
            y: info.data.position.y
          };
          let CircleRadius = 100; //TODO: Dont hard code this
          let MousePosition = { x: position[0], y: position[1] };
          //We need to get the distance between the mouse and the cirlce center.
          //a^2+b^2=c^2
          let DeltaX = CircleCenter.x - MousePosition.x;
          DeltaX *= DeltaX;

          let DeltaY = CircleCenter.y - MousePosition.y;
          DeltaY *= DeltaY;

          let TargetMaxDistanceSquared = CircleRadius * CircleRadius;

          let DistanceSquared = DeltaX + DeltaY;

          if (DistanceSquared < TargetMaxDistanceSquared) {
            map.set("targetlayerlegend", "swipe");
          } else {
            map.set("targetlayerlegend", "theme");
          }

          break;
        default:
          map.set("targetlayerlegend", "theme");
          break;
      }
    } else {
      map.set("targetlayerlegend", "theme");
    }

    // get canvas pixel rgb values
    let ctx = map.getRenderer().canvas_.getContext("2d");
    let pixel = ctx.getImageData(position[0], position[1], 1, 1);
    let canvasRgb = { r: pixel.data[0], g: pixel.data[1], b: pixel.data[2] };

    // get layer pixel rgb values
    let layerRgb = null;
    map.forEachLayerAtPixel(
      evt.pixel,
      (layer, pixel) => {
        if (pixel) {
          layerRgb = { r: pixel[0], g: pixel[1], b: pixel[2] };
        } else {
          layerRgb = null;
        }
      },
      this,
      layer => {
        return layer.get("name") === data.targetLayerLegendName;
      },
      this
    );

    map.set("colorunderpointer", [layerRgb, canvasRgb]);
  };
  //map.on("pointermove", throttle(getPixelData, 100));

  data.map = map;

  return this;
}

/**
 * Initial render of the OL Map
 * @param {MapData} data
 * @returns MapData
 */
function _renderMap(data) {
  data.map.setTarget(data.options.target);
  rendered = true;

  if (!data.extent) {
    data.extent = data.map.getView().calculateExtent(data.map.getSize());
  }
  try {
    data.map.getView().fit(data.extent, {
      size: data.map.getSize(),
      nearest: true,
      duration: 1000
    });
  } catch (err) {
    console.error("Error occured fitting map with its size and extent: ", err);
  } finally {
    return data;
  }
}

/**
 * Initializes OL Map Controls
 * @param {MapData} data
 * @returns MapData
 */
function _initControls(data) {
  let mapcontrols = data.flags.mapcontrols;
  let controls = [];

  if (mapcontrols.zoomtoextent) {
    let ctrl = new ZoomToExtent({
      label: createFaLabel("fa-arrows-alt"),
      extent: data.extent
    });
    ctrl.set("name", "zoomtoextent");
    controls.push(ctrl);
  }

  if (mapcontrols.fullscreen) {
    let ctrl = new FullScreen({
      label: createFaLabel("fa-window-maximize"),
      labelActive: createFaLabel("fa-window-close-o")
    });
    ctrl.set("name", "fullscreen");
    controls.push(ctrl);
  }

  if (mapcontrols.overviewmap) {
    let ctrl = new OverviewMap({
      target: "overview-map-container",
      label: createFaLabel("fa-expand"),
      collapseLabel: createFaLabel("fa-compress")
    });
    ctrl.set("name", "overviewmap");
    controls.push(ctrl);
  }

  if (mapcontrols.scaleline) {
    let ctrl = new ScaleLine({
      target: "scale-line-container",
      units: "us"
    });
    ctrl.set("name", "scaleline");
    controls.push(ctrl);
  }

  if (mapcontrols.mouseposition) {
    let ctrl = new MousePosition({
      target: document.getElementById("mouse-pos-container"),
      projection: "EPSG:4326",
      coordinateFormat: coord => {
        return format(coord, "{y}, {x}", 4);
      }
      //coordinateFormat: coordinate.createStringXY(4)
    });
    ctrl.set("name", "mouseposition");
    controls.push(ctrl);
  }

  // add the controls to the map
  for (let ctrl of controls) {
    // data.map.addControl(ctrl); TODO:
  }

  return data;
}

/**
 * Initializes OL Map Interactions
 * @param {MapData} data
 * @return {MapData}
 */
function _initInteractions(data) {
  let mapcontrols = data.flags.mapcontrols;
  let interactions = [];

  // if (mapcontrols.dragrotateandzoom) {
  //   let interaction = new DragRotateAndZoom();
  //   interactions.push(interaction);
  // }

  // add the interactions to the map
  debugger;
  for (let intr of interactions) {
    data.map.addInteraction(intr);
  }

  return data;
}

/**
 * Parses Map Themes from flag data.
 * @param {MapData} data
 * @return {Object} - { basemaps, layers, reference }
 */
function _parseThemes(data) {
  let basemaps = [],
    layers = [],
    reference = [];

  data.flags.themes.forEach(item => {
    if (!item.category) return;
    switch (item.category.toLowerCase()) {
      case "basemaps":
        basemaps = basemaps.concat(_parseLayers(data, item));
        break;
      // case "layers":
      //   layers = layers.concat(_parseLayers(data, item));
      //   break;
      // case "reference":
      //   reference = reference.concat(_parseLayers(data, item));
      //   break;
      default:
        break;
    }
  });

  return { basemaps: basemaps, layers: layers, reference: reference };
}

/**
 * Parses a layers for a particular theme category. The `usage` property
 * that is set is primarily to make it easier to distinguise between
 * layers that are "Map Themes" and layers that are for displaying and
 * modifying Project Areas.
 * @param {category} data
 * @return {Array} parsed layers
 */
function _parseLayers(data, category) {
  //Dont push into this
  let layers = [];

  for (let grp of category.layerGroups) {
    for (let item of grp.layerItems) {
      if (!Array.isArray(item.url)) item.url = [item.url];

      //Push into this
      let groupLayers = [];
      for (let url of item.url) {
        let set = true;
        let newLayer = {};
        switch (item.type.toLowerCase()) {
          case "xyz":
            newLayer = newXYZ(
              url,
              item.maxZoom,
              item.minZoom,
              item.name + "_" + groupLayers.length, //This shouldnt be needed anymore
              item.attribution
            );

            break;

          case "dynamic":
            newLayer = newDynamic(
              item.url,
              item.boundingBox,
              item.layerToShow,
              item.layerDefs,
              item.useToken,
              item.tokenKey
            );
            break;

          case "bingmaps":
            newLayer = newBing(
              item.api_key,
              item.imagerySet,
              item.maxZoom,
              item.name
            );
            break;

          case "wmts":
            newLayer = newWMTS(
              url,
              item.extent,
              item.name,
              data.flags.mapProxy,
              data.options.maxZoom
            );

            break;
          default:
            set = false;
            break;
        }
        if (set) {
          newLayer.set("usage", "map-themes");
          groupLayers.push(newLayer);
        }
      }
      //If we have more than one item in this group then create a layer group for it
      if (groupLayers.length > 1) {
        let group = new LayerGroup({
          layers: groupLayers,
          name: item.name
        });
        group.set("usage", "map-themes");
        group.set("name", item.name);
        layers.push(group);
      } else if (groupLayers.length > 0) {
        //If our group only has one item then add it to the layers as is
        layers.push(groupLayers[0]);
        groupLayers[0].set("name", item.name);
      }
    }
  }

  return layers;
}
