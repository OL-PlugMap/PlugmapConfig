"use strict";

import * as Sphere from "ol/sphere";
import * as Feature from "ol/Feature";
import Polygon from "ol/geom/Polygon";
import MultiPolygon from "ol/geom/MultiPolygon";
import { get } from "ol/proj";
import { getWidth } from "ol/extent";
import ColorConvert from "color-convert";
import LayerGroup from "ol/layer/Group";
import TileLayer from "ol/layer/Tile";
import ImageLayer from "ol/layer/Image";
import VectorLayer from "ol/layer/Vector";
import XYZ from "ol/source/XYZ";
import Dynamic from "ol/source/ImageArcGISRest";
import BingMaps from "ol/source/BingMaps";
import WMTS from "ol/source/WMTS";
import WMTS_TileGrid from "ol/tilegrid/WMTS";

/**
 * Return the layer with the given name from the list of given layers.
 * @param {String} layerName
 * @param {ol.Collection} layers
 * @return {Layer || LayerGroup}
 */
export function findLayerByName(layerName, layers) {
  if (!(typeof layerName === "string") || !layers) return undefined;
  for (let lyr of layers.getArray()) {
    if (layerName === lyr.get("name")) {
      return lyr;
    } else if (lyr instanceof LayerGroup) {
      for (let nestedLyr of lyr.getLayers().getArray()) {
        if (layerName === nestedLyr.get("name")) {
          return nestedLyr;
        }
      }
    }
  }
}

/**
 * Return the layers with the given name from the list of given layers.
 * @param {String} layerName
 * @param {ol.Collection} layers
 * @return {Layer || LayerGroup}
 */
export function findAllLayersByName(layerName, layers, level) {
  if (level == undefined) level = 5;

  let ret = [];

  if (level >= 0 && typeof layerName === "string" && layers) {
    for (let lyr of layers.getArray()) {
      if (layerName === lyr.get("name")) {
        ret.push(lyr);
      } else if (lyr instanceof LayerGroup) {
        ret = ret.concat(
          findAllLayersByName(layerName, lyr.getLayers(), level - 1)
        );
      }
    }
  }
  return ret;
}


/**
 * Return the layers with the given name from the list of given layers.
 * @param {String} id
 * @param {ol.Collection} layers
 * @return {Layer || LayerGroup}
 */
export function findAllLayersById(id, layers, level) {
  if (level == undefined) level = 5;

  let ret = [];

  if (level >= 0 && typeof id === "string" && layers) {
    for (let lyr of layers.getArray()) {
      if (id === lyr.get("id")) {
        ret.push(lyr);
      } else if (lyr instanceof LayerGroup) {
        ret = ret.concat(
          findAllLayersById(id, lyr.getLayers(), level - 1)
        );
      }
    }
  }
  return ret;
}

/**
 * Return a list of visible Map Theme layer names and opacity.
 * @param {ol.Collection} layers
 */
export function getVisibleMapThemeNamesWithOpacity(layers) {
  let visibleLayers = [];
  for (let lyr of layers.getArray()) {
    let opacity = lyr.getOpacity();
    if (lyr instanceof LayerGroup) {
      for (let nestedLyr of lyr.getLayers().getArray()) {
        if (nestedLyr.getVisible() && nestedLyr.get("usage") === "map-themes") {
          if (nestedLyr.get("layers")) {
            //this is for combined layer (Ignitions & Fire Occurrence Density in utwrap)

            if (
              nestedLyr
                .get("layers")
                .getArray()[0]
                .get("visible") == true
            ) {
              visibleLayers.push({
                name: nestedLyr.get("name"),
                opacity: opacity
              });
            }
          } else {
            visibleLayers.push({
              name: nestedLyr.get("name"),
              opacity: opacity
            });
          }
        }
      }
    } else if (lyr.getVisible() && lyr.get("usage") === "map-themes") {
      visibleLayers.push({
        name: lyr.get("name"),
        opacity: opacity
      });
    }
  }
  return visibleLayers;
}

/**
 * Return a list of all visible features.
 * @param {ol.Collection} layers
 */
export function getRenderedFeatures(layers) {
  let features = [];
  for (let lyr of layers.getArray()) {
    if (lyr instanceof LayerGroup) {
      for (let nestedLyr of lyr.getLayers().getArray()) {
        if (nestedLyr.getVisible() && nestedLyr instanceof VectorLayer) {
          let feats = nestedLyr.getSource().getFeatures();
          features = [...features, ...feats];
        }
      }
    } else if (lyr.getVisible() && lyr instanceof VectorLayer) {
      let feats = lyr.getSource().getFeatures();
      features = [...features, ...feats];
    }
  }
  return features;
}

/**
 * Return the layer group with the given name from the list of given layers.
 * @param {String} layerGroupName
 * @param {ol.Collection} layers
 * @return {LayerGroup}
 */
export function findLayerGroupByName(layerGroupName, layers) {
  if (!(typeof layerGroupName === "string") || !layers) return undefined;
  for (let lyr of layers.getArray()) {
    if (lyr instanceof LayerGroup) {
      if (layerGroupName === lyr.get("name")) {
        return lyr;
      }
    }
  }
  return undefined;
}

/**
 * Creates a new icon given a font-awesome classname like `fa-x`.
 * @param {String} className
 * @return {DOMElement} <i class="fa className"></i>
 */
export function createFaLabel(className) {
  let lbl = document.createElement("i");
  lbl.className = `fa ${className}`;
  return lbl;
}

/**
 * Creates a new XYZ tile layer
 * @param {String} url
 * @param {Int} maxZoom
 * @param {Int} minZoom
 * @param {String} name
 * @return {TileLayer}
 */
export function newXYZ(url, maxZoom, minZoom, name, attribution) {
  let logos = [];
  if (attribution != undefined)
    for (let idx in attribution.logos) {
      let logo = attribution.logos[idx];

      //Logos must have a path for the image
      if (!logo.path) continue;

      //Where did my "".format function go
      //I swear it was here
      let format = "$0$1$2";

      let linkPre = "";
      let linkPost = "";

      if (logo.link) {
        linkPre = "<a href='$1'>".replace("$1", attribution.logos[idx].link);
        linkPost = "</a>";
      }

      let image = "<img class='$0' src='$1' /></a>"
        .replace("$1", logo.path)
        .replace("$0", logo.class ? logo.class : "attributionLogo");

      logos.push(
        format
          .replace("$0", linkPre)
          .replace("$1", image)
          .replace("$2", linkPost)
      );
    }
  let attributionTexts = [];
  if (attribution)
    if (attribution.texts)
      attributionTexts = attributionTexts.concat(attribution.texts);

  if (logos.length > 0) attributionTexts = attributionTexts.concat(logos);

  let xyz = new TileLayer({
    visible: false,
    preload: 4,
    source: new XYZ({
      crossOrigin: "anonymous",
      url: url,
      maxZoom: maxZoom,
      minZoom: minZoom,
      tileLoadFunction: (imageTile, src) => {
        imageTile.getImage().src = src;
      },
      attributions: attributionTexts.length ? attributionTexts : null
    })
  });
  xyz.set("name", name);
  return xyz;
}

export function newDynamic(
  url,
  boundingBox,
  layerToShow,
  layerDefs,
  useToken,
  tokenKey
) {
  let customParams = {};

  if (useToken == undefined) useToken = true;

  if (boundingBox !== undefined) {
    customParams["BBOX"] = boundingBox;
  }
  if (layerToShow !== undefined) {
    customParams["LAYERS"] = layerToShow;
  }
  if (layerDefs !== undefined) {
    customParams["LAYERDEFS"] = layerDefs;
  }

  if (useToken) {
    if (tokenKey == undefined) {
      customParams["token"] = wrap.agsToken;
    } else {
      //TODO: Pull from tokens
      if (wrap.ags) {
        customParams["token"] = wrap.ags[tokenKey];
      }

      //Fallback because the token key wasnt found
      if (!customParams["token"]) {
        customParams["token"] = wrap.agsToken;
      }
    }
  }

  let dynamicLayer = new ImageLayer({
    visible: false,
    preload: 4,
    source: new Dynamic({
      crossOrigin: "anonymous",
      ratio: 1,
      params: customParams,
      url: url[0],
      imageLoadFunction: (image, src) => {
        image.getImage().src = src;
      }
    })
  });
  dynamicLayer.set("name", name);
  return dynamicLayer;
}

/**
 * Creates a new Bing Maps tile layer
 * @param {String} api_key
 * @param {String} imagerySet
 * @param {Int} maxZoom
 * @param {String} name
 * @return {TileLayer}
 */
export function newBing(api_key, imagerySet, maxZoom, name) {
  let bing = new TileLayer({
    visible: false,
    preload: 4,
    source: new BingMaps({
      key: api_key,
      imagerySet: imagerySet,
      maxZoom: maxZoom
    })
  });
  bing.set("name", name);
  return bing;
}

/**
 * Creates a new WMTS tile layer
 * @param {String} url
 * @param {Array Float} ext - extent
 * @param {String} name
 * @param {Object} mapProxy
 * @return {TileLayer}
 */
export function newWMTS(url, ext, name, mapProxy, maxZoom) {
  let projection = get("EPSG:3857"),
    projectionExtent = projection.getExtent(),
    size = getWidth(projectionExtent) / 256,
    zooms = maxZoom + 1,
    resolutions = new Array(zooms),
    matrixIds = new Array(zooms);
  for (let z = 0; z < zooms; ++z) {
    resolutions[z] = size / Math.pow(2, z);
    matrixIds[z] = z;
  }

  let wmts = new TileLayer({
    visible: false,
    preload: 4,
    source: new WMTS({
      crossOrigin: "anonymous",
      url: `${mapProxy.root}${url}`,
      matrixSet: "webmercator",
      format: "image/png",
      projection: projection,
      requestEncoding: "REST",
      tileGrid: new WMTS_TileGrid({
        extent: ext,
        resolutions: resolutions,
        matrixIds: matrixIds
      }),
      style: "default",
      tileLoadFunction: (imageTile, src) => {
        imageTile.getImage().src = `${src}?token=${mapProxy.token}`;
      }
    })
  });
  wmts.set("name", name);
  return wmts;
}

/*************************************************************************
 * GIS functions thanks to Jonatas Walker and this thread
 * https://gis.stackexchange.com/a/151276
 *************************************************************************/

/**
 * All coordinates expected EPSG:4326
 * @param {Array} start Expected [lon, lat]
 * @param {Array} end Expected [lon, lat]
 * @return {number} Distance - meter.
 */
export function calculateDistance(start, end) {
  let lat1 = parseFloat(start[1]),
    lon1 = parseFloat(start[0]),
    lat2 = parseFloat(end[1]),
    lon2 = parseFloat(end[0]);

  return sphericalCosinus(lat1, lon1, lat2, lon2);
}

/**
 * All coordinates expected EPSG:4326
 * @param {number} lat1 Start Latitude
 * @param {number} lon1 Start Longitude
 * @param {number} lat2 End Latitude
 * @param {number} lon2 End Longitude
 * @return {number} Distance - meters.
 */
export function sphericalCosinus(lat1, lon1, lat2, lon2) {
  const radius = 6371e3; // meters
  let dLon = toRad(lon2 - lon1),
    lat1r = toRad(lat1),
    lat2r = toRad(lat2),
    distance =
      Math.acos(
        Math.sin(lat1r) * Math.sin(lat2r) +
          Math.cos(lat1r) * Math.cos(lat2r) * Math.cos(dLon)
      ) * radius;

  return distance;
}

/**
 * Creates a new lon-lat coordinate
 * @param {Array} coord Expected [lon, lat] EPSG:4326
 * @param {number} bearing Bearing in degrees
 * @param {number} distance Distance in meters
 * @return {Array} Lon-lat coordinate.
 */
export function createCoord(coord, bearing, distance) {
  /** http://www.movable-type.co.uk/scripts/latlong.html
   * φ is latitude, λ is longitude,
   * θ is the bearing (clockwise from north),
   * δ is the angular distance d/R;
   * d being the distance travelled, R the earth’s radius*
   **/
  const radius = 6371e3; // meters
  let δ = Number(distance) / radius, // angular distance in radians
    θ = toRad(Number(bearing)),
    φ1 = toRad(coord[1]),
    λ1 = toRad(coord[0]);

  let φ2 = Math.asin(
    Math.sin(φ1) * Math.cos(δ) + Math.cos(φ1) * Math.sin(δ) * Math.cos(θ)
  );

  let λ2 =
    λ1 +
    Math.atan2(
      Math.sin(θ) * Math.sin(δ) * Math.cos(φ1),
      Math.cos(δ) - Math.sin(φ1) * Math.sin(φ2)
    );
  // normalise to -180..+180°
  λ2 = (λ2 + 3 * Math.PI) % (2 * Math.PI) - Math.PI;

  return [toDeg(λ2), toDeg(φ2)];
}

/**
 * All coordinates expected EPSG:4326
 * @param {Array} start Expected [lon, lat]
 * @param {Array} end Expected [lon, lat]
 * @return {number} Bearing in degrees.
 */
export function getBearing(start, end) {
  let startLat = toRad(start[1]),
    startLong = toRad(start[0]),
    endLat = toRad(end[1]),
    endLong = toRad(end[0]),
    dLong = endLong - startLong;

  let dPhi = Math.log(
    Math.tan(endLat / 2.0 + Math.PI / 4.0) /
      Math.tan(startLat / 2.0 + Math.PI / 4.0)
  );

  if (Math.abs(dLong) > Math.PI) {
    dLong = dLong > 0.0 ? -(2.0 * Math.PI - dLong) : 2.0 * Math.PI + dLong;
  }

  return (toDeg(Math.atan2(dLong, dPhi)) + 360.0) % 360.0;
}

/**
 * Translates from radians to degrees.
 * @param {radian} n
 * @return {Number}
 */
export function toDeg(n) {
  return n * 180 / Math.PI;
}

/**
 * Translates from degrees to radians.
 * @param {degree} n
 * @return {Number}
 */
export function toRad(n) {
  return n * Math.PI / 180;
}

/**
 * Translates an RGB value to Hex value.
 * @param {Array Int} rgb
 * @return {String}
 */
export function rgbToHex(rgb) {
  let toHex = c => {
    let hex = c.toString(16);
    return hex.length == 1 ? "0" + hex : hex;
  };
  return `#${toHex(rgb[0])}${toHex(rgb[1])}${toHex(rgb[2])}`;
}

/**
 * Translates a Hex value to LAB value.
 * @param {String} hex
 * @return {Object}
 */
export function hexToLab(hex) {
  if (!hex) return undefined;
  let labArr = ColorConvert.hex.lab(hex.replace("#", ""));
  let lab = {
    L: labArr[0],
    A: labArr[1],
    B: labArr[2]
  };
  return lab;
}

/**
 * Translates a Hex value to RGBA value.
 * @param {String} hex
 * @return {Object}
 */
export function hexToRGBA(hex) {
  if (!hex) return undefined;
  let labArr = ColorConvert.hex.rgba(hex.replace("#", ""));
  let rgb = {
    r: labArr[0],
    g: labArr[1],
    b: labArr[2],
    a: labArr[3]
  };
  return lab;
}

/**
 * Converts meters to acres.
 * @param {Float} area
 */
export function metersToAcres(area) {
  if (isNaN(parseFloat(area))) {
    return 0;
  } else {
    return Math.round(area / 4046.85642 * 100) / 100;
  }
}

/**
 * Calculate the acreage of a polygonal feature. Assumes
 * initial projection of EPSG:3857 - Web Mercator and will
 * reproject to EPSG:4326.
 * @param {Feature} feature
 * @return {Float} totalAcreage
 */
export function calculateAcreage(feature) {
  if (!(feature instanceof Feature)) {
    throw new Error("Must provide a valid OL Feature");
  }
  let geometry = feature.getGeometry();
  if (!(geometry instanceof Polygon) && !(geometry instanceof MultiPolygon)) {
    throw new Error(
      "Must provide a Feature with geometry type of Polygon or MultiPolygon"
    );
  }

  // prepare the geometry for area calculation
  let clone = geometry.clone();
  clone.transform("EPSG:3857", "EPSG:4326");
  let wgs84sphere = new Sphere(6378137);

  let totalArea = 0;
  if (clone.getType() === "Polygon") {
    totalArea = wgs84sphere.geodesicArea(clone.getCoordinates()[0]);
    totalArea = Math.abs(totalArea);
  } else if (clone.getType() === "MultiPolygon") {
    totalArea = clone.getCoordinates().reduce((acc, val) => {
      let area = wgs84sphere.geodesicArea(val[0]);
      return acc + Math.abs(area);
    }, 0);
  }
  let totalAcreage = metersToAcres(totalArea);
  return totalAcreage;
}
