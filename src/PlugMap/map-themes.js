"use strict";

import WKT from "ol/format/WKT";
import LayerGroup from "ol/layer/Group";
import {
  findLayerByName,
  findAllLayersByName,
  findLayerGroupByName,
  hideGroupRem,
  unhideGroupRem,
  getVisibleMapThemeNamesWithOpacity,
  getRenderedFeatures
} from "./map-helpers.js";

export function showLayerByNameRecurs(layerName, opacity, inLayers, level) {
  let layers = findAllLayersByName(layerName, inLayers);
  let op = parseFloat(opacity);

  if (isNaN(op) || op >= 1.0 || op <= 0.0) {
    op = 1.0;
  }
  for (let layer of layers) setOpacityRecurs(layer, op, 5);
  for (let layer of layers) setVisibleRecurs(layer, true, 5);
}

export function setOpacityRecurs(layer, opacity, level) {
  if (level < 0) return;

  if (layer instanceof LayerGroup) {
    for (let nestedLyr of layer.getLayers().getArray()) {
      setOpacityRecurs(nestedLyr, opacity, level - 1);
    }
  }

  layer.setOpacity(opacity);
}

export function setVisibleRecurs(layer, visible, level) {
  if (level < 0) return;

  if (layer instanceof LayerGroup) {
    for (let nestedLyr of layer.getLayers().getArray()) {
      setVisibleRecurs(nestedLyr, visible, level - 1);
    }
  }

  layer.setVisible(!!visible);
}

/**
 * Sets visibility to true for a layer specified by its name.
 * scope - OL Map
 * @param {String} layerName
 * @param {Float} opacity - optional
 */
export function showLayerByName(layerName, opacity) {
  showLayerByNameRecurs(layerName, opacity, this.getLayers());
}

/**
 * Get the names and opacities of visible map themes.
 * scope - OL Map
 * @return {Array}
 */
export function getVisibleMapThemeDataForPrint() {
  return getVisibleMapThemeNamesWithOpacity(this.getLayers());
}

/**
 * Get a list of rendered features as wkt strings.
 * scope - OL Map
 * @return {Array}
 */
export function getRenderedFeaturesAsWkt() {
  let wkt = new WKT();
  let features = getRenderedFeatures(this.getLayers());
  const wkts = features.map(feature => wkt.writeFeature(feature));
  return wkts;
}

/**
 * Sets visibility to false for a layer specified by its name.
 * scope - OL Map
 * @param {String} layerName
 */
export function hideLayerByName(layerName) {
  let layer = findLayerByName(layerName, this.getLayers());
  if (layer) {
    layer.setVisible(false);
  }
}

/**
 * Set the transparency of all layers that are part of a specific category.
 * scope - OL Map
 * @param {Float} transparency
 * @param {String} category
 */
export function setTransparencyByCategoryName(transparency, category) {
  if (!(typeof transparency === "number") || !(typeof category === "string"))
    return;
  if (transparency > 1.0) transparency = 1.0;
  if (transparency < 0.0) transparency = 0.0;
  let lg = findLayerGroupByName(category, this.getLayers());
  if (!lg) return;
  lg.setOpacity(transparency);
}

export function hideLayersRem() {
}

export function unhideLayersRem() {
}
