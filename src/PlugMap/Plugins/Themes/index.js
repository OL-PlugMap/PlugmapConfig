import { Group as LayerGroup, Tile as TileLayer } from "ol/layer.js";
import XYZ from "ol/source/XYZ";
import { get } from "ol/proj";
import { getWidth } from "ol/extent";
import WMTS from "ol/source/WMTS";
import WMTSTileGrid from "ol/tilegrid/WMTS";
import ImageWMS from 'ol/source/ImageWMS.js';
import ImageLayer from "ol/layer/Image";
import { ImageArcGISRest, TileArcGISRest } from "ol/source";
import TileGrid from "ol/tilegrid/TileGrid"

import VectorTileLayer from 'ol/layer/VectorTile';
import VectorTileSource from 'ol/source/VectorTile';
import MVT from 'ol/format/MVT';

import { Fill, Stroke, Style, CircleStyle } from 'ol/style';

export default class Themes {
  constructor() {
    this.pendingConfiguration = [];
    this.core = null;
    this.lastState = {}
    this.states = [];
  }

  apply(core) {
    this.core = core;

    core.mapCmd("addThemesCmd", this.addLayerCategories.bind(this));

    core.mapCmd("toggleSelectedThemesCmd", this.toggleSelectedThemes.bind(this));

    //core.mapCmd("updateLayersFiltersCmd", this.updateLayersFilters.bind(this));

    core.mapCmd("setCategoryTransparencyCmd", this.setCategoryTransparency.bind(this));

    //core.mapCmd("save", this.save.bind(this));
    //core.mapCmd("revert", this.revert.bind(this));

    core.on("setServicesCmd", this.processPending.bind(this));
  }

  save() {
    this.states.push(this.lastState);
  }

  revert() {
    this.toggleSelectedThemes(this.states.pop())
  }


  processPending() {
    let self = this;
    this.pendingConfiguration.forEach(item => {
      item.fn.apply(self, item.params);
    });
  }

  init() { }

  addLayerCategories(categories) {
    let self = this;
    let core = this.core;
    let map = core.getMap();
    let groups = categories.map(category => {
      // build out all the layers, none will be visible yet
      let layers = category.layers.map(layer => {
        return self.makeLayer.call(self, layer);
      });

      // show layers that are part of current selection
      // and hide ones that are not part of current selection
      self.setLayerVisibilities(category.selection, layers);

      // group category layers into a layer group
      let group = new LayerGroup({
        opacity: category.opacity,
        layers: layers
      });
      group.set('id', category.category_key);
      return group;
    });
    groups.forEach(group => map.addLayer(group));
  }

  // updateLayersFilters(categories) {



  //   let self = this;
  //   let map = this.core.getMap();

  //   var allLayersFlat = map
  //     .getLayers()
  //     .getArray()

  //   var allLayersDict = {};

  //   allLayersFlat.forEach(layer => {
  //     allLayersDict[layer.get('id')] = layer;
  //     if (layer.getLayers) {
  //       layer
  //         .getLayers()
  //         .getArray()
  //         .forEach(l => {
  //           allLayersDict[l.get('id')] = l;
  //         });
  //     }
  //   })

  //   categories.forEach(data => {
  //     data.filters.forEach(layer => {
  //       var lyr = allLayersDict[layer.key];
  //       if (lyr) {
  //         var newdef = "";
  //         var olddef = "";

  //         var src = lyr.getSource()
  //         if (layer.values && layer.values.length > 0) {
  //           if (src.applyFilters) {
  //             src.applyFilters(layer);
  //           }
  //           else {
  //             var def = {};
  //             def[layer.layerid] = layer.filter
  //             def = JSON.stringify(def);

  //             olddef = src.params_["layerDefs"];
  //             newdef = def;

  //             src.params_["layerDefs"] = def;
  //           }
  //         }
  //         else {
  //           if (src.clearFilters) {
  //             src.clearFilters(layer);
  //           }
  //           else {
  //             var def = {};
  //             def[layer.layerid] = "1=0";
  //             def = JSON.stringify(def);

  //             olddef = src.params_["layerDefs"];
  //             newdef = def;

  //             src.params_["layerDefs"] = def;
  //           }
  //         }

  //         if (olddef != newdef) {
  //           if (src.tileCache) {
  //             src.tileCache.clear();
  //           }
  //           src.changed();
  //         }
  //       }
  //       else {
  //       }
  //     })
  //   })
  // }

  toggleSelectedThemes(data) {
    if (!data)
      return;
    let self = this;
    let map = this.core.getMap();
    data.forEach(datum => {
      let category =
        map
          .getLayers()
          .getArray()
          .find(l => l.get('id') === datum.category_key);

      let layers = category.getLayers().getArray();

      self.setLayerVisibilities(datum.selection, layers);
    });

    this.lastState = data;
  }

  setLayerVisibilities(selection, layers) {
    let toggleLayer = function (layer, isMatch) {
      if (layer instanceof LayerGroup) {
        layer.setVisible(isMatch);
        layer.getLayers().getArray().forEach(child => child.setVisible(isMatch));
      } else {
        layer.setVisible(isMatch);
      }
    };

    switch (selection.selection_type) {
      case "monoselection":
        layers.forEach(layer => {
          toggleLayer(layer, selection.selection_key === layer.get('id'));
        });
        break;

      case "polyselection":
        layers.forEach(layer => {
          toggleLayer(layer, selection.selection_keys.includes(layer.get('id')));
        });
        break;

      default:
        break;
    }
  }

  setCategoryTransparency(data) {
    let map = this.core.getMap();
    let category =
      map
        .getLayers()
        .getArray()
        .find(l => l.get('id') === data.category_key);
    if (category) {
      category.setOpacity(data.transparency);
    }
  }

  makeLayer(data) {
    // finalizes layer as either a layer group if it has multiple
    // endpoints or as a single layer if it only has one endpoint
    let groupLayers = function (layers) {
      if (layers.length > 1) {
        let group = new LayerGroup({ layers: layers });
        group.set('id', data.key);
        return group;
      } else if (layers.length === 1) {
        layers[0].set('id', data.key);
        return layers[0];
      } else {
        throw new Error(`Could not make layer for ${data.key}`);
      }
    };

    try {
      let self = this;
      let core = this.core;
      let layers = null;

      switch (data.config.type) {
        case "mvt":







          layers = data.config.value.endpoints.map(endpoint => {

            endpoint.highlightFeats = {};

            var filterEngine = function (feature) {
              var renderFeature = true;
              var fev = 
                { render : false
                , renderFn : function () { return false; }
                , filtersRan : false
                , filtersChecked : {}
                }

              if (endpoint.filter) 
              {
                fev.filtersRan = true;
                renderFeature = true;
                var keys = Object.keys(endpoint.filter);
                keys.forEach(key => {
                  var filterResult = 
                    { checked : true
                    , result : false
                    , valuesChecked : {}
                    }
                  
                  var valuesMet = [];
                  endpoint.filter[key].values.forEach(value => {
                    var valueResult =
                      { conditionMet : false
                      }
                    if (value.filter.all) {
                      var allMet = true;
                      value.filter.all.forEach(filter => {
                        if (!feature || !feature.properties_) {
                        }
                        if (feature.properties_[filter.field]) {
                          if (filter.values && filter.values.exact)
                          {
                            allMet = allMet && (feature.properties_[filter.field] + "") == filter.values.exact;
                          }
                          else if (filter.values && (filter.values.greaterThan != undefined)) {
                            var value = feature.properties_[filter.field];

                            allMet = allMet && (value >= filter.values.greaterThan) && (value < filter.values.lessThan);
                          }
                          else if (filter.values && filter.values.null)
                          {
                            allMet = allMet && (feature.properties_[filter.field] == undefined || feature.properties_[filter.field] == null)
                          }
                          else {
                            allMet = false;
                          }
                        }
                        else if(filter.values.null)
                        {
                          allMet = true;
                        } else
                        {
                          allMet = false;
                        }
                      });

                      valueResult.conditionMet = allMet;
                      filterResult[value.name] = valueResult;
                      
                      //feature.properties_["FilterEngine_" + key + "_allMet_" + value.name] = allMet;

                      valuesMet.push(allMet);
                    }

                    if (value.filter.any) {
                      var allMet = false;
                      value.filter.any.forEach(filter => {
                        if (!feature || !feature.properties_) {
                        }
                        if (feature.properties_[filter.field]) {
                          if (filter.values && filter.values.exact)
                          {
                            allMet = allMet || (feature.properties_[filter.field] + "") == filter.values.exact;
                          }
                          else if (filter.values && (filter.values.greaterThan != undefined)) {
                            var value = feature.properties_[filter.field];

                            allMet = allMet || (value >= filter.values.greaterThan) && (value < filter.values.lessThan);
                          }
                          else if (filter.values && filter.values.null)
                          {
                            allMet = allMet || (feature.properties_[filter.field] == undefined || feature.properties_[filter.field] == null)
                          }
                          else {
                            allMet = false;
                          }
                        }
                        else if(filter.values.null)
                        {
                          allMet = true;
                        } else
                        {
                          allMet = false;
                        }
                      });

                      valueResult.conditionMet = allMet;
                      filterResult[value.name] = valueResult;
                      
                      //feature.properties_["FilterEngine_" + key + "_allMet_" + value.name] = allMet;

                      valuesMet.push(allMet);
                    }

                    fev.filtersChecked[key] = filterResult;
                  });


                  var renderThisFeature = false || endpoint.filter[key].values.length == 0;

                  if (endpoint.filter[key].mode == "OR") {
                    valuesMet.forEach(value => {
                      renderThisFeature = renderThisFeature || value;
                    })
                  }

                  if (endpoint.filter[key].mode == "AND") {
                    var renderThisFeature = true;
                    valuesMet.forEach(value => {
                      renderThisFeature = renderThisFeature && value;
                    })
                  }

                  filterResult.result = renderThisFeature;

                  fev.filtersChecked[key] = filterResult;

                  
                  
                  //feature.properties_["FilterEngine_" + key + "_render"] = renderThisFeature
                  if(endpoint.filterMode == "AND")
                    renderFeature = renderFeature && renderThisFeature;
                  else
                    renderFeature = renderFeature || renderThisFeature;
                })

              }

              fev.renderFn = function()
              {
                let flt = endpoint.filter;
                let filterSets = Object.keys(flt);
                let render = true;

                if(endpoint.filterMode == "AND")
                {
                  render = true;
                }
                else
                {
                  render = false;
                }
                
                for(var i = 0; i < filterSets.length; i++)
                {
                  var filterSet = flt[filterSets[i]];

                  var match = false; // || filterSet.values.length == 0;
                  var orMode = true;

                  if(filterSet.mode == "AND")
                  {
                    match = true;
                    orMode = false;
                  }
                  

                  for(var t = 0; t < filterSet.values.length; t++)
                  {
                    var value = filterSet.values[t];
                    var fc = fev.filtersChecked[filterSets[i]]
                    if(!fc)
                    {
                      debugger;
                      //We gonna crash ... why ...
                    }
                    var condVal = fc[value.name]

                    if(value.applied)
                    {
                      if(orMode)
                      {
                        match = match || condVal.conditionMet;
                        if(match)
                          break;
                      }
                      else
                      {
                        match = match && condVal.conditionMet;
                        if(!match)
                          break
                      }
                        
                    }
                  }

                  if(endpoint.filterMode == "AND")
                  {
                    render = render && match;
                    if(!render)
                    {
                      break;
                    }
                  }
                  else
                  {
                    render = render || match;
                    if(render)
                    {
                      break;
                    }
                  }
                }
                fev.render = render;

              } 

              feature.properties_["FilterEngine"] = fev;



              //feature.properties_["FilterEngine_render"] = renderFeature
              return renderFeature;
            }

            var styleFn = function (endpoint) {

              if (endpoint.style) {
                if (endpoint.style.static) {
                  var style = endpoint.style.static;

                  return new Style({
                    fill: new Fill({
                      color: style.fillColor || "rgba(255,0,0,0.5)"
                    }),
                    stroke: new Stroke({
                      color: style.strokeColor || "rgba(255,0,255,0.75)",
                      width: style.strokeWidth != undefined ? style.strokeWidth : 4
                    })
                  })
                }
                else if (endpoint.style.dynamic) {
                  return function (feature) {

                    if(endpoint.highlightFeats[feature.properties_.id])
                    {
                      return new Style({
                        fill: new Fill({
                          color: "rgba(255,255,0,1)"
                        }),
                        stroke: new Stroke({
                          color: "rgba(255,255,0,0)",
                          width: 0
                        })
                      })
                    }

                    var renderFeature = true;
                    var fev = feature.get("FilterEngine");
                    var r = true;

                    if(fev && fev.renderFn)
                    {
                      fev.renderFn();
                      r = fev.render;
                    }

                    renderFeature = feature.get("selected") || r;

                    let map = endpoint.style.dynamic.map;
                    let field = endpoint.style.dynamic.field;
                    if (!renderFeature) {
                      return new Style({
                        fill: new Fill({
                          color: "rgba(0,0,0,0)"
                        }),
                        stroke: new Stroke({
                          color: "rgba(0,0,0,0)",
                          width: 0
                        })
                      })
                    } else if (feature.get("selected"))
                    {
                      return new Style({
                        fill: new Fill({
                          color: "rgba(255,255,100,0.7)"
                        }),
                        stroke: new Stroke({
                          color: "rgba(255,255,0,1)",
                          width: 1
                        })
                      })
                    } else if (feature && feature.properties_ && feature.properties_[field]) {
                      var val = feature.properties_[field] + "";
                      if (map[val]) {
                        var style = map[val];
                        return new Style({
                          fill: new Fill({
                            color: style.fillColor || "rgba(255,0,0,0.5)"
                          }),
                          stroke: new Stroke({
                            color: style.strokeColor || "rgba(255,0,255,0.75)",
                            width: style.strokeWidth != undefined ? style.strokeWidth : 4
                          })
                        })
                      }
                      else {
                        //Need default style

                        return new Style({
                          fill: new Fill({
                            color: "rgba(255,255,255,0.5)"
                          }),
                          stroke: new Stroke({
                            color: "rgba(0,0,0,0.75)",
                            width: 4
                          })
                        })
                      }
                    }

                  }
                }
              }
              else {
                return new Style({
                  fill: new Fill({
                    color: endpoint.fillColor || "rgba(255,0,0,0.5)"
                  }),
                  stroke: new Stroke({
                    color: endpoint.strokeColor || "rgba(255,0,255,0.75)",
                    width: style.strokeWidth != undefined ? style.strokeWidth : 4
                  })
                })
              }
            };

            var url = endpoint.url;

            var source = new VectorTileSource({
              maxZoom: 15,
              format: new MVT({
                idProperty: 'iso_a3'
              }),
              url: url
            });

            // source.on("tileloadend", evt => {
            //   var f = evt.tile.getFeatures();
            //   f.forEach(filterEngine);
            // })

            let configureSource = function (tokenKey) {
              if (core.services && core.services[tokenKey]) {
                let tokenData = core.services[tokenKey];
                source.setUrl(`${tokenData.baseUrl || ""}${endpoint.url}`);
              }
            }
            if (endpoint.tokenKey) {
              // if the token data has already been fetched and stored in core.services
              // go ahead and configure the source w/ the data, otherwise, postpone
              // the configuration until `setServicesCmd` has been triggered
              if (core.services && core.services[endpoint.tokenKey]) {
                configureSource(endpoint.tokenKey);
              } else {
                self.pendingConfiguration.push({
                  name: data.key,
                  fn: configureSource,
                  params: [endpoint.tokenKey]
                });
              }
            }

            var vtLayer = new VectorTileLayer({
              declutter: true,
              source: source,
              style: styleFn(endpoint),
              zIndex: endpoint.zIndex || 1000

            });

            vtLayer.set('id', data.key);
            source.refreshFunction = 
              function () 
              { 
                source.changed();
                source.refresh();
              }
            console.log("Adding in the applyFilters for " + data.key)
            source.highlight =
              function(feature) {
                endpoint.highlightFeats[feature] = true;
                this.changed();
              }

              source.unhighlight =
                function(feature) {
                  delete endpoint.highlightFeats[feature];
                  this.changed();
                }


            source.applyFilters =
              function (layerset) {
                if (!endpoint.filter)
                  endpoint.filter = {};

                if(Array.isArray(layerset.filts))
                {
                    layerset.filts.forEach(lyr => {
                    //todo remove the test 
                    endpoint.filter['test_' + lyr.layerid] = lyr;
                  })
                }

                endpoint.filterMode = layerset.mode;

                var epk = Object.keys(endpoint.filter)

                var anyApplied = false;
                
                for(var i = 0; i < epk.length && !anyApplied; i++)
                {
                  var f = endpoint.filter[epk[i]];
                  for(var v = 0; v < f.values.length && !anyApplied; v++)
                  {
                    anyApplied = anyApplied || f.values[v].applied
                  }
                }

                if(source.inview)
                    vtLayer.setVisible(true);
                
                if(anyApplied)
                {
                  if(!source.inview)
                    vtLayer.setVisible(true);
                  this.changed();
                }
                else
                {
                  if(!source.inview)
                    vtLayer.setVisible(false);
                  this.changed();
                }
                // layer.getFeatures().forEach(feature =>
                //   {
                //     var fev = feature.get("FilterEngine");
                //     if(fev && fev.renderFn)
                //       fev.renderFn();
                //   })

                
              };

            source.clearFilters =
              function (layer) {
                if (!endpoint.filter)
                  endpoint.filter = {};
                delete endpoint.filter['test_' + layer.layerid];

                if (Object.keys(endpoint.filter).length == 0) {
                  vtLayer.setVisible(false);
                }

                this.changed();
                //endpoint.filter = {};
              }

            source.filterEngine = filterEngine;


            return vtLayer;
          });
          return groupLayers(layers);
        case "xyz":
          layers = data.config.value.endpoints.map(endpoint => {
            let lyr = new TileLayer({
              visible: false,
              preload: 4,
              zIndex: endpoint.zIndex || 0,
              opacity: data.opacity || 1,
              source: new XYZ({
                crossOrigin: 'anonymous',
                url: endpoint.url,
                maxZoom: data.config.value.maxZoom || 26,
                minZoom: data.config.value.minZoom || 1,
                tileLoadFunction: (imageTile, src) => {
                  imageTile.getImage().src = src;
                }
              })
            });
            lyr.set('id', data.key);
            lyr.set('name', data.name);
            return lyr;
          });

          return groupLayers(layers);

        case "wmts":
          var projection = get("EPSG:3857"),
            projectionExtent = projection.getExtent(),
            size = getWidth(projectionExtent) / 256,
            zooms = 15 + 1,
            resolutions = new Array(zooms),
            matrixIds = new Array(zooms);
          for (let z = 0; z < zooms; ++z) {
            resolutions[z] = size / Math.pow(2, z);
            matrixIds[z] = z;
          }

          layers = data.config.value.endpoints.map(endpoint => {
            let source = new WMTS({
              crossOrigin: 'anonymous',
              matrixSet: 'webmercator',
              format: 'image/png',
              projection: projection,
              requestEncoding: 'REST',
              tileGrid: new WMTSTileGrid({
                extent: data.config.value.extent,
                resolutions: resolutions,
                matrixIds: matrixIds
              }),
              style: 'default',
              opaque: false,
              transparent: true
            });
            let configureSource = function (tokenKey) {
              if (core.services && core.services[tokenKey]) {
                let tokenData = core.services[tokenKey];
                source.setUrl(`${tokenData.baseUrl || ""}${endpoint.url}`);
                source.setTileLoadFunction(function (imageTile, src) {
                  imageTile.getImage().src = `${src}?token=${tokenData.token || ""}`;
                });
              }
            }

            if (endpoint.tokenKey) {
              // if the token data has already been fetched and stored in core.services
              // go ahead and configure the source w/ the data, otherwise, postpone
              // the configuration until `setServicesCmd` has been triggered
              if (core.services && core.services[endpoint.tokenKey]) {
                configureSource(endpoint.tokenKey);
              } else {
                self.pendingConfiguration.push({
                  name: data.key,
                  fn: configureSource,
                  params: [endpoint.tokenKey]
                });
              }
            }

            let lyr = new TileLayer({
              visible: false,
              preload: 4,
              zIndex: endpoint.zIndex || 0,
              opacity: data.opacity || 1,
              source: source,
              opaque: false
            });
            lyr.set('id', data.key);
            lyr.set('name', data.name);
            return lyr;
          });

          return groupLayers(layers);

        case "wms":
          var projection = proj.get("EPSG:3857"),
            projectionExtent = projection.getExtent(),
            size = getWidth(projectionExtent) / 256,
            zooms = 15 + 1,
            resolutions = new Array(zooms);
          for (let z = 0; z < zooms; ++z) {
            resolutions[z] = size / Math.pow(2, z);
          }

          layers = data.config.value.endpoints.map(endpoint => {
            //The random adds a random value to the parameter
            //essentually cache busting  
            let customParams = {
              get random() {
                return Math.random();
              }
            };

            let source = new ImageWMS({
              params: { 'LAYERS': 'geonode:shapes' },
              ratio: 1,
              serverType: 'geoserver',
              resolutions: resolutions,
              projection: projection
            });

            let configureSource = function (tokenKey) {
              if (core.services && core.services[tokenKey]) {
                let tokenData = core.services[tokenKey];
                source.setUrl(`${tokenData.baseUrl || ""}${endpoint.url}`);
                if (tokenData.token) {
                  customParams["token"] = tokenData.token;
                }
                source.params_ = customParams;
              }
            }

            if (endpoint.tokenKey) {
              // if the token data has already been fetched and stored in core.services
              // go ahead and configure the source w/ the data, otherwise, postpone
              // the configuration until `setServicesCmd` has been triggered
              if (core.services && core.services[endpoint.tokenKey]) {
                configureSource(endpoint.tokenKey);
              } else {
                self.pendingConfiguration.push({
                  name: data.key,
                  fn: configureSource,
                  params: [endpoint.tokenKey]
                });
              }
            }

            let lyr = new ImageLayer({
              zIndex: endpoint.zIndex || 0,
              extent: data.config.value.extent,
              source: source
            });
            lyr.set('id', data.key);
            lyr.set('name', data.name);
            return lyr;
          })

          return groupLayers(laeyrs);

        case "esriExport":
          layers = data.config.value.endpoints.map(endpoint => {
            //The random adds a random value to the parameter
            //essentually cache busting  
            let customParams = {
              get random() {
                return Math.random();
              }
            };

            if (endpoint.bbox) {
              customParams["BBOX"] = endpoint.bbox;
            }

            if (endpoint.layersToShow) {
              customParams["LAYERS"] = endpoint.layersToShow;
            }

            if (data.config.value.layerDefs) {
              customParams["layerDefs"] = data.config.value.layerDefs
            }

            var projExtent = get('EPSG:3857').getExtent();
            var startResolution = getWidth(projExtent) / 256;
            var resolutions = new Array(22);
            for (var i = 0, ii = resolutions.length; i < ii; ++i) {
              resolutions[i] = startResolution / Math.pow(2, i);
            }
            var tileGrid = new TileGrid({
              extent: [-13884991, 2870341, -7455066, 6338219],
              resolutions: resolutions,
              tileSize: [256, 256]
            });

            let source = new TileArcGISRest({
              crossOrigin: 'anonymous',
              ratio: 1,
              maxZoom: 26,
              tileLoadFunction: (image, src) => {
                image.getImage().src = src;
              },
              tileGrid: tileGrid
              // tileGrid: new TileGrid(
              //     { tileSize:[2048,2048]
              //       , resolutions:[]

              //       , extent: data.config.value.extent
              //     }
              //     )
            });


            let lyr = new TileLayer({
              visible: false,
              preload: 4,
              zIndex: endpoint.zIndex || 0,
              opacity: data.opacity || 1,
              source: source,
              extent: data.config.value.extent
            });
            lyr.set('id', data.key);

            source.applyFilters =
              function (ls) {
                if(!Array.isArray(ls) && ls.filts)
                  ls = ls.filts;
                ls.forEach(layer => {
                  
                var def = {};
                var conditions = [];
                layer.values.forEach(value => {
                  if (value.applied && value.filter && value.filter.all && value.filter.all.length > 0) {
                    var indiConds = [];
                    value.filter.all.forEach(condition => {

                      if (condition.values.exact) {
                        indiConds.push("( " + condition.field + " = '" + condition.values.exact + "' )")
                      } else if (condition.values.range) {
                        indiConds.push("( " + condition.field + " > '" + condition.values.greaterThan + "' AND " + condition.field + " < '" + condition.values.lessThan + "')")
                      }

                    })

                    var finalCond = "(" + indiConds.join(" AND ") + ")";
                    conditions.push(finalCond);
                  }
                });

                var finalFilter = "";
                switch (layer.mode) {
                  case "OR": finalFilter = conditions.join(" OR "); break;
                  case "AND": finalFilter = conditions.join(" AND "); break;
                }

                if(finalFilter.length > 0)
                {
                  def[layer.layerid] = finalFilter;
                  def = JSON.stringify(def);
                }  
                else
                { def = ""
                }
                

                let olddef = source.params_["layerDefs"];
                let newdef = def;

                this.params_["layerDefs"] = def;
                if (olddef != newdef) {
                  if(newdef.length > 0)
                  {
                    lyr.setVisible(true);
                    if (this.tileCache) {
                      this.tileCache.clear();
                    }
                    this.changed();
                  }
                  else
                  {
                    lyr.setVisible(false);
                  }
                }
              })
              };

            source.clearFilters =
              function (layer) {
                var def = {};
                def[layer.layerid] = "0=1";
                def = JSON.stringify(def);

                let olddef = source.params_["layerDefs"];
                let newdef = def;

                this.params_["layerDefs"] = def;

                lyr.setVisible(false);

                if (olddef != newdef) {
                  if (this.tileCache) {
                    this.tileCache.clear();
                  }
                  this.changed();
                }
              }

            let configureSource = function (tokenKey) {
              if (core.services && core.services[tokenKey]) {
                let tokenData = core.services[tokenKey];
                source.setUrl(`${tokenData.baseUrl || ""}${endpoint.url}`);
                if (tokenData.token) {
                  customParams["token"] = tokenData.token;
                }
                source.params_ = customParams;
              }
            }

            if (endpoint.tokenKey) {
              // if the token data has already been fetched and stored in core.services
              // go ahead and configure the source w/ the data, otherwise, postpone
              // the configuration until `setServicesCmd` has been triggered
              if (core.services && core.services[endpoint.tokenKey]) {
                configureSource(endpoint.tokenKey);
              } else {
                self.pendingConfiguration.push({
                  name: data.key,
                  fn: configureSource,
                  params: [endpoint.tokenKey]
                });
              }
            }
            else {
              source.setUrl(endpoint.url);
              source.params_ = customParams;
            }
            return lyr;
          });

          return groupLayers(layers);

        default:
          throw new Error(`Layer type '${data.config.type}' has not been implemented.`);
      }
    }
    catch (err) {
      debugger;
      console.error(err);
    }

  }
  render() { }
}
