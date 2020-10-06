import Feature from "ol/Feature";
import Point from "ol/geom/Point";

import { Vector as VectorLayer } from "ol/layer.js";
import { Vector as VectorSource } from "ol/source.js";

import { Style, Fill, Stroke } from "ol/style";
import CircleStyle from "ol/style/Circle";

import WKTizer  from "../../Common/WKTizer";

import { Select } from "ol/interaction.js";

import {Group as LayerGroup} from 'ol/layer.js';

import ImageLayer from "ol/layer/Image";

import { ImageArcGISRest } from "ol/source";

import { get } from "ol/proj";
import { getWidth } from "ol/extent";
import ImageWMS from 'ol/source/ImageWMS.js';

import {xml2json} from 'xml-js';

const CalcAreaWorker = require("../../Common/calc-area.worker.js");

export default class FeaturePickerPlugin {
  constructor() {
    //Best practices state that you should set up the objects properties here
    this.core = null;
    this.source = null;
    this.layer = null;
    this.dragAndDrop = null;
    this.shapeFileFormat = null;
    this.format = new WKTizer();
    this.layers = {};
    this.idPairs = {};
    this.selectedIDs = []
  }

  apply(core) {
    //Apply is called when the plugin is added and core is ready to add plugins
    this.core = core;
    this.setupPorts(core);
  }

  xmlToJs(val) {
    var converted = xml2json(val.xml, {compact: true, spaces: 4});
    val.result = JSON.parse(converted);
    this.core.emit("xmlToJsResult", val);
  }

  normalizeEsri(val) {
    var parsed = JSON.parse(val.esri);
    let layer = this.layer.config.esri;

    for(var i in parsed.features)
    {
      if(parsed.features[i].geometry)
      {
        let oldFeat = parsed.features[i];

        let fw = this.format.geojsonToWKTAndOl(oldFeat)
        let feat = fw.ol; //this.format.geojsonToOl(oldFeat)

        if(feat && this.source)
        {
          let idField = "id";

          if(layer.idField)
            idField = layer.idField;

          let nf = new Feature({
            geometry: feat,
            id: oldFeat.properties[idField] || Math.floor(Math.random() * 9999)
          });
          this.source.addFeature(nf);

          let view = this.map.getView();
          if (view) {
            let geometry = nf.getGeometry();
            if (geometry) {
              view.fit(geometry, {
                size: this.map.getSize(),
                duration: 1000
              });
            }
          }
        }

        var wkt = fw.wkt; //this.format.geojsonToWKT(oldFeat)
        
        parsed.features[i].geometry = wkt;

        
      }
      if(!parsed.features[i].properties && parsed.features[i].attributes)
      {
        parsed.features[i].properties = parsed.features[i].attributes;
      }
    }

    
    val.value = parsed;



    let worker = new CalcAreaWorker();
      
      worker.onmessage = e => {
        switch (e.data.type) {
          case "done":
            this.core.emit("normalizeEsriResult", e.data.val);
            
            worker.terminate();
            break;
          case "error":
            this.core.emit("drawingEnded", { type_:"Polygon",acres:0, wkt:thisWKT, flat:flattenedWKTs, flatArea:0, id: this.nextID }) ;
            worker.terminate();
            break;
          default:
            break;
        }
      };
      worker.onerror = e => {
        console.error(e);
        worker.terminate();
        this.core.emit("drawingEnded", {type_:"Polygon",acres:0, wkt:thisWKT, flat:flattenedWKTs, id: this.nextID});
      };

      worker.postMessage({
        type: "processEsri",
        id: -1,
        val: val
      });
  }

  setupPorts(core) {
    //core mapCmd will try to map to a port and set the subscribe to the passed in function
    //Attempt to keep this clean by calling the functions in this class instead of dropping your code
    //into the body

    core.mapCmd("featurePickerSetLayer", val => {
      this.init();
      this.showReferenceLayer(val);
    })

    core.mapCmd("featurePickerDisable", val => {
      this.denit();
    })

    core.mapCmd("xmlToJs", xml => {
      this.xmlToJs(xml);
    })

    core.mapCmd("selectFeature", id => {
      this.selectFeature(id);
    })

    core.mapCmd("deselectFeature", id => {
      this.deselectFeature(id);
    })

    core.mapCmd("normalizeEsri", val => {
      this.normalizeEsri(val);
    })
  }


  selectFeature(id) {
  }

  deselectFeature(id) {
    var feats = this.source.getFeatures();
    for(var feat of feats)
    {
      if(feat.get("id") == id)
        this.source.removeFeature(feat);
    }
  }

  setSelected(items) {
    let f = this.select.getFeatures();
    f.clear();

    for (let item of items) {
      let layerGroup = this.layers[this.layer.url];
      let layer = layerGroup.getLayers().array_[0];
      let source = layer.getSource();
      let feature = source.getFeatureById(item[0]);
      if (feature) f.push(feature);
    }
  }

  showReferenceLayer(layer) {
    this.layer = layer;

    if(!this.boundClick)
    { 
      this.map.on('singleclick', this.onclick.bind(this));
      this.boundClick = true;
    }
    
    // if (this.select) {
    //   this.map.removeInteraction(this.select);
    //   this.select = null;
    // }

    // if(this.ler)
    // {
    //   this.map.removeLayer(this.ler);
    // }

    // if (this.layers[layer.name])
    // {
    //   this.map.removeLayer(this.layers[layer.name]);
    //   this.source = null;

    //   this.layers[layer.name] = null;
    // }


    // if (!this.layers[layer.name]) this.initLayer(layer);
    // if(this.idPairs[layer.name])
    //   this.emitIDPairs(layer);
    
    // this.initSelect(layer);

    // this.hideAll();
    // this.showLayer(layer);
  }

  fetchBatch(layer, query, batch, batchSize, source, centroidSource)
  {
    var that = this;
    var offset = "&resultOffset=" + (batch * batchSize);
    var count = ("&resultRecordCount=" + batchSize);
    fetch(layer.url + "query?" + query + offset + count, {
      method: "GET",
      headers: {
        //"Content-Type": "application/json",
        //"Content-Type": "application/x-www-form-urlencoded"
      },
      //body: formBody,
      mode: "cors"
    })
      .then(response => {
        return response.json();
      })
      .then(value => {
        if (!value || !value.features || !value.features.length) return;

        //console.log(value);
        var features = that.geojson.readFeatures(value, {
          featureProjection: "EPSG:3857"
        });
        //console.log(features);
        source.addFeatures(features);

        let hasCentroid = value.features[0].properties.CentroidX != null;
        
        let idPairs = [];
        //console.log("We have centroids");
        let centroids = [];

        for (let feature of value.features) {
          idPairs.push([
            feature.id,
            layer.nameField
              ? feature.properties[layer.nameField] || feature.id + ""
              : feature.id + ""
          ]);

          if (hasCentroid) {
            let centroid = new Feature({
              geometry: new Point([feature.CentroidX, feature.CentroidY]),
              id: feature.id
            });

            centroid.setId(feature.id);
            centroids.push(centroid);
            //console.log(centroids);
          }
        }

        if (hasCentroid) {
          centroidSource.addFeatures(centroids);
        }

        //If we have centroids add them here
        //Otherwise we should let elm know we got features as well ^_^
        if(!that.idPairs[layer.url])
          that.idPairs[layer.url] = idPairs;
        else
          that.idPairs[layer.url] = that.idPairs[layer.url].concat(idPairs);
        

        if(value.features.length >= batchSize)
        {
          this.fetchBatch(layer,query,batch+1,batchSize,source, centroidSource);
        }
        else
        {
          that.emitIDPairs(layer);
          that.initSelect(layer);
        }
        
      });
  }

  addDynamic(layer, core, group, stopProcessing)
  {
    var map = core.getMap();



    //The random adds a random value to the parameter
    //essentually cach busting
    
    let customParams = {
       get random() {
         return Math.random();
      }
    };

    layer.useToken = layer.tokenKey != undefined;

    if (layer.boundingBox !== undefined) {
      customParams["BBOX"] = layer.boundingBox;
    }
    if (layer.layerToShow !== undefined) {
      customParams["LAYERS"] = layer.layerToShow;
    }
    if (layer.layerDefs !== undefined) {
      customParams["LAYERDEFS"] = layer.layerDefs;
    }
    if (layer.layers !== undefined) {
      customParams["LAYERS"] = "show:" + layer.layers.join(",")
    }

    if (layer.useToken) {
      
        if (core.services) 
        {
          if ( core.services[layer.tokenKey] )
          {
              if( core.services[layer.tokenKey].baseUrl &&
                    core.services[layer.tokenKey].baseUrl.length )
              {
                layer.url =
                  core.services[layer.tokenKey].baseUrl + layer.url;
              }
              

              if(core.services[layer.tokenKey].token)
              {
                customParams["token"] = core.services[layer.tokenKey].token;
              }
          }
          else
          {
            if(!stopProcessing)
              this.addPending(this.addDynamic, layer, group);
            return;
          }
          
        }
    }    
    

    let dynamicLayer = new ImageLayer({
      visible: true,
      preload: 4,
      zIndex: 9999,
      opacity: 0.5, //layer.opacity || 1,
      source: new ImageArcGISRest({
        crossOrigin: "anonymous",
        ratio: 1,
        params: customParams,
        url: layer.url,
        maxZoom: 19,
        tileLoadFunction: (image, src) => {
          image.getImage().src = src;
        }
      })
    });
    
    group.getLayers().push(dynamicLayer);
  }

  addWMS(layer,core, group, stopProcessing)
  {
    let map = core.getMap();
    let projection = get("EPSG:3857"),
    projectionExtent = projection.getExtent(),
    size = getWidth(projectionExtent) / 256,
    zooms = 19 + 1;
    
    
    let params = {};

    layer.useToken = layer.tokenKey != undefined;

    if (layer.useToken) {
      //TODO: Pull from tokens
      if (core.services) {

        
        if (
          core.services[layer.tokenKey]
        ) {
          if( core.services[layer.tokenKey].baseUrl &&
            core.services[layer.tokenKey].baseUrl.length )
            {
              layer.url =
                core.services[layer.tokenKey].baseUrl + layer.url;
            }
            

            if(core.services[layer.tokenKey].token)
            {
              params["token"] = core.services[layer.tokenKey].token;
            }
          // layer.url =
          //   core.services[layer.tokenKey].baseUrl + layer.url;
        }
        else
          {
            if(!stopProcessing)
              this.addPending(this.addWMS, layer, group);
            return;
          }
        
      }

      //Fallback because the token key wasnt found
      
    
  }

  params['LAYERS'] = (layer.layers || []).join(',');
  //TODO build filters
  
  var doc = document.implementation.createDocument(null, "Filter");
  var filter = doc.getElementsByTagName("Filter")[0];
  
  var piet = doc.createElement("PropertyIsEqualTo");
  filter.appendChild(piet);

  var pn = doc.createElement("PropertyName");
  pn.innerHTML = "type";

  var lv = doc.createElement("Literal");
  lv.innerHTML = "1";

  piet.appendChild(pn);
  piet.appendChild(lv);

  //params.filter = (new XMLSerializer()).serializeToString(filter);
  
    let wmts = new ImageLayer({
      zIndex: 9999,
      extent: [-13884991, 2870341, -7455066, 6338219],
      source: new ImageWMS({
        url: layer.url,
        crossOrigin: 'Anonymous',
        params: params, //{'LAYERS': 'geonode:shapes'},
        ratio: 1,
        serverType: 'geoserver'
      })
    })

    // wmts.set("name", layer.val.name);
    // wmts.set("id", layer.val.id);
    
    
    group.getLayers().push(wmts);
  }


  
  initLayer(layer) {
    var core = this.core;
    var map = core.getMap();
    let group = new LayerGroup();
    group.set("name", layer.name);
    group.set("id", layer.id);
    group.set("opacity", 0.5);
    group.setVisible(true);
    group.setZIndex(9999);


    let source = new VectorSource({});

    this.source = source;

    let lyr = new VectorLayer({
      source: source,
      zIndex: 9999,
      style: new Style({
        image: new CircleStyle({
          radius: 4,
          stroke: new Stroke({
            color: "#696969",
            width: 1
          }),
          fill: new Fill({
            color: "rgba(0,0,0,0.75)"
          })
        })
      })
    });

    lyr.setVisible(true);

    group.getLayers().push(lyr);

    map.addLayer(group);


    //Prioritize WMTS over WMS over ESRI
    if(layer.config.wmts)
    {
      this.addWMTS(layer.config.wmts, core, group)
    }
    if(layer.config.wms)
    {
      //WMS/WFS layer
      this.addWMS(layer.config.wms, core, group)
    }
    else if (layer.config.esri)
    {
      this.addDynamic(layer.config.esri, core, group)
    }

    this.layers[layer.name] = group;

    let ler = this.newLayer("rgba(255, 255, 255, 0.2)", "#5BE", this.source, 10003);
    this.ler = ler;
    this.map.addLayer(ler);

    if(!this.boundClick)
    { 
      this.map.on('singleclick', this.onclick.bind(this));
      this.boundClick = true;
    }

  }

  

onclick(evt) {

  
  //TODO: Make this work with the new config
  //ALso this is only esri specific


  var coordinate = evt.coordinate;
  this.coordinate = coordinate;
  
  this.data = [];
  this.index = 0;

  let reqs = [];
  var core = this.core;

  
  var layerModelMap = {};
  
  let layer = this.layer;

  let layerConfig = layer.config.esri;

  for(let i = 0; i < layerConfig.layers.length; i++)
  {

    let baseURL = ""; 

    let token = undefined;
    
    if(layerConfig.tokenKey)
    {
      if (core.services) 
      {
        if ( core.services[layerConfig.tokenKey] )
        {
          baseURL = core.services[layerConfig.tokenKey].baseUrl;
          token = core.services[layerConfig.tokenKey].token
        }
      }
    }
    let map = this.core.getMap();

    let tolerance = 0;
    let view = map.getView();
    
    if (view) 
    {
      tolerance = 5 - view.getZoom();
    }

    if(tolerance < 0)
      tolerance = 0;

    let url = layerConfig.url + "/identify?geometry=" +
    coordinate[0] + "%2C" + coordinate[1] + 
    "&geometryType=esriGeometryPoint&sr=102100"

    // if(o[i].identify.queryLayerId != null)
    // {
    //   layerModelMap[o[i].identify.queryLayerId + ""] = layer;
    url += "&layers=all%3A" + layerConfig.layers[i]
    // }
    

    url += "&tolerance=" + tolerance + //TODO
      "&mapExtent=-13599773.911815433%2C4274035.298291555%2C-11251628.40289482%2C5284227.064108444" + //TODO
      "&imageDisplay=1920%2C1080%2C96&returnGeometry=false" + 
      "&dynamicLayers=&returnZ=false&returnM=false&gdbVersion=&returnUnformattedValues=false" +
      "&returnFieldName=false&datumTransformations=&layerParameterValues=&mapRangeValues=&layerRangeValues=&f=pjson"

    if (token)
      url += "&token=" + token

    let req =
      { src: layer
      , url: url        
      }

    reqs.push(req);
  }

  /*
    let url="https://maps3dev.timmons.com/arcgis/rest/services/utwrapdev/PAM/MapServer/identify?geometry=" +
    coordinate[0] + "%2C" + coordinate[1] + 
    "&geometryType=esriGeometryPoint&sr=102100&layers=0&layerDefs=&time=&layerTimeOptions=&tolerance=100" +
    "&mapExtent=-13599773.911815433%2C4274035.298291555%2C-11251628.40289482%2C5284227.064108444" + 
    "&imageDisplay=1920%2C1080%2C96&returnGeometry=false&maxAllowableOffset=&geometryPrecision=" + 
    "&dynamicLayers=&returnZ=false&returnM=false&gdbVersion=&returnUnformattedValues=false" +
    "&returnFieldName=false&datumTransformations=&layerParameterValues=&mapRangeValues=&layerRangeValues=&f=pjson"

    */

    var that = this;


    //TODO: Map the data back to the model by locating the idfield. Then send the resulting IDs back to elm so that it can do things
    for(var req of reqs)
    {
      req = JSON.parse(JSON.stringify(req));
      var rfn = function(req)
      {
        
        that.requestsActive ++;
        window.fetch(req.url)
        .then(function(response) {
            return response.json();
          })
          .then(function(myJson) {
            
            if(!myJson.results)
              return;
              
            myJson = myJson.results;

            let layer = that.layer.config.esri;


            for(var i = 0; i < myJson.length; i++)
            {
              if(myJson[i].attributes[layer.idField || "id"] !== undefined)
              {
                var id = myJson[i].attributes[layer.idField || "id"];
                core.emit("featureSelected", id);
              }
            }
            
            
            that.requestsActive --;
          })
          .catch(function(error) {
            that.requestsActive --;
            debugger;
          })
      }
      rfn(req);
    }
}




  newLayer(fill, stroke, source, zIndex)
  {
    return new VectorLayer({
      source: source,
      zIndex: zIndex,
      style: function(feat)
      { return new Style({
        fill: new Fill({
          color: "rgba(255,255,255,0.8)"
        }),
        stroke: new Stroke({
          color: stroke,
          width: 6
        }),
        image: new CircleStyle({
          radius: 7,
          fill: new Fill({
            color: fill
          })
        })
      })
    }
    });
  }

  emitIDPairs(layer) {
    let idPairs = this.idPairs[layer.url];
    this.core.emit("featurePickerLayerLoadComplete", idPairs);
  }

  initSelect(layer) {
    let layerGroup = this.layers[layer.url];

    if (!layerGroup) return;
    if (this.select) return;

    let select = new Select({
      layers: layerGroup.getLayers().array_,
      //toggleCondition: condition.platformModifierKeyOnly,
      multi: true,
      hitTolerance: 2
    });
    select.set("name", "select");

    this.select = select;
    this.select.on("select", this.onSelectHandler.bind(this));

    //TODO: Move this somewhere else maybe
    this.map.addInteraction(select);

    this.core.emit("featurePickerSelectionChanged", []);
  }

  onSelectHandler(event) {
    let idPairs = [];
    let layer = this.layer;

    for (let feature of this.select.getFeatures().array_) {
      idPairs.push([
        feature.id_,
        layer.nameField
          ? feature.getProperties()[layer.nameField] || feature.id_ + ""
          : feature.id_ + ""
      ]);
    }
    this.core.emit("featurePickerSelectionChanged", idPairs);
  }

  hideAll() {
    for (let layer in this.layers) {
      this.layers[layer].setVisible(false);
    }
  }

  showLayer(layerDef) {
    let layer = this.layers[layerDef.name];

    if (layer) layer.setVisible(true);
  }

  init() {
    this.map = this.core.getMap();
  }

  denit() 
  {
    for (let layer in this.layers) {
      this.map.removeLayer(this.layers[layer]);
    }

    if(this.ler)
      this.map.removeLayer(this.ler);
  }

  enable() {
    if (!this.layer) return;
    if (this.layers[this.layer.url])
      this.layers[this.layer.url].setVisible(true);
    if (this.select) this.map.addInteraction(this.select);
  }

  disable() {
    if (!this.layer) return;
    if (this.layers[this.layer.url])
      this.layers[this.layer.url].setVisible(false);
    if (this.select) this.map.removeInteraction(this.select);
  }

  clearLayer() {}

  getFeatureWKTs() {
    let featsCollection = this.select.getFeatures();
    let feats = featsCollection.array_;
    let wkts = [];

    for (let feat of feats) {
      wkts.push({wkt: this.format.convertFeatureToWKT(feat.getGeometry())});
    }


    let worker = new CalcAreaWorker();
      
      worker.onmessage = e => {
        switch (e.data.type) {
          case "done":
            //let evt = {type_:"Polygon",acres:e.data.data[0].acres, wkt:e.data.data[0].wkt, flat:e.data.data[1].wkt, flatArea:e.data.data[1].acres, id: e.data.data[0].id };
            this.core.emit("getFeaturePickerWKTsResult", e.data.wkts);
            
            worker.terminate();
            break;
          case "error":
            this.core.emit("drawingEnded", { type_:"Polygon",acres:0, wkt:thisWKT, flat:flattenedWKTs, flatArea:0, id: this.nextID }) ;
            worker.terminate();
            break;
          default:
            break;
        }
      };
      worker.onerror = e => {
        console.error(e);
        worker.terminate();
        this.core.emit("drawingEnded", {type_:"Polygon",acres:0, wkt:thisWKT, flat:flattenedWKTs, id: this.nextID});
      };

      worker.postMessage({
        type: "start",
        id: -1,
        wkts: wkts
      });

    
  }

  render() {}
}

class ESRIQueryVectorSource {}