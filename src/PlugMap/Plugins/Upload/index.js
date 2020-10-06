import { DragAndDrop } from "ol/interaction.js";
import { GPX, GeoJSON, IGC, KML, TopoJSON } from "ol/format.js";
import { Vector as VectorLayer } from "ol/layer.js";
import { Vector as VectorSource } from "ol/source.js";
import { Style, Fill, Stroke } from "ol/style";
import CircleStyle from "ol/style/Circle";
import shp from "shpjs";
import { toMercator } from "@turf/turf";

import WKTizer from "../../Common/WKTizer";

const CalcAreaWorker = require("../../Common/calc-area.worker.js");
// This plugin adds a port for core to receive extent/zoom level from outside world (e.g rest api, ...)

function isGeometryGeographicHack(geojson) {
  switch (geojson.features[0].geometry.type) {
    case "Point":
      return isCoordinateGeographicHack(
        geojson.features[0].geometry.coordinates[0]
      );

    case "MultiPoint":
      return isCoordinateGeographicHack(
        geojson.features[0].geometry.coordinates[0][0]
      );

    case "LineString":
      return isCoordinateGeographicHack(
        geojson.features[0].geometry.coordinates[0][0]
      );

    case "MultiLineString":
      return isCoordinateGeographicHack(
        geojson.features[0].geometry.coordinates[0][0][0]
      );

    case "Polygon":
      return isCoordinateGeographicHack(
        geojson.features[0].geometry.coordinates[0][0][0]
      );

    case "MultiPolygon":
      return isCoordinateGeographicHack(
        geojson.features[0].geometry.coordinates[0][0][0][0]
      );

    default:
      throw new Error(`Tried to parse an invalid or unsupported shape type.
      To create a project area, please upload a shapefile with only features 
      of the supported types: Point, MultiPoint, LineString, MultiLineString, 
      Polygon, or MultiPolygon.`);
  }
}

function isCoordinateGeographicHack(x) {
  let value = Math.abs(x);
  return value > -10000 && value < 10000;
}

export default class UploadPlugin {
  constructor() {
    //Best practices state that you should set up the objects properties here
    this.core = null;
    this.source = null;
    this.layer = null;
    this.dragAndDrop = null;
    this.shapeFileFormat = null;
    this.format = new WKTizer();
    this.geojson = new GeoJSON();
  }

  apply(core) {
    //Apply is called when the plugin is added and core is ready to add plugins
    this.core = core;
    this.setupPorts(core);
  }

  setupPorts(core) {
    //core mapCmd will try to map to a port and set the subscribe to the passed in function
    //Attempt to keep this clean by calling the functions in this class instead of dropping your code
    //into the body

    // core.mapCmd("initUpload", val => {
    //   this.init();
    // });

    // core.mapCmd("unloadUpload", val => {
    //   this.denit();
    // });

    // core.mapCmd("enableUpload", val => {
    //   this.enableDragAndDrop();
    // });

    // core.mapCmd("clearUploadLayer", val => {
    //   this.clearLayer();
    // });

    // core.mapCmd("getUploadedFeatureWKTs", val => {
    //   this.getFeatureWKTs();
    // });

    // core.mapCmd("manualUpload", val => {
    //   this.manualUpload(val);
    // });
    core.mapCmd("openChooseFile", val => {
      this.init();
      this.enableDragAndDrop();
      this.manualUpload(val);
    })
  }

  manualUpload(elementId) {
    elementId =  elementId + ""

    if (!this.dragAndDrop) return; //TODO

    var elm = document.getElementById(elementId);

    if (!elm)
    {
      elm = document.createElement("input");
      elm.type = "file";

      elm.onchange = evt => {
        this.core.emit("statusUpdate", { status : "Opening" });
        this.openFiles(elm);
      }

      
    };

    elm.click();

    
  }

  clearLayer() {
    this.source.clear();
  }

  getFeatureWKTs() {
    let wkts = this.format.convertFromSource(this.source, 10); //TODO: Get this from overworld
    this.core.emit("getUploadedFeatureWKTsResult", wkts);
  }

  init() {
    let map = this.core.getMap();
    this.map = map;
    this.initSource(map);
    this.initLayer(map);
    this.initShapeFileFormat(map);
    this.initDragAndDrop(map);
  }

  denit() {}

  initSource() {
    if (!this.source) this.source = new VectorSource();
  }

  initLayer(map) {
    this.initSource();
    if (!this.layer) {
      var vector = new VectorLayer({
        source: this.source,
        zIndex: 1002,
        style: new Style({
          fill: new Fill({
            color: "rgba(255, 255, 255, 0.2)"
          }),
          stroke: new Stroke({
            color: "#33cc33",
            width: 2
          }),
          image: new CircleStyle({
            radius: 7,
            fill: new Fill({
              color: "#33cc33"
            })
          })
        })
      });
      map.addLayer(vector);
      this.layer = vector;
    }
  }

  initShapeFileFormat() {}

  initDragAndDrop() {
    if (this.dragAndDrop) return;

    this.initSource();

    var dragAndDrop = new DragAndDrop({
      //source: this.source,
      //TODO: Formatters
      formatConstructors: [GPX, GeoJSON, IGC, KML, TopoJSON]
    });

    this.dragAndDrop = dragAndDrop;

    let onAddFeatures = function(root) {
      return function(event) {
        root.addFeatures(event, root);
      };
    };

    dragAndDrop.on("addfeatures", onAddFeatures(this));

    let oldHandle = dragAndDrop.handleResult_;
    let me = this;

    dragAndDrop.handleResult_ = function(file, oldEvent) {
      me.handleDDFile(file, oldEvent);
      
    };
  }


  openFiles(elm) {

    var files = elm.files;

    let worker = new CalcAreaWorker();
    let errorLimit = 5;

    worker.onmessage = e => {
      switch (e.data.type) {
        case "openingFile":
          this.core.emit("statusUpdate", { status : "Opening" });
          break;

        case "processingFile":
          this.core.emit("statusUpdate", { status : "Opening" });
          break;

        case "processingFeatures":
          this.core.emit("statusUpdate", { status : "Converting" });
          break;

        case "calculating":
          this.core.emit("statusUpdate", { status : "Calculating" });
          break;

        case "calculated":
          var ent = { features : e.data.result }
          worker.terminate();

          for(var f of ent.features)
          {
            f.wkt = this.format.geojsonToWKT(f.feature);
            f.acres = f.area;
          }


          console.log(ent);
          this.core.emit("fileUploaded", ent.features);

          break;

        case "done":
          
          worker.terminate();
          break;
        case "error":
          console.log(e);
          this.core.emit("statusUpdate", { status : { error : [ e.data.error ] }});
          
          if(e.data.fatal || errorLimit == 0)
            worker.terminate();
          errorLimit --;
          break;
        default:
          break;
      }
    };
    worker.onerror = e => {
      console.error(e);
      worker.terminate();
      //this.core.emit("drawingEnded", {type_:"Polygon",acres:0, wkt:thisWKT, flat:flattenedWKTs, id: this.nextID});
    };

    worker.postMessage({
      type: "processUpload",
      id: -1,
      files: files
    });
  }

  handleDDFile(file, oldEvent) {



  }

  addFeatures(event, that) {
    this.core.emit("statusUpdate", { status : "Converting" });

    var res = [];


    this.core.emit("statusUpdate", { status : "Calculating" });

    this.core.emit("fileUploaded", res); 
  }

  enableDragAndDrop() {
    if (this.map && this.dragAndDrop) {
      this.map.addInteraction(this.dragAndDrop);
    }
  }

  render() {
    //This will be used in the future if we need to add some sore of custom element
    const el = document.createElement("div");
    return el;
  }
}

class ShapeFileReader {
  constructor() {
    this.format = {
      geojson: new GeoJSON()
    };
  }

  readFeatures(event, options) {
    console.log("Must use the array buffer method");
    return null;
  }

  readFeaturesFromArrayBuffer(evt, options) {
    let arrayBuffer = evt.target.result;
    let self = this;
    return shp(arrayBuffer).then(
      geojson => {
        let isGeographic = isGeometryGeographicHack(geojson);
        if (isGeographic) {
          geojson = toMercator(geojson);
        }
        let buffered = self.format.geojson.readFeatures(geojson);
        return buffered;
      },
      reason => {
        let msg = reason.message;
        switch (reason.message) {
          case "forgot to pass buffer":
            msg = "Shapefile reader needs a valid ArrayBuffer to read from.";
            break;
          case "I don't know that shp type":
            msg = `Tried to parse an invalid or unsupported shape type.
              To create a project area, please upload a shapefile with only features 
              of the supported types: Point, MultiPoint, LineString, MultiLineString, 
              Polygon, or MultiPolygon.`;
            break;
          case "no layers founds":
            msg = `The uploaded file must be a zip file containing, at minimum, 
              the following extensions: shp, dbf, prj.`;
            break;
          default:
            msg = `The uploaded file must be a zip file containing, at minimum, 
              the following extensions: shp, dbf, prj.`;
            break;
        }
        //TODO: Return the error to elm land here
        return null;
      }
    );
  }
}
