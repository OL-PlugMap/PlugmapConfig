import { Vector as VectorLayer, Group as LayerGroup } from "ol/layer";

import VectorSource from "ol/source/Vector";

import { Style, Fill, Stroke } from "ol/style";

import CircleStyle from "ol/style/Circle";

import WKT from "ol/format/WKT";

import ImageLayer from "ol/layer/Image";

import { ImageArcGISRest } from "ol/source";

import Feature from "ol/Feature";

import WKTizer  from "../../Common/WKTizer";

import { Draw, Modify, Snap, Select, DragAndDrop } from "ol/interaction.js";
import { GPX, GeoJSON, IGC, KML, TopoJSON } from "ol/format.js";


//import { altKeyOnly } from "ol/events/condition"

const CalcAreaWorker = require("../../Common/calc-area.worker.js");
import {altKeyOnly, never} from 'ol/events/condition.js';

const POINT = "Point";
const LINE = "LineString";

export default class DrawingPlugin {
  constructor() {

    this.source = null;
    this.display = null;
    this.layer = null;
    this.displayLayer = null;

    this.worker = new CalcAreaWorker();

    this.worker.onmessage = this.workerEventHandler.bind(this);


    this.drawLayer =
      { olLayer: null,
        olSource: null,
        initialized: false
      }

    this.referenceLayer =
      { olLayer: null
      , olSource: null
      , initialized: false
      , layer : null
      , renderLayer : null
      }
    
    this.mergedLayer =
      { olLayer: null,
        olSource: null,
        initialized: false
      }
    
    this.selectedLayer =
      { olLayer: null,
        olSource: null,
        initialized: false
      }
    
    this.highlightLayer =
      { olLayer: null,
        olSource: null,
        initialized: false,
      }

    
    this.tools =
      { draw: null,
        snap: null,
        modify: null,
        select: null
      }
    

    
    this.options = null;
    this.buffer = 50.0;

    this.drawingModes = {
      polygon: "Polygon",
      line: "LineString",
      point: "Point",
      circle: "Circle"
    };

    this.drawingMode = this.drawingModes.polygon;

    
    this.format = {
      unifiedWKTizer: new WKTizer(), //TODO
      wkt: new WKT() 
    };


    //Uncomment this to enable verbosity 
    this.debug = true;
  }

  workerEventHandler(e) {
    if(this.debug) console.log("We got a worker message: \"" + e.data.type + "\"");

    //All work should end with done. Every done event should have a source tied to it so that we know what to do with it.

    switch (e.data.type) {
      case "done":
        switch(e.data.source)
        {
          case "drawingEnd": {
            if(this.debug) console.log("Drawing End");
            if(this.debug) console.log(e);

              for(var i = 0; i < e.data.data.length; i++)
              {
                let evt = {type_:"Polygon", acres:e.data.data[i].acres, wkt:e.data.data[i].wkt, id: e.data.data[i].id };
                if(this.debug) console.log(evt);
                this.core.emit("drawingEnded", evt );
              }
          } break;
          case "modifyEnd": {
            if(this.debug) console.log("Modify End");
            if(this.debug) console.log(e);
            let modifiedShapes = [];
                
            for(var i = 0; i < e.data.data.length; i++)
            {
              let wkt = e.data.data[i].wkt;
              let area = e.data.data[i].acres;
              let id = e.data.data[i].id;
              let ol_uid = e.data.data[i].ol_uid;

              let result = 
                { type_:"Polygon", wkt: wkt, acres:area, id:id, ol_uid:ol_uid}

                modifiedShapes.push(result);
            }
            let result = { modifiedShapes:modifiedShapes };
            if(this.debug) console.log(result);
            this.core.emit("modifyEnd", result );
          } break;
          case "flatten": {
            if(this.debug) console.log(e);
              if(!e.data.data)
              {
                let result = null;
                this.core.emit("flattened", result );
              }
              else
              {
                var geojson = e.data.data.flattenedGeoJSON;
                
                let flattened = this.format.unifiedWKTizer.geojsonToOl(geojson);
                this.flattenedGeoJSON = geojson;
                let flatFeat = new Feature(flattened);
                this.mergedLayer.olSource.addFeature(flatFeat);
                  
                let result = { type_:"Polygon", wkt:e.data.data.flattenedWKT, acres:e.data.data.flattenedAcres, id: -1 };
                if(this.debug) console.log(result);
                this.core.emit("flattened", result );
              }
          } break;
          case "init": {
            if(this.debug) console.log(e);
              if(!e.data.data)
              {
                let result = null;
              }
              else
              {
                var geojson = e.data.data.flattenedGeoJSON;
                
                let flattened = this.format.unifiedWKTizer.geojsonToOl(geojson);
                this.flattenedGeoJSON = geojson;
                let flatFeat = new Feature(flattened);
                this.mergedLayer.olSource.addFeature(flatFeat);
              }
          } break;
          default: debugger; break;
        }
        
        break;
      
      
      case "openingFile": {
          this.core.emit("statusUpdate", { status : "Opening" });
      } break;

      case "processingFile": {
        this.core.emit("statusUpdate", { status : "Opening" });
      } break;

      case "processingFeatures": {
        this.core.emit("statusUpdate", { status : "Converting" });
      } break;

      case "calculating": {
        this.core.emit("statusUpdate", { status : "Calculating" });
      } break;

      case "calculated": {

        var ent = { features : e.data.result }
        
        this.core.emit("drawingStarted");
        var toAdd = [];
        for(var f of ent.features)
        {
          try
          {
            let olFeature = this.format.unifiedWKTizer.geojsonToOl(f.feature);
            this.flattenedGeoJSON = f;
            let feat = new Feature(olFeature);
            this.drawLayer.olSource.addFeature(feat);
            
            this.nextID++;
            feat.set("trackingID", this.nextID);
            f.feature.properties.source = "upload"
            feat.set("geoJSON", f.feature);
            let evt = {type_:"Polygon", acres:f.area, wkt:f.wkt, id: this.nextID };

            toAdd.push({evt:evt, feat: feat});
          }
          catch(ex) 
          {
            debugger;
          }
        }
        
        this.addList(toAdd);
        
        //this.clearAndResetDisplayLayer();
        this.core.emit("fileUploaded", null);
        

      } break;
      case "error":
          if(this.debug) console.log("error");
          if(this.debug) console.log(e);
        //this.core.emit("drawingEnded", { type_:"Polygon",acres:0, wkt:thisWKT, flat:flattenedWKTs, flatArea:0, id: this.nextID }) ;
        break;
      case "uploadError":
          if(this.debug) console.log(JSON.stringify(e));
          this.core.emit("statusUpdate", { status : { error : e.data.error }});
      default:
        break;
    }
  }


  addList(list) {
    let evt = list.shift();
    this.core.emit("drawingEnded", evt.evt );

    if(list.length == 0)
    {
      this.clearAndResetDisplayLayer();
      let view = this.map.getView();
      if (view) {
        // if (evt.feat) {
        //   view.fit(evt.feat.getGeometry(), {
        //     size: this.map.getSize(),
        //     duration: 1000
        //   });
        // }
      }
    }
    else
    {
      setTimeout(() => {
        this.addList(list);
      }, 5);
    }
  }
  setupLayers() {

    //Setup the sources if they are not
    if(!this.drawLayer.olSource) this.drawLayer.olSource = new VectorSource();
    if(!this.mergedLayer.olSource) this.mergedLayer.olSource = new VectorSource();
    if(!this.selectedLayer.olSource) this.selectedLayer.olSource = new VectorSource();
    if(!this.referenceLayer.olSource) this.referenceLayer.olSource = new VectorSource();
    if(!this.highlightLayer.olSource) this.highlightLayer.olSource = new VectorSource();


    //Setup the layers if they are not
    if(!this.drawLayer.olLayer) 
    {
        this.drawLayer.olLayer = this.newLayer("rgba(255, 255, 255, 0.2)", "#333", this.drawLayer.olSource, 1001);
        this.map.addLayer(this.drawLayer.olLayer);
    }

    if(!this.mergedLayer.olLayer)
    {
      this.mergedLayer.olLayer = this.newLayer("rgba(255, 255, 255, 0.2)", "#39c", this.mergedLayer.olSource, 1002);
      this.map.addLayer(this.mergedLayer.olLayer);
    }

    if(!this.selectedLayer.olLayer)
    {
      this.selectedLayer.olLayer = this.newLayer("rgba(255, 255, 255, 0.2)", "#5BE", this.selectedLayer.olSource, 1003); 
      this.map.addLayer(this.selectedLayer.olLayer);
    }

    if(!this.highlightLayer.olLayer)
    {
      this.highlightLayer.olLayer = this.newLayer("rgba(255, 255, 200, 0)", "#FF3", this.highlightLayer.olSource, 1004, "rgba(255, 255, 200, 0.75)", 6, 0, "rgba(255, 255, 200, 0)") ; 
      this.map.addLayer(this.highlightLayer.olLayer);
    }

    if(!this.referenceLayer.olLayer)
    {
      let vectorLayer = this.newLayer("rgba(220, 220, 255, 0.2)", "#5BE", this.referenceLayer.olSource, 91004);
      this.referenceLayer.olLayer = new LayerGroup();
      this.referenceLayer.olLayer.getLayers().push(vectorLayer);
      this.map.addLayer(this.referenceLayer.olLayer);
    }
  }

  destructLayers()
  {
    if(this.selectedLayer.olLayer)
      this.map.removeLayer(this.selectedLayer.olLayer);

      if(this.referenceLayer.olLayer)
        this.map.removeLayer(this.referenceLayer.olLayer);

    if(this.mergedLayer.olLayer)
      this.map.removeLayer(this.mergedLayer.olLayer)

    if(this.drawLayer.olLayer)
      this.map.removeLayer(this.drawLayer.olLayer)


    if(this.drawLayer.olSource) this.drawLayer.olSource = null;
    if(this.mergedLayer.olSource) this.mergedLayer.olSource = null;
    if(this.selectedLayer.olSource) this.selectedLayer.olSource = null;
    if(this.referenceLayer.olSource) this.referenceLayer.olSource = null;


    if(this.selectedLayer.olLayer)
      this.selectedLayer.olLayer = null;

    if(this.referenceLayer.olLayer)
      this.referenceLayer.olLayer = null;

    if(this.mergedLayer.olLayer)
      this.mergedLayer.olLayer = null;

    if(this.drawLayer.olLayer)
      this.drawLayer.olLayer = null;

  }

  newLayer(fill, stroke, source, zIndex, selectedFillStyle, selectedStrokeWidth, unselectedStrokeWidth, unselectedStrokeStyle)
  {
    if(!selectedFillStyle)
    {
      selectedFillStyle = "rgba(255,255,255,0.3)";
      selectedStrokeWidth = 5;
      unselectedStrokeWidth = 2;
      unselectedStrokeStyle = stroke;
    }

    return new VectorLayer({
      source: source,
      zIndex: zIndex,
      style: function(feat)
      { return new Style({
        fill: new Fill({
          color: feat.get("selected") ? selectedFillStyle : fill
        }),
        stroke: new Stroke({
          color: feat.get("selected") ? stroke : unselectedStrokeStyle,
          width: feat.get("selected") ? selectedStrokeWidth : unselectedStrokeWidth
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

  setupTools() {
    if(!this.tools.draw)
    {
      this.tools.draw = new Draw({
        source: this.drawLayer.olSource,
        type: this.drawingMode
      });

      this.tools.draw.on("drawstart", this.onDrawStartHandler.bind(this));
      this.tools.draw.on("drawend", this.onDrawEndHandler.bind(this));
    }

    if(!this.tools.snap)
    {
      this.tools.snap = new Snap({ source: this.drawLayer.olSource });
    }

    if(!this.tools.select)
    {
      this.tools.select = new Select({
        layers: [this.drawLayer.olSource],
        multi: true,
        hitTolerance: 2,
        condition: never
      });
  
      this.tools.select.on("select", this.onSelectHandler.bind(this));
    }

    if(!this.tools.modify)
    {
      this.tools.modify = new Modify({
        source: this.drawLayer.olSource,
        deleteCondition: altKeyOnly
      });


      //Monkey patch for a bug in modify
      this.tools.modify.handleUpEvent_old = this.tools.modify.handleUpEvent;
      this.tools.modify.handleUpEvent = function (evt) { try { this.handleUpEvent_old(evt); } catch(ex) { debugger; } }
  
      this.tools.modify.on("modifystart", this.onModifyStartHandler.bind(this));
      this.tools.modify.on("modifyend", this.onModifyEndHandler.bind(this));
    }
  }  

  destructTools() 
  {
    if(this.tools.draw)
    {
      this.map.removeInteraction(this.tools.draw);

      this.tools.draw.un("drawstart");
      this.tools.draw.un("drawend");
      
      this.tools.draw = null;
    }

    if(this.tools.snap)
    {
      this.map.removeInteraction(this.tools.snap);

      this.tools.snap = null;
    }

    if(this.tools.select)
    {
      this.map.removeInteraction(this.tools.select);

      this.tools.select.un("select");

      this.tools.select = null;
 
    }

    if(this.tools.modify)
    {
      this.apply.map.removeInteraction(this.tools.modify);
  
      this.tools.modify.un("modifystart");
      this.tools.modify.un("modifyend");

      this.tools.modify = null
    }
  }


  onDrawStartHandler(event) {
    this.core.emit("drawingStarted", null);
    this.isDrawing = true;
    this.drawingFeature = event.feature;
  }

  onDrawEndHandler(event, ref) {
    try
    {
      if(this.nextID == undefined || this.nextID == null)
      {  
        console.warn("We ended drawing without an expected ID ...");
        this.nextID = 0;
      }

      this.nextID ++;

      this.core.emit("drawingEnd", null);
      this.isDrawing = false;
      
      this.drawingFeature = null;

      let newFeat = event.feature;
      newFeat.set("trackingID", this.nextID);
      newFeat.set("buffer", this.buffer);
      newFeat.set("source", "draw");
      let newFeat2 = newFeat;
      event.feature = newFeat;
      this.lastFeature = newFeat;
      let geo = newFeat.getGeometry();
      this.disableDraw();
      this.enableModify();


      var thisGeoJSON = this.format.unifiedWKTizer.olToGeoJSONSimple(newFeat);
      thisGeoJSON.properties.source = "draw"
      newFeat.set("geojson", thisGeoJSON);

      this.clearAndResetDisplayLayer(newFeat);

      let worker = this.worker;
      
      worker.postMessage({
        type: "start",
        id: -1,
        source: "drawingEnd",
        wkts: [ { geoJSON: thisGeoJSON, id:this.nextID} ]
      });

      
      
      
      // let thisWKT = null, thisGeoJSON = null;

      // if(newFeat.get("wkt"))
      // {
      //   thisWKT = newFeat.get("wkt");
      // }
      // else
      // {
        
      //   //thisWKT = this.format.unifiedWKTizer.olToGeoJSONSimple(newFeat); 
      // }

      // if(newFeat.get("geojson"))
      // {
      //   thisGeoJSON = newFeat.get("geojson");
      // }
      // else
      // {
        
      // }

      //This will merge the features as well
      //this.clearAndResetDisplayLayer(newFeat);
      
      
      //

      //let flattenedWKTs = this.flattenedWKT;

      

      
      
    }
    catch(ex)
    {
      console.warn("Ran into a problem with this shape and prevented a crash.");
      console.warn(ex);
      console.warn(event);
    }

  }

  addOl(f)
  {
    this.onDrawStartHandler({feature: f});
    this.onDrawEndHandler({feature: f});
    this.drawLayer.olSource.addFeature(f);
  }

  addReferenceItems(wkts) {

    this.core.emit("drawingStarted", null);

    let fts = [];

    let feats = this.referenceLayer.olSource.getFeatures();

    for(var f of feats)
    {
      this.drawLayer.olSource.addFeature(f);
      this.nextID ++;
      f.set("trackingID", this.nextID);
      fts.push({geoJSON: f.get("geoJSON"), id: this.nextID } );
    }

    this.referenceLayer.olSource.clear();
    this.referenceLayer.olLayer.getLayers().remove(this.referenceLayer.renderLayer);
    this.referenceLayer.renderLayer = null;
    


    this.clearAndResetDisplayLayer();

    //this.resetModifyFeaturesToSource();

    let worker = this.worker;

    worker.postMessage({
      type: "start",
      source: "drawingEnd",
      id: -1,
      wkts: fts
    });
    
      
    
  }



  addWKTs(wkts) {
    
    this.core.emit("drawingStarted", null);

    for(var wkt of wkts)
    {
      var feat = this.format.unifiedWKTizer.wktToOl(wkt.wkt, true);
      var f = new Feature({geometry: feat});
      this.onDrawStartHandler({feature: f});
      this.onDrawEndHandler({feature: f});
      this.drawLayer.olSource.addFeature(f);

      let view = this.map.getView();
          if (view) {
            if (feat) {
              // view.fit(feat, {
              //   size: this.map.getSize(),
              //   duration: 1000
              // });
            }
          }
    }
    
      
    
  }

  selectById(id) {
    
    let features = this.drawLayer.olSource.getFeatures();

    let found = undefined;

    for(let feat of features)
    {
      if(feat.get("trackingID") == id)
      {
        found = feat;
        //break;
      }
      else{
        if(feat.get("selected"))
          feat.set("selected", false);
      }
    }

    if(found)
    {
      if(found.get("selected"))
        found.set("selected", false);
      else
        found.set("selected", true);
    }

    this.resetModifyFeaturesToSource();
  }

  onSelectHandler(event) {
    if (event.selected.length == 0) {
      //In this event the user has clicked off of a feature
      //Deselect and return
      this.setModifyFeaturesToSource(event.selected);
      this.featureSelected = null;
      return;
    }

    //Check to see if the user is drawing
    if (this.isDrawing || this.drawingMode == "Point") {
      /*
        Drawing has started already.
        Two situations that can arise here
        Case one: The user started drawing by clicking inside of an existing poly
          In this case we want to abort drawing and instead handle the select and start modifying
        Case two: The user started drawing outside of a poly and then clicked inside
          In this case we dont want to set the clicked feature to modify
        */

      if (this.drawingFeature) {
        let geo = this.drawingFeature.getGeometry();
        let coords = geo.getCoordinates();

        //This would be case two
        //TODO SWAP ON GEO TYPE, POLY > 3 Point meh line > 1

        let maxPoints = 1;
        switch (geo.getType()) {
          case POINT:
            maxPoints = 9999;
            break;
          case LINE:
            maxPoints = 2;
            break;
          default:
            maxPoints = 3;
            coords = coords[0];
        }

        if (coords && coords.length > maxPoints) {
          //Abort the select
          //this.modify.features_.clear();
          return;
        }
      }
    }

    //This would be case one
    //If we are not drawing or we have started drawing inside a poly, select the poly

    this.setModifyFeaturesToSource(event.selected);

    if (event.selected && event.selected.length) this.tools.draw.abortDrawing_();

    this.featureSelected = event.selected;
  }

  onModifyStartHandler(event) {
    this.core.emit("drawingStarted", null);
    this.revisions = {};
    for(let feat of event.features.array_)
    {
      this.revisions[feat.ol_uid+""] = feat.revision_;
    }
  }

  onModifyEndHandler(event) {
    try
    {
      this.core.emit("drawingEnded", null);
      if(!this.revisions) return;

      //Build a list of items to be quackulated
      //Also add in the flat or whatever
      

      let featuresModifiedWKT = [];
      let featuresModifiedUID = [];
      let allFeatures = [];
      let allModifiedIds = [];

      for(let feat of event.features.array_)
      {
        if(feat.revision_ > this.revisions[feat.ol_uid+""])
        {
          
          
          let newFeat = feat;
          let nextID = newFeat.get("trackingID");
          feat.set("source", "draw");
          
        

          this.lastFeature = newFeat;
          let geo = newFeat.getGeometry();
          let newFeat2 = newFeat;

          
          //let thisWKT = this.format.unifiedWKTizer.convertFromOpenLayersFeature(newFeat);
          let thisGeoJSON = this.format.unifiedWKTizer.olToGeoJSONSimple(newFeat);
          let oldgj = newFeat.get("geoJSON");
          if(oldgj)
            thisGeoJSON.properties.source = oldgj.properties.source || "unknown"
          newFeat.set("geoJSON", thisGeoJSON);
      
          
          allModifiedIds.push(nextID);
          featuresModifiedWKT.push({ geoJSON: thisGeoJSON, id: nextID, ol_uid: newFeat.ol_uid });
          featuresModifiedUID.push(newFeat.ol_uid);
          allFeatures.push(newFeat);
        }
        else
        {
          allFeatures.push(feat);
        }

      }

      this.clearAndResetDisplayLayer();

      let worker = this.worker;
      worker.postMessage({
        type: "start",
        id: -1,
        source: "modifyEnd",
        wkts: featuresModifiedWKT
      });
    }
    catch(ex)
    {
      console.warn("Ran into a problem with this shape and prevented a crash");
      console.warn(ex);
      console.warn(event);
    }

  }

  clearAndResetDisplayLayer(addlFeat, skipFlatten)
  {
    let map = this.map;

    let allFeatures = this.drawLayer.olSource.getFeatures();
    
    if(addlFeat)
      allFeatures.push(addlFeat);

    let newFeatures = [];
    this.mergedLayer.olSource.clear();

    let geoFeats = [];

    for(let feature of allFeatures)
    {
      let geo = feature.getGeometry();
      let newFeat2 = feature;
      let nextID = feature.get("trackingID");
      newFeat2.set("psudo", true);
      newFeat2.set("buffer", feature.get("buffer"));

      
      
      if(newFeat2.values_.geometry)
      {  
        newFeatures.push(newFeat2);
        //Convert if the geojson doesnt exist
        if(!feature.get("geoJSON"))
        {
          var gj = this.format.unifiedWKTizer.olToGeoJSONSimple(feature)
          feature.set("geoJSON", gj)
        }

        let geoJSON = feature.get("geoJSON"); // || this.format.unifiedWKTizer.olToGeoJSONSimple(feature);
        geoJSON.properties["source"] = feature.get("source")
        geoFeats.push(geoJSON);
      }
    }

    this.flattenedGeoJSON = null;
    
    let worker = this.worker;
    worker.postMessage({
        type: "flatten",
        id: -1,
        source: skipFlatten ? "init" : "flatten",
        features: geoFeats
      });

  }

  //TODO: Setup the selected layer when an item is selected
  

  resetModifyFeaturesToSource() {
    var af = this.drawLayer.olSource.getFeatures();

    var selected = [];

    this.selectedLayer.olSource.clear();
    this.tools.modify.features_.clear();

    for(var f of af)
      if(f.get("selected"))
        selected.push(f);

    if(selected.length == 0)
    {
      
      for(var f of af)
        this.tools.modify.features_.push(f);
        return;
    }

    for(var f of selected)
    {
      this.tools.modify.features_.push(f);
      this.selectedLayer.olSource.addFeature(f);
    }

  }

  setModifyFeaturesToSource(addlFeats) {
    
    if(!this.tools.modify)
      return;
    
    if ( this.tools.modify.features_.array_.length == 1 )
      if ( this.tools.modify.features_.array_[0].get("trackingID") == addlFeats[0].get("trackingID") )
      {
        this.tools.modify.features_.clear();
        var af = this.drawLayer.olSource.getFeatures();
        for(var f of af)
          this.tools.modify.features_.push(f);
        return;    
      }

    this.tools.modify.features_.clear();
    
      if(!this.tools.select)
        return;

    let feats = this.tools.select.getFeatures();

    if(!feats || !feats.array_ || !feats.array_.length)
    {
      feats = [];
    }

    if(feats && feats.array_ && feats.array_.length)
    {
      for (let feat of feats.array_) {
        this.tools.modify.features_.push(feat);
      }
    }
    else if (feats)
    {
      for (let feat of feats) {
        this.tools.modify.features_.push(feat);
      }
    }

    if (addlFeats) {
      for (let feat of addlFeats) {
        this.tools.modify.features_.push(feat);
      }
    }
  }

  

  
  xmlToJs(val) {
    var converted = xmlConvert.xml2json(val.xml, {compact: true, spaces: 4});
    val.result = JSON.parse(converted);
    this.core.emit("xmlToJsResult", val);
  }

  normalizeEsri(val) {
    var parsed = JSON.parse(val.esri);
    let layer = this.referenceLayer.layer.config.esri;

    for(var i in parsed.features)
    {
      if(parsed.features[i].geometry)
      {
        let oldFeat = parsed.features[i];

        let fw = this.format.unifiedWKTizer.geojsonToWKTAndOl(oldFeat)
        let feat = fw.ol;

        if(feat && this.referenceLayer.olSource)
        {
          let idField = "id";

          if(layer.idField)
            idField = layer.idField;

          let nf = new Feature({
            geometry: feat,
            id: oldFeat.properties[idField] || Math.floor(Math.random() * 9999)
          });
          this.referenceLayer.olSource.addFeature(nf);
          
          nf.set("geoJSON", Object.assign({}, oldFeat));

          let view = this.map.getView();
          if (view) {
            let geometry = nf.getGeometry();
            if (geometry) {
              // view.fit(geometry, {
              //   size: this.map.getSize(),
              //   duration: 1000
              // });
            }
          }
        }

        var wkt = fw.wkt;
        
        parsed.features[i].geometry = wkt;

        
      }
      if(!parsed.features[i].properties && parsed.features[i].attributes)
      {
        parsed.features[i].properties = parsed.features[i].attributes;
      }
    }

    
    val.value = parsed;

    this.core.emit("normalizeEsriResult", val);
  }


  selectFeature(id) {
  }

  deselectFeature(id) {
    var feats = this.referenceLayer.olSource.getFeatures();
    for(var feat of feats)
    {
      if(feat.get("id") == id)
      this.referenceLayer.olSource.removeFeature(feat);
    }
  }




  apply(core) {
    this.core = core;
    this.setupPorts(core);
    this.map = core.getMap();
  }

  setupPorts(core) {
    //TODO: Prepend these with drawing or something
    //      so that the name cannot be confused
    core.mapCmd("unloadDrawingPlugin", val => {
      core.unmapCmds([
        
      ]);
    });

    
    
    core.mapCmd("addReferenceItems", wkts =>{
      this.addReferenceItems(wkts);
    });

    core.mapCmd("setWKTs", val => {
      this.setWKTOnlyFromElmLandPlz(val);
    });

    core.mapCmd("setBuffer", val => {
      this.setBuffer(val.buffer);
    });
    

    core.mapCmd("startDrawingWithMode", val => {
      this.startDrawingWithMode(val);
    });


    core.mapCmd("enableModify", val => {
      this.enableModify(val);
    });

    core.mapCmd("disableModify", val => {
      this.disableModify(val);
    });
    
    core.mapCmd("disableDraw", val => {
      this.disableDraw(val);
    });

    core.mapCmd("selectById", val => {
      this.selectById(val);
    })

    core.mapCmd("deleteFeatureById", val => {
      this.deleteById(val);
    })

    core.mapCmd("setExtentByFeatueId", val => {
      this.setExtentByFeatureId(val);
    })

    core.mapCmd("initDrawingCmd", options => {
      this.init(options);
    })

    core.mapCmd("destructDrawingCmd", options => {
      this.destruct(options);
    })

    core.mapCmd("hideDrawingCmd", options => {
      this.hide()
    })


    /*FP Commands*/

    
    core.mapCmd("featurePickerSetLayer", val => {
      //this.init();
      this.showReferenceLayer(val);
    })

    core.mapCmd("hideReferenceLayer", val => {
      //this.init();
      this.showReferenceLayer();
    })

    core.mapCmd("hideReferenceLayerDisplay", v => {
      this.hideFP();
    })



    core.mapCmd("featurePickerDisable", val => {
      this.denitFP();
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

    core.mapCmd("clearReferenceFeatures", id => {
      this.clearReferenceFeatures();
    })

    core.mapCmd("normalizeEsri", val => {
      this.normalizeEsri(val);
    })


    core.mapCmd("addWKTsToMap", val => {
      this.addWKTs(val);
    })

    core.mapCmd("highlightCmd", val => {
      this.highlight(val);
    })

    core.mapCmd("unhighlightCmd", val => {
      this.unhighlight(val);
    })

    core.mapCmd("clearHighlightLayerCmd", val => {
      this.clearHighlightLayer();
    })

    core.mapCmd("openChooseFile", val => {
      this.upload_init();
      this.upload_enableDragAndDrop();
      this.upload_manualUpload(val);
    })
  }


  clearHighlightLayer() {
    this.highlightLayer.olSource.clear();
  }

  highlight(data) {
    let feats = this.highlightLayer.olSource.getFeatures();
    let f = null;

    for(var feat of feats)
    {
      if(feat.get("trackingID") == data.id)
      {
        f = feat;
        break;
      }
    }
    if(!f)
    {
      var feat = this.format.unifiedWKTizer.wktToOl(data.wkt, true);
      f = new Feature({geometry: feat});
      f.set("trackingID", data.id);
      this.highlightLayer.olSource.addFeature(f);
    }
    
    f.set("selected", true);
  }

  unhighlight(data) {
    let feats = this.highlightLayer.olSource.getFeatures();
    let f = null;

    for(var feat of feats)
    {
      if(feat.get("trackingID") == data.id)
      {
        f = feat;
        break;
      }
    }
    if(f)
    {
      f.set("selected", false);
    }
  }

  clearReferenceFeatures() {
    if(this.referenceLayer.olSource) this.referenceLayer.olSource.clear();
  }
  denitFP() {
    
    this.showReferenceLayer();
    if(this.referenceLayer.olSource) this.referenceLayer.olSource.clear();
    
  
  }

  hideFP() {
    if(this.referenceLayer.olLayer)
    this.referenceLayer.olLayer.setVisible(false);
  }

  showReferenceLayer(layer) {
    
    var map = this.core.getMap();
    if (map) {

      if(this.referenceLayer.olSource) this.referenceLayer.olSource.clear();
      
      this.map = map;
      
      //This will add the layers to the map if they are not already setup
      this.setupLayers();

      //This will setup the tools
      this.setupTools();

      this.referenceLayer.layer = layer;

      //TODO: Remove the tile/image layer from the referenceLayer
      if(this.referenceLayer.renderLayer)
      {
        this.referenceLayer.olLayer.getLayers().remove(this.referenceLayer.renderLayer);
        this.referenceLayer.renderLayer = null;
      }


      this.core.pumpCmd('revert');

      if(!layer)
        return;
    


      var renderLayer = null;
      if(layer.key)
      {
        this.core.pumpCmd('save');
        
        this.core.pumpCmd('toggleSelectedThemesCmd', [ {
          "category_key": "cat_reference",
          "selection": {
            "selection_type": "monoselection",
            "selection_key": layer.key
          } } ] )

          
      }
      else
      {
        if(layer.config.wmts)
        {
          renderLayer = this.newWMTS(layer.config.wmts, this.core)
        }
        if(layer.config.wms)
        {
          //WMS/WFS layer
          renderLayer = this.newWMS(layer.config.wms, this.core)
        }
        else if (layer.config.esri)
        {
          renderLayer = this.newDynamic(layer.config.esri, this.core)
        }

        this.referenceLayer.olLayer.getLayers().push(renderLayer);
        this.referenceLayer.renderLayer = renderLayer;
      }
      

      if(!this.boundClick)
      { 
        this.map.on('singleclick', this.onclick.bind(this));
        this.boundClick = true;
      }

      if(this.referenceLayer.olLayer)
        this.referenceLayer.olLayer.setVisible(true);
    }
  }


  newDynamic(layer, core, stopProcessing)
  {
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
              this.addPending(this.addDynamic, layer);
            return;
          }
          
        }
    }    
    

    let dynamicLayer = new ImageLayer({
      visible: true,
      preload: 4,
      zIndex: 999,
      opacity: 0.7, //layer.opacity || 1,
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
    
    return dynamicLayer;
  }




  hide()
  {
    this.disableModify();
    this.disableDraw();

    if(this.drawLayer.olLayer)
      this.drawLayer.olLayer.setVisible(false);

    if(this.mergedLayer.olLayer)
      this.mergedLayer.olLayer.setVisible(false);

    if(this.selectedLayer.olLayer)
      this.selectedLayer.olLayer.setVisible(false);
  }

  init()
  {
    this.setupLayers();
   
    if(this.tools.draw)
    {
      this.draw.olSource.clear();
      this.mergedLayer.olSource.clear();
    }
    
    this.setupTools();

    if(this.drawLayer.olLayer)
      this.drawLayer.olLayer.setVisible(true);
      
    if(this.mergedLayer.olLayer)
      this.mergedLayer.olLayer.setVisible(true);

    if(this.selectedLayer.olLayer)
      this.selectedLayer.olLayer.setVisible(true);

    if(this.nextID == undefined)
      this.nextID = 0;
  }

  destruct()
  {
    this.destructTools();
    this.destructLayers();
  }


  setExtentByFeatureId(data) {
    var id = data.id;
    var leftPad = data.left;
    var bottomPad = data.bottom;

    let feats = this.drawLayer.olSource.getFeatures();


    let featsDeleted = 0;

    for(var f of feats)
    {
      if(f.get("trackingID") == id)
      {
        if (this.map) {
          let view = this.map.getView();
          if (view) {
            let geometry = f.getGeometry();
            
            let pudding = [5, 5, bottomPad + 5, leftPad + 5 ]

            if (geometry) {
              view.fit( geometry, {
                size: this.map.getSize(),
                duration: 1000,
                padding:  pudding
              });
            }
          }
        }
        break;
      }
    }
  }

  deleteById(id) {
    let feats = this.drawLayer.olSource.getFeatures();


    let featsDeleted = 0;

    for(var f of feats)
    {
      if(f.get("trackingID") == id)
      {
        this.drawLayer.olSource.removeFeature(f);
        featsDeleted ++;
      }
    }


    if(!featsDeleted)
      return;

    this.core.emit("drawingStarted", null);
    
    this.clearAndResetDisplayLayer();

    //this.resetModifyFeaturesToSource();

  }

  disableModify(options)
  {

    
    
    if(!this.tools.modify) return;

    this.map.removeInteraction(this.tools.modify);
    this.map.removeInteraction(this.tools.select);
    
    this.selectedLayer.olSource.clear();

    var features = this.drawLayer.olSource.getFeatures();

    for(let feat of features)
    {
      feat.set("selected", false);
    }
    

    this.modifyEnabled = false;
  }

  disableDraw()
  {
    this.tools.draw.abortDrawing_();
    this.map.removeInteraction(this.tools.draw);
  }

  enableModify(options) 
  {
    if(this.tools.modify) this.map.addInteraction(this.tools.modify);
    if(this.tools.select) this.map.addInteraction(this.tools.select);

    this.modifyEnabled = true;

    if(this.drawLayer.olLayer)
      this.drawLayer.olLayer.setVisible(true);
      
    if(this.mergedLayer.olLayer)
      this.mergedLayer.olLayer.setVisible(true);

    if(this.selectedLayer.olLayer)
      this.selectedLayer.olLayer.setVisible(true);
  }

  startDrawingWithMode(options)
  {
    //Setup some vars
    var map = this.core.getMap();
    if (map) {
      
      this.map = map;
      
      //This will add the layers to the map if they are not already setup
      this.setupLayers();

      //This will setup the tools
      this.setupTools();

      //Setup the drawing mode
      let lc = options.mode.toLowerCase();
      this.drawingMode = this.drawingModes[lc] || this.drawingModes.polygon; //Default to polygon if we cant match our modes
      
      if (this.tools.draw)
      {
        //We already had the draw setup, we should do something about it

        //First remove all the interactions
        this.tools.draw.finishDrawing();

        if (this.map && this.tools.draw) this.map.removeInteraction(this.tools.draw);

        if (this.map && this.tools.snap) this.map.removeInteraction(this.tools.snap);

        //Remove the hooks
        try{ this.tools.draw.un("drawstart"); } catch {}
        try{ this.tools.draw.un("drawend"); } catch {}

        //Clear some values
        this.tools.draw.source_ = null;
        this.tools.draw = null;
      }


      //Setup a new draw with the selected mode
      var draw = new Draw({
        source: this.drawLayer.olSource,
        type: this.drawingMode
      });


      //Setup the hooks
      draw.on("drawstart", this.onDrawStartHandler.bind(this));
      draw.on("drawend", this.onDrawEndHandler.bind(this));

      this.tools.draw = draw;
      
      this.map.addInteraction(this.tools.draw);
      this.map.addInteraction(this.tools.snap);
      


      //Save these for later when we enable modify
      // set up event listener for deleting drawn shapes
      //document.addEventListener("keydown", this.deleteFeature, false);
      this.drawEnabled = true;
      this.disableModify();
    }

    
    //this.nextID = options.id;
  }


  setBuffer(buffer) {
    this.buffer = buffer;
  }


  //TODO: Convert this functio to use geojson
  setWKTOnlyFromElmLandPlz(v) {
    //this.init();
    if(v.clear)
    {
      this.drawLayer.olSource.clear();
      this.nextID = 0;
    }

    for(let o of v.wkts) {
      let wkt = o.wkt;
      let id = o.id;
      try {
      let newGeometry = undefined;

      if (wkt && wkt.length) {
        newGeometry = this.format.wkt.readGeometry(wkt);

        let feature = new Feature({
          geometry: newGeometry
        });
  
        feature.set("trackingID", id);
        feature.set("source", "Server");

        if(!feature.get("geoJSON"))
        {
          var thisGeoJSON = this.format.unifiedWKTizer.olToGeoJSONSimple(feature);
          thisGeoJSON.properties.source = "server"
          feature.set("geoJSON", thisGeoJSON)
        }

          if(this.nextID == null || this.nextID == undefined || id > this.nextID)
            this.nextID = id;
  
        this.drawLayer.olSource.addFeature(feature);
      }


      } catch (err) {
        console.error(
          `An error occurred updating feature with new buffer ${err}`
        );
      }
    }

    this.clearAndResetDisplayLayer(null, true);

    let requestAnimationFrame =
        window.requestAnimationFrame ||
        window.mozRequestAnimationFrame ||
        window.webkitRequestAnimationFrame ||
        window.msRequestAnimationFrame;
      requestAnimationFrame(() => {
        //TODO: Make this extent to the state first

        if (this.map) {
          let view = this.map.getView();
          if (view) {
            let feats = this.drawLayer.olSource.getFeatures();
            if (feats && feats.length) {
              let geometry = feats[0].getGeometry();
              if (geometry) {
                // view.fit(feats[0].getGeometry(), {
                //   size: this.map.getSize(),
                //   duration: 2000
                // });
              }
            };
          }
        }
      });
    

    

  }


  setDrawingModePolygon() {
    this.drawingMode = this.drawingModes.polygon;
  }

  setDrawingModeLine() {
    this.drawingMode = this.drawingModes.line;
  }

  setDrawingModePoint() {
    this.drawingMode = this.drawingModes.point;
  }

  setDrawingModeCircle() {
    this.drawingMode = this.drawingModes.circle;
  }


  render() {
    return null;
  }



  //TODO: Move the requests into the elm side
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
    
    let layer = this.referenceLayer.layer;
  
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
  
      let url = baseURL + "/" + layerConfig.url + "/identify?geometry=" +
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
  
              let layer = that.referenceLayer.layer.config.esri;
  
  
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


















  upload_manualUpload(elementId) {
    if(this.debug) console.log("Manual upload top")
    elementId =  elementId + "";


    if (!this.dragAndDrop) 
    {
      if(this.debug) console.log("NO DND FOUND")
      return; //TODO
    }

    var elm = document.getElementById(elementId);

    if(elm)
    {
      document.removeChild(elm);
      elm = null;
    }

    if (!elm)
    {
      if(this.debug) console.log("No ELM")
      elm = document.createElement("input");
      elm.type = "file";

      elm.onchange = evt => {
        this.core.emit("statusUpdate", { status : "Opening" });
        this.upload_openFiles(elm);
      }

      
    }
    elm.click();

    if(this.debug) console.log("Manual upload bottom")
  }

  upload_clearLayer() {
    this.source.clear();
  }

  upload_getFeatureWKTs() {
    let wkts = this.format.convertFromSource(this.source, 10); //TODO: Get this from overworld
    this.core.emit("getUploadedFeatureWKTsResult", wkts);
  }

  upload_init() {
    let map = this.core.getMap();
    this.map = map;
    this.upload_initSource(map);
    this.upload_initLayer(map);
    this.upload_initShapeFileFormat(map);
    this.upload_initDragAndDrop(map);
  }

  upload_denit() {}

  upload_initSource() {
    if (!this.source) this.source = new VectorSource();
  }

  upload_initLayer(map) {
    this.upload_initSource();
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

  upload_initShapeFileFormat() {}

  upload_initDragAndDrop() {
    if (this.dragAndDrop) return;

    this.upload_initSource();

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
      me.upload_handleDDFile(file, oldEvent);
      
    };
  }


  upload_openFiles(elm) {

    var files = elm.files;

    let worker = this.worker;
    let errorLimit = 5;

    
    worker.postMessage({
      type: "processUpload",
      id: -1,
      files: files
    });
  }

  upload_handleDDFile(file, oldEvent) {



  }

  upload_addFeatures(event, that) {
    this.core.emit("statusUpdate", { status : "Converting" });

    var res = [];


    this.core.emit("statusUpdate", { status : "Calculating" });

    this.core.emit("fileUploaded", res); 
  }

  upload_enableDragAndDrop() {
    if (this.map && this.dragAndDrop) {
      this.map.addInteraction(this.dragAndDrop);
    }
  }
}
