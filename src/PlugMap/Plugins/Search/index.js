
import Feature from "ol/Feature";
import Point from "ol/geom/Point";
import {Vector as VectorLayer} from "ol/layer";
import VectorSource from "ol/source/Vector";
import Icon from "ol/style/Icon"
import Style from "ol/style/Style";
import {createEmpty, extend} from "ol/extent.js"

/**
 * Zoom to candidate from Search result.
 * scope - OL Map
 * @param {Object} candidate
 */


export default class Search {
  constructor() {
    this.core = null;
  }

  apply(core) {
    
    this.core = core;
    this.setupPorts(core);
  }

  setupPorts(core) {
    core.mapCmd("gotoSearchCandidates", val => {
      this.gotoSearchCandidates(val);      
    })
    core.mapCmd("gotoSearchCandidate", val => {
      this.gotoSearchCandidate(val);      
    })
    core.mapCmd("setPoints", val => {
      this.setPoints(this, val);
    })
    core.mapCmd("moveMapCenter", val => {
      this.moveMapCenter(val);      
    })
       
    core.mapCmd("clearSearchCmd", this.clearPins.bind(this));
  }

  gotoSearchCandidate(candidate) {
    if (!candidate) return;
  
    let points = [];
    var marker = new Feature({
      geometry: new Point([candidate.location.x, candidate.location.y])
    });
    points.push(marker);
  
    let ext = this._transformSearchExtent(candidate.extent);
    
    this.core.data.map.getView().fit(ext, {
      size: this.core.data.map.getSize(),
      duration: 1000
    });
  
    setPoints(this, points);
  }

  
  moveMapCenter(feature) {
    if (!feature) return;      
    this.core.data.map.getView().setCenter([feature[0], feature[1]]);  
    this.core.data.map.getView().setZoom(11);
  }

  setPoints(that, points) {
    let layer = this.getOrMakeLayer(that);
    var src = layer.getSource();
    src.clear();
    if (points && points.length) src.addFeatures(points);
  }
  
  clearPins() {
    let layer = this.getOrMakeLayer(this);
    var src = layer.getSource();
    src.clear();
  }

  gotoSearchCandidates(val) {
    
    let candidates = val.candidates;
    let paddingLeft = val.paddingLeft;
    let paddingBottom = val.paddingBottom;

    let pudding = [5, 5, paddingBottom + 5, paddingLeft + 5 ]

    if (!candidates || candidates.length < 1) return;
    let self = this;
    let ext = createEmpty();
    let points = [];
    candidates.map(candidate => {
      console.log (candidate.extent);
      let transformed = this._transformSearchExtent(candidate.extent);
      console.log ("transformed");
      console.log (transformed);
      extend(ext, transformed);
      var marker = new Feature({
        geometry: new Point([candidate.location.x, candidate.location.y])
      });
      points.push(marker);
    });
  
    this.setPoints(this, points);
  
    this.core.data.map.getView().fit(ext, {
      size: this.core.data.map.getSize(),
      duration: 1000,
      padding: pudding
    });
  }
  getOrMakeLayer(that) {
    if (!this.core.data.map.pinLayer) {
      let vectorSource = new VectorSource({
        features: []
      });
      var iconStyle = feat => {
        return  new Style({
          image: new Icon({
            src : "./MapMarker.svg",
            scale : 1
          })
        });
      }
    
      let vectorLayer = new VectorLayer({
        source: vectorSource,
        style: iconStyle
      });
      vectorLayer.setZIndex(99999);   
      vectorLayer.setProperties("name", "pin");
      vectorLayer.setProperties("banPixel", true);
      
      this.core.data.map.addLayer(vectorLayer);
      
      this.core.data.map.pinLayer = vectorLayer;
      
    }
  
    return this.core.data.map.pinLayer;
  }
  _transformSearchExtent(ext) {
    
    if (!ext) return [];
    console.log ("_transformSearchExtent");
    return [ext.xmin, ext.ymin, ext.xmax, ext.ymax];
  }
  
  init() {}
  render() {}
}

