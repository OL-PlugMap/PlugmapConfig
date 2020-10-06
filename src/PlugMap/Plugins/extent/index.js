import { createEmpty, extend } from "ol/extent";
import { transform } from "ol/proj";
import WKT from "ol/format/wkt";

// This plugin adds a port for core to receive extent/zoom level from outside world (e.g rest api, ...)

export default class Extent {
  constructor() {
    //Best practices state that you should set up the objects properties here
    this.core = null;
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
    core.mapCmd("extentToPointZoomCmd", val => {
      this.extentToPointZoom(val);
    });
    core.mapCmd("extentToBoundingBoxCmd", val => {
      this.extentToBoundingBox(val);
    });
    core.mapCmd("extentToWKTCmd", val => {
      this.extentToWKT(val);
    });
  }

  extentToPointZoom(pointZoom) {
    //TODO: Your implementation goes here!
    
    //core emit will try to push data over a port if it exists
    //If it does not exist nothing happens
    //The second parameter is optional
    //If it is not set then null is sent

    var map = this.core.data.map;

    map
      .getView()
      .setCenter(
        transform([pointZoom.x, pointZoom.y], "EPSG:4326", "EPSG:3857")
      );
    map.getView().setZoom(pointZoom.zoom);

    let someValue = {};
    this.core.emit("extentSubscription", someValue);
  }

  extentToBoundingBox(bbox) {
    //TODO: Your implementation goes here!
    var newbbox = [bbox.xmin, bbox.ymin, bbox.xmax, bbox.ymax];

    var map = this.core.data.map;
    map.getView().fit(newbbox);

    let someValue = {};
    this.core.emit("extentSubscription", someValue);
  }

  extentToWKT(WKTs) {
    var map = this.core.data.map;
    var combinedExtent = createEmpty();
    for (let wkt of WKTs) {
      try {
        let newGeometry = undefined;
        //toWgs84
        var format = new WKT();
        if (wkt) {
          
          newGeometry = format.readGeometry(wkt, {
            dataProjection: 'EPSG:3857',})
          extend(combinedExtent, newGeometry.getExtent());
        }
      } catch (err) {
        console.error(`An error occurred from extentToWKT`);
      }
    }
    map.getView().fit(combinedExtent); //, { constrainResolution: false }
    //map.getView().setZoom(map.getView().getZoom() + 2);
    

    let someValue = {};
    this.core.emit("extentSubscription", someValue);
  }

  init() {}

  render() {
    //This will be used in the future if we need to add some sore of custom element
    const el = document.createElement("div");
    return el;
  }
}
