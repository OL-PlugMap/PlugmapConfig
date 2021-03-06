import { bootMap, destructMap } from "./map-init";
import WKT from "ol/format/WKT";
import { transform } from "ol/proj";


export default class Core {
  constructor(elID) {
    this._plugins = [];
    this.subs = [];
    this.triggers = {};
    this.hooks = {
    };
    this.bindMap(elID);
    this.el = {
      itms: [],
      childrens: [],
      append: function(itm) {
        this.itms.push(itm);
      },
      appendChild: function(child) {
        this.childrens.push(child);
      }
    };
    this.services = {};
  }
  bindMap(elID) {
    let requestAnimationFrame =
      window.requestAnimationFrame ||
      window.mozRequestAnimationFrame ||
      window.webkitRequestAnimationFrame ||
      window.msRequestAnimationFrame;
    requestAnimationFrame(() => {
      var element = document.getElementById(elID);
      if (element) {
        var ph = this.el;
        this.el = element;
        for (var itm of ph.itms) this.el.append(itm);
        for (var child of ph.childrens) this.el.appendChild(child);
      } else {
        //TODO Handle error
      }
    });
  }
  init(config, elm, options) {
    //Add Map

    this.config = config;
    this.options = options || {
      target: "ol-map",
      center: [-12350000.17245, 4705131.1812],
      zoom: 7,
      maxZoom: 30,
      minZoom: 3
    };

    this.elm = elm;

    this._initPortSubs();
    this._bootMap(this.options);
    this.mapCmd("setServicesCmd", this.setServices.bind(this));
    this.mapCmd("fitToWktCmd", this.fitToWkt.bind(this));
    this.mapCmd("extentToPointZoomCmd", this.fitToPointZoom.bind(this));
    this.mapCmd("refreshMap", this.refresh.bind(this));


    var map = this.getMap();
    if(map)
    {
      map.getView().on("change", evt =>
        {
          var extent = map.getView().calculateExtent(map.getSize());
          var center = map.getView().getCenter();


          center = transform(center, "EPSG:3857", "EPSG:4326")
          var extent_1 = transform([extent[0],extent[1]], "EPSG:3857", "EPSG:4326")
          var extent_2 = transform([extent[2],extent[3]], "EPSG:3857", "EPSG:4326")

          var res =
            { xmin : extent_1[0]
            , ymin : extent_1[1]
            , xmax : extent_2[0]
            , ymax : extent_2[1]
            , center : 
              { x : center[0]
              , y : center[1]
              }
            }

          
          this.emit("extentUpdated", res);
        }
      )
    }
    //extentUpdated

    var that = this;
    setTimeout(() => {
      that.emit("mapReady");
    }, 700 );
  }

  refresh() {
    ;
    let layers = this.getMap().getLayers();

    for (let layer of layers.array_) {
      this.recursiveRefresh(layer);
    }
  }

  recursiveRefresh(layer, depth) {
    if (depth == undefined) depth = 5;

    if (depth < 0) 
    { ;
      return;
    }

    if (layer.getLayers) {
      for (let layersub of layer.getLayers().array_) {
        this.recursiveRefresh(layersub, depth - 1);
      }
    } else if (layer.get("visible"))
    { if(layer.getSource) {
        var src = layer.getSource();
        if(src.tileCache)
          {
            src.tileCache.clear();
          }
        if(src.refreshFunction)
        {
          src.refreshFunction();
        }
        else
        {
          src.changed();
        }
      } else debugger;
    }
  }

  setServices(services) {
    this.services = {};
    for (let service of services) {
      this.services[service.key] = service;
    }
  }

  fitToWkt(data) {
    let wkt = data.wkt;
    var leftPadding = data.left;
    var bottomPadding = data.bottom;

    let format = new WKT();
    let feature = format.readFeature(wkt);
    let extent = feature.getGeometry().getExtent();

    let pudding = [ 50, 50, 50, 50 + leftPadding];
    this.data.view.cancelAnimations();
    
    this.data.view.fit(extent, {
      size: this.data.map.getSize(),
      duration: 1000,
      padding: pudding
    });
  }

  fitToPointZoom(data)
  {
    this.data.map.getView().animate(
      { zoom : data.zoom
      , center : [ data.x, data.y ]
      //, duration: 100
      }
    );
  }

  destruct() {
    if (this.data) destructMap(this.data);
    if (this._plugins && this._plugins.length) {
      for (let plugin of this._plugins) {
        try {
          plugin.destruct(this);
        } catch (e) {
          console.error("Unable to blow up the plugin!");
        }
      }
    }
    if (this.subs && Object.keys(this.subs).length) {
      console.warn(
        "Found subscriptions that should have been removed. This is probably caused by a bad destructor. Forcibly removing these items"
      );
      console.warn(Object.keys(this.subs));
      for (let sub of Object.keys(this.subs)) {
        console.warn("Unmapping: " + sub);
        this.unmapCmd(sub);
      }
    }
  }
  _initPortSubs() {}
  _bootMap(options) {
    // options is the map options, and config is coming from yaml files
    this.data = bootMap(options, this.config);
  }

  getMap() {
    return this.data.map;
  }
  render() {}

  // brake() {
  //   this.hooks.brake.call();
  // }
  // newMessage(message) {
  //   this.hooks.newMessage.call(message);
  // }
  // moveMap(x, y) {
  //   // this.map.
  // }

  register(plugins) {
    for (let pluginWithOptions of plugins) {
      
      let [plugin, options] = pluginWithOptions;
      this.initPlugin(plugin, options);
    }
  }

  initPlugin(Plugin, options) {
    let plugin = new Plugin();
    this._plugins.push(plugin);
    this.applyToPlugin(plugin, options);
    this.renderPlugin(plugin);
  }

  applyToPlugin(plugin, options) {
    plugin.apply(this, options);
  }
  renderPlugin(plugin) {
    // console.log(this)
    // plugins create an element and embed the elm object in it and then returns that element

    if (typeof plugin !== "undefined") {
      let el = plugin.render();
      if (this.el instanceof Element || this.el instanceof HTMLDocument) {
        this.el.append(plugin.render() || "");
      }
    }
  }

  // Stuff related to changing layers and extends plugins can hook to this and run these functions
  _changeLayer(layer) {
    //console.log("Layer is changed to layer", layer, this);
  }

  // _showLayerByName(layerName, opacity) {
  //   showLayerByName.call(this.data.map, layerName, opacity);
  // }

  zoomToExtent(extent) {
    this.data.map.getView().fit(extent, {
      size: this.data.map.getSize(),
      duration: 1000
    });
  }

  addShape(featureId) {
    this.hooks.newMessage.call(featureId);
  }

  on(name, f) {
    if (!this.triggers[name]) {
      this.triggers[name] = [];
    }

    this.triggers[name].push(f);
  }

  processTrigger(name, value) {
    console.log("We talked the talk of '" + name + "'", value)
    if (this.triggers[name]) for (let fn of this.triggers[name]) fn(value);
  }

  mapCmd(name, f) {
    let fn = val => {
      let requestAnimationFrame =
        window.requestAnimationFrame ||
        window.mozRequestAnimationFrame ||
        window.webkitRequestAnimationFrame ||
        window.msRequestAnimationFrame;
      requestAnimationFrame(() => {
        try {
          f(val);
          this.processTrigger(name, val);
        } catch (ex) {
          //TODO: Rollbar
          console.error(
            "Handler for " + name + " encountered an unhandled exception"
          );
          console.error(ex);
        }
      });
    };
    this.subs[name] = fn;
    if (this.elm) {
      if (this.elm.ports[name]) {
        this.elm.ports[name].subscribe(fn);
      } else {
        console.warn("Port " + name + " does not exist. Cannot bind");
      }
    } else {
      console.error("No elm to bind to");
    }
  }

  pumpCmd(name, val) {
    if(this.subs[name])
    {
      this.subs[name](val);
    }
  }

  unmapCmd(name) {
    if (this.subs[name]) {
      let fn = this.subs[name];
      if (this.elm) {
        if (this.elm.ports[name]) {
          this.elm.ports[name].unsubscribe(fn);
        }
      }
      this.subs[name] = null;
      delete this.subs[name];
    }
  }

  unmapCmds(names) {
    for (let name of names) {
      this.unmapCmd(name);
    }
  }

  emit(name, value) {
    if (!value) value = null;
    if (this.elm) {
      if (this.elm.ports[name]) {
        try {
          this.elm.ports[name].send(value);
        } catch (ex) {
          console.error("Error sending data. Is it in the corrct format?");
          console.error(ex);
        }
      } else {
        console.warn(
          "Unable to send value to a port named " + name + " as it didnt exist!"
        );
        //debugger;
      }
    }
  }

  getMap() {
    if (this.data) return this.data.map;
    return null;
  }
}
