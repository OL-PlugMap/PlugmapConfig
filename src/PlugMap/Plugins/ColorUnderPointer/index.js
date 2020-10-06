// This plugin adds a port for core to receive extent/zoom level from outside world (e.g rest api, ...)
import throttle from "lodash.throttle";

export default class ColorUnderPointer {
  constructor() {
    //Best practices state that you should set up the objects properties here
    this.core = null;
    this.last = { r: 255, g: 255, b: 255 };
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
    core.mapCmd("enableColorUnderPointerCmd", val => {
      this.enableColorUnderPointer(val);
    });
    core.mapCmd("disableColorUnderPointerCmd", val => {
      this.disableColorUnderPointer(val);
    });
  }

  enableColorUnderPointer(options) {
    //TODO: Your implementation goes here!
    let someValue = {r:1,g:0,b:1};

    //core emit will try to push data over a port if it exists
    //If it does not exist nothing happens
    //The second parameter is optional
    //If it is not set then null is sent
    

    var map = this.core.getMap();
    if(map)
    {
      let getPixelData = evt => {
        
        // get mouse position
        let position = map.getEventPixel(evt.originalEvent)

        let ctx = map.getRenderer().canvas_.getContext("2d");
        let pixel = ctx.getImageData(position[0], position[1], 1, 1);
        let canvasRgb = { r: pixel.data[0], g: pixel.data[1], b: pixel.data[2] };

        // get layer pixel rgb values
        let layerRgb = null;
        
        map.forEachLayerAtPixel(
          evt.pixel,
          (layer, pixel) => {
            if(options.layers && options.layers.includes(layer.get("id")))
            {
              if (pixel) {
                layerRgb = { r: pixel[0], g: pixel[1], b: pixel[2] };
              } else {
                layerRgb = null;
              }
            }
          },
          this,
          layer => {
            if(options.layers)
              return options.layers.includes(layer.get("id"))
              
            return true;
          },
          this
        );
        if(!layerRgb)
          layerRgb = { r: 255, g: 255, b: 255 };
        if(this.last.r != layerRgb.r ||
            this.last.g != layerRgb.g ||
            this.last.b != layerRgb.b
          )
          {
            this.core.emit("colorChanged", layerRgb);
            this.last = layerRgb;
          }
        }
      map.on("pointermove", throttle(getPixelData, 150)); //TODO
      
    }

  }


  disableColorUnderPointer() {
    //TODO: Your implementation goes here!
    let someValue = {};

    //core emit will try to push data over a port if it exists
    //If it does not exist nothing happens
    //The second parameter is optional
    //If it is not set then null is sent
    //this.core.emit("colorChanged", someValue);
  }

  init() {}

  render() {
    //This will be used in the future if we need to add some sore of custom element
    const el = document.createElement("div");
    return el;
  }
}
