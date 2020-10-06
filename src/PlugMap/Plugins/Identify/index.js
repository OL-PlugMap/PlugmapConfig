import Overlay from "ol/Overlay"


export default class Identify {
  constructor() {
    this.enabled = false;
    this.core = null;
    this.index = 0;

    this.requestsActive = 0;

    this.models =
      [{
        enabled: true
        , dataMapping:
        {
          title:
          {
            query: "name"
          }
          , fields:
            [{
              name: "Name"
              , query: "name"
            }
              , {
                name: "Type"
              , query: "type"
            }
              , {
                name: "Status"
              , static: "Completed"
            }
            ]
        }
        , canViewDetails: true
      }
      ];

    this.data =
      [
      ]
  }

  apply(core) {
    this.core = core;
    this.map = core.getMap();

    core.mapCmd("enableIdentify", this.enable.bind(this));
    core.mapCmd("disableIdentify", this.disable.bind(this));
  }

  previous() {
    this.index = (this.index || 1) - 1;
    this.renderPopup();
  }

  next() {
    this.index = (this.index || 0) + 1;
    this.renderPopup();
  }

  getDetails() {
    var selectedItem = this.data[this.index];

    //TODO: this is kinda hacky 
    if (selectedItem) {
      let selid = undefined;

      if (selectedItem.attributes.id != undefined)
        selid = selectedItem.attributes.id;

      if (selectedItem.attributes.objectid != undefined)
        selid = selectedItem.attributes.objectid;

      var aid = parseInt(selid);
      if(aid != undefined)
        aid += selectedItem.model.idTransform;

      this.core.emit("identifyGetDetails", parseInt(selid));
    }
  }

  feature(featurekey) {
    var selectedItem = this.data[this.index];

    //TODO: this is kinda hacky 
    if (selectedItem) {
      let selid = undefined;

      if (selectedItem.attributes.id != undefined)
        selid = selectedItem.attributes.id;

      if (selectedItem.attributes.objectid != undefined)
        selid = selectedItem.attributes.objectid;

      var msg = { key: featurekey, id: selid, typeKey: selectedItem.model.typeKey };
      debugger;
      this.core.emit("identifyFeature", msg);
    }
  }

  enable(options) {

    if (this.container)
      this.container.setAttribute("style", "display:block;")

    if (this.enabled) {
      return;
    }


    this.options = options;

    window.identify_prevresult = this.previous.bind(this);
    window.identify_nextresult = this.next.bind(this);
    window.identify_getdetails = this.getDetails.bind(this);
    window.identify_feature = this.feature.bind(this);


    let map = this.core.getMap();

    if (!this.popup) {
      var popup = new Overlay({
        element: document.getElementById('popup')
      });
      map.addOverlay(popup);
      this.popup = popup;

      var content = document.getElementById('popup-content')
      this.content = content;


      var closer = document.getElementById('popup-closer');
      closer.onclick = function () {
        popup.setPosition(undefined);
        closer.blur();
        return false;
      };

      map.on('singleclick', this.onclick.bind(this));
    }

    this.enabled = true;



  }



  onclick(evt) {

    if (!this.enabled)
      return;

    var coordinate = evt.coordinate;
    this.coordinate = coordinate;

    this.data = [];
    this.index = 0;

    var o = this.options.layers;

    let reqs = [];
    var core = this.core;


    var layerModelMap = {};

    for (let i = 0; i < o.length; i++) {
      let layer = o[i];

      let baseURL = "";

      let extent = layer.config.value.extent;
      let layerConfig = layer.config.value.endpoints[0];
      let token = undefined;

      if (layerConfig.tokenKey) {
        if (core.services) {
          if (core.services[layerConfig.tokenKey]) {
            baseURL = core.services[layerConfig.tokenKey].baseUrl;
            token = core.services[layerConfig.tokenKey].token
          }
        }
      }
      let map = this.core.getMap();

      let tolerance = 0;
      let view = map.getView();

      if (view) {
        tolerance = 5 - view.getZoom();
      }

      if (tolerance < 0)
        tolerance = 0;

      let url = baseURL + layerConfig.url + "identify?geometry=" +
        coordinate[0] + "%2C" + coordinate[1] +
        "&geometryType=esriGeometryPoint&sr=102100"

      if (o[i].identify.queryLayerId != null) {
        layerModelMap[o[i].key + "_" + o[i].identify.queryLayerId + ""] = layer;
        url += "&layers=all%3A" + o[i].identify.queryLayerId
      }
      if (o[i].identify.queryLayerIds != null) {
        for (let id of o[i].identify.queryLayerIds)
          layerModelMap[o[i].key + "_" + id + ""] = layer;
        url += "&layers=all%3A" + o[i].identify.queryLayerIds.join(",")
      }

      if (o[i].identify.tolerance != null) {
        tolerance = o[i].identify.tolerance
      }
      url += "&tolerance=" + tolerance + //TODO
        "&mapExtent=-13599773.911815433%2C4274035.298291555%2C-11251628.40289482%2C5284227.064108444" + //TODO
        "&imageDisplay=1920%2C1080%2C96&returnGeometry=false" +
        "&dynamicLayers=&returnZ=false&returnM=false&gdbVersion=&returnUnformattedValues=false" +
        "&returnFieldName=false&datumTransformations=&layerParameterValues=&mapRangeValues=&layerRangeValues=&f=pjson"

      if (token)
        url += "&token=" + token

      let req =
      {
        src: layer
        , url: url
      }

      if (o[i].identify.enabled == true) {
        reqs.push(req);
      }
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


    for (var req of reqs) {
      req = JSON.parse(JSON.stringify(req));
      var rfn = function (req) {

        that.requestsActive++;
        window.fetch(req.url)
          .then(function (response) {
            return response.json();
          })
          .then(function (myJson) {

            if (!myJson.results)
              return;

            myJson = myJson.results;
            for (var i = 0; i < myJson.length; i++) {
              //myJson[i].model = req.src.identify;
              if (myJson[i].layerId > -1) {
                if (layerModelMap[req.src.key + "_" + myJson[i].layerId + ""]) {
                  myJson[i].model = layerModelMap[req.src.key + "_" + myJson[i].layerId + ""].identify;
                  that.data.push(myJson[i]);
                }
              }

            }


            that.requestsActive--;
            that.renderPopup();
          })
          .catch(function (error) {
            that.requestsActive--;
            that.renderPopup();
            debugger;
          })
      }
      rfn(req);
    }

    //setTimeout(this.emulateNetwook.bind(this), Math.random()*250+250);

    this.popup.setPosition(coordinate);

    this.renderPopup();
  }




  renderPopup() {
    //var hdms = toStringHDMS(toLonLat(coordinate));

    this.content.innerHTML = this.makeContent(this.data, this.index);
  }



  makeContent(items, index) {
    if (!items.length) {
      if (this.requestsActive > 0)
        return '<div class="identify">' +
          "Searching ..." +
          "</div>"
      else
        return '<div class="identify">No Results</div>'
    }

    var selectedItem = items[index];

    var fields = '<div class="identify_fields">'

    if (selectedItem && selectedItem.model) {
      for (let field of selectedItem.model.dataMapping.fields) {
        var value = "?";
        if (field.query)
          value = selectedItem.attributes[field.query] || "";
        if (field.static)
          value = field.static;
        if (field.map)
          value = field.map.values[selectedItem.attributes[field.map.query] + "" || ""] || "No mapping for value " + selectedItem.attributes[field.map.query]
        fields += '<div class="identify_field_row"><span class="identify_field">' + field.name + ':</span><span>' + value + '</span></div>'
      }
    }

    fields += '</div>';


    var actions = '<div class="identify_details">';

    if (items[index].model.canViewDetails)
      actions += '<button onclick="identify_getdetails()">Details</buton>'

    if (this.options.extraFeatures)
      for (var feature of this.options.extraFeatures) {
        actions += '<button onclick="identify_feature(\'' + feature.key + '\');">' + feature.name + '</buton>';
      }

    actions += '</div>';

    var footer = '';

    if (items.length > 1) {
      footer = '<div class="identify_seperator" ></div>';
      footer += '<div class="identify_results"><div class="identify_item">' + (index + 1) + ' of ' + items.length + ' Results</div>'

      footer += '<div class="identify_prevNext">'
      footer += "<button "
      if (index > 0)
        footer += " onclick='identify_prevresult()' "
      else
        footer += " class='disabled' "
      footer += ">Prev</button> | <button "
      if (index < items.length - 1)
        footer += " onclick='identify_nextresult()' "
      else
        footer += " class='disabled' "
      footer += ">Next</button>"
      footer += '</div>'

      footer += '</div>'
    }

    var ret = '<div class="identify">' +
      fields +
      actions +
      footer +
      "</div>"

    return ret;
  }

  disable() {
    console.log("Disable id")
    if (this.container)
      this.container.setAttribute("style", "display:none;")
    if (!this.enabled) {
      return;
    }


    let map = undefined;

    if (this.core)
      map = this.core.getMap();


    // if(map)
    //   map.un('singleclick');

    window.identify_prevresult = undefined;
    window.identify_nextresult = undefined;
    window.identify_getdetails = undefined;


    if (this.popup)
      this.popup.setPosition(undefined);

    this.enabled = false;
  }

  render() {

    var el = document.createElement("div");
    var el2 = document.createElement("div");
    var closer = document.createElement("div")

    el.setAttribute("id", "popup");
    el.setAttribute("class", "ol-popup noselect");

    el2.setAttribute("id", "popup-content");
    el2.setAttribute("class", "popup-content");

    closer.setAttribute("id", "popup-closer")
    closer.setAttribute("class", "ol-popup-closer")

    el.appendChild(closer);
    el.appendChild(el2);

    document.body.appendChild(el);

    this.container = el;
  }
}
