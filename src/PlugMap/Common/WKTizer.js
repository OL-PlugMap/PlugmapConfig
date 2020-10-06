
import WKT from "ol/format/WKT";
import GeoJSON from "ol/format/GeoJSON";
import Circle from "ol/geom/Circle";
import {fromCircle} from "ol/geom/Polygon";
import { truncate } from "@turf/truncate";
import { booleanWithin } from "@turf/helpers";
import { union } from "@turf/union";
import { unkinkPolygon } from "@turf/helpers";
import {
  lineString,
  polygon,
  buffer,
  difference,
  multiPolygon,
  simplify
} from "@turf/helpers";
import { toWgs84, toMercator } from "@turf/projection";

//TODO: Alias this as TURF types for readability


const POINT = "Point";
const LINE = "LineString";
const POLYGON = "Polygon";
const MULTI_POINT = "MultiPoint";
const MULTI_LINE = "MultiLineString";
const MULTI_POLYGON = "MultiPolygon";
const CIRCLE = "Circle";
const METERS_PER_MILE = 1609.34;
const feetToMeters = 0.3048;



export default class WKTizer {
  // format: any; //TODO
  // options: any;
  
    constructor() {
     
      this.format = {
        wkt: new WKT(),
        geojson: new GeoJSON()
      };

    }

    /*Noms a feature geometry into a WKT*/
  convertFeatureToWKT(feature) {
    //feature = featureFromSource.getGeometry();
    if(feature.getGeometry)
      feature = feature.getGeometry();

    

    let type = feature.type || feature.getType();
    let wkt = "";
    if (type == "Feature") {
      wkt = this.format.wkt.writeGeometry(feature.getGeometry());
    } else if (type == "Polygon" || type == "MultiPolygon") {
      wkt = this.format.wkt.writeGeometry(feature);
    } else {
      throw new Error(
        "Must pass a valid Feature, Polygon, or MultiPolygon to use as a new project shape."
      );
    }
    return wkt;
  }

  /*Omnoms a list of features into a multipoly*/
  mergeFeatures(features, skipRemove) {
    if (!features || !features.length) return features;

    if (features.length == 1) {
      return features[0]; //Union one feature is one feature
    }
    let hb = features.shift();
    let head = skipRemove ? hb : this.removeIntersections(hb).features[0];
    if(!head)
      head = hb;

    //head = this.removeIntersections(head).features();

    for (let feature of features) {
      var uk_feature = skipRemove ? feature : this.removeIntersections(feature).features[0];
      try
      {
        head = union(head, uk_feature);
      } catch(ex)
      {
        try
        {
          let f1 = buffer(head,0.1);
          let f2 = buffer(uk_feature,0.1);
          head = union(f1, f2);
        }
        catch(ex2)
        {
          let t = turf;
          //Dunno what to do here
          // console.warn("Unable to merge features");
          // console.warn(ex)
          // console.warn(ex2)
        }
      }
    }

    return head;
  }

  removeIntersections(feature)
  {
    if(!feature || !feature.geometry)
      debugger;
    if(feature.geometry.type == "MultiPolygon")
    {
      //debugger;
      /*var itms = [];
      for(var i = 0; i < feature.geometry.coordinates.length; i++)
      {
        var p = polygon(feature.geometry.coordinates[i]);
        var unkinked = this.removeIntersections(p).features[0];
        itms.push(unkinked);
      }
      console.log(feature);
      return { features: [ featureCollection(itms) ] };*/
      return { features: [ feature ] };
    }
    let res = feature;

    try
    {
      
      let buffered = unkinkPolygon(feature);
      let t = turf;

      let feats = buffered.features;
      feats.sort( a => b =>
          {
            if(a.geometry.type == "MultiPolygon" || b.geometry.type == "MultiPolygon")
              return 0;
            return booleanWithin(a,b) ? -1 : 1;
          } 
        );

      res = feats[0];
      let backup = res;
      for(var i = 1; i < feats.length; i++)
      {
        if (res.geometry.type != "MultiPolygon" && booleanWithin(feats[i], res))
        {
          res = difference(res, feats[i]);
        }
        else
        {
          res = union(res,feats[i]);
        }

        if(res == null)
          res = backup;
        else
          backup = res;
      }
      //booleanWithin
      return  { features: [ res ] };
    }
    catch(ex)
    {
      //do something
      debugger;
    }
    //let intersected = intersect(feature, buffered);
    return { features: [ feature ] };
  }

  convertPolygon(geom, skipRemove, skipMerge) {
    let shape = null;

    if (!geom) debugger;

    if (!geom.getType) return geom;

    switch (geom.getType()) {
      case POLYGON:
        var p = polygon(geom.getCoordinates());
        var ni = skipRemove ? p : this.removeIntersections(p).features
        var mer = skipMerge ? ni : this.mergeFeatures(ni, skipRemove);
        shape = mer;
        break;

      case MULTI_POLYGON:
        shape = skipMerge ? multiPolygon(geom.getCoordinates()) : this.mergeFeatures(flatten(multiPolygon(geom.getCoordinates())).features, skipRemove);
        let x = turf;
        break;

      default:
        throw new Error(
          `Invalid Geometry type: ${geom.getType()}. Geometry must be either a Polygon or MultiPolygon.`
        );
    }

    if(shape == undefined || shape == null)
      debugger;
    return shape;
  }

  convertLineIntoPolygon(geom, buffer_) {
    let poly = lineString(geom.getCoordinates());
    poly = toWgs84(poly);
    poly = truncate(poly);
    poly = buffer(poly, buffer_, { units: "feet" });
    poly = this.simplifyIfGreaterThan(500, poly);
    poly = toMercator(poly);
    poly = this.format.geojson.readGeometryFromObject(poly.geometry);
    return poly;
  }

  convertFeatureIntoPolygon(feature, buffer, skipRemove, skipMerge) {
    let geom = feature.getGeometry().clone(); //.transform("EPSG:3857","EPSG:4326");
    
    let poly = null;
    
    switch (geom.getType()) {
      case POLYGON:
        {
          poly = geom;
        }
        break;
      case MULTI_POLYGON:
        {
          poly = geom;
        }
        break;
      case CIRCLE:
        {
          poly = fromCircle(geom);
        }
        break;
      case LINE:
        {
          buffer = feature.get("buffer") || buffer;
          poly = this.convertLineIntoPolygon(geom, buffer);
        }
        break;
      case MULTI_LINE:
        {
          buffer = feature.get("buffer") || buffer;
          poly = this.convertLineIntoPolygon(geom, buffer);
        }
        break;
      case POINT:
        {
          buffer = feature.get("buffer") || buffer;
          let circle = new Circle(
            geom.getCoordinates(),
            buffer * feetToMeters
          ); //TODO radius and buffer things
          poly = fromCircle(circle);
        }
        break;
    }

    if(poly == undefined || poly == null)
      debugger;

    return this.convertPolygon(poly, skipRemove, skipMerge);
  }

  simplifyIfGreaterThan(limit, geojson) {
    if (!limit) {
      limit = 1000;
    }

    let verticeCount;
    switch (geojson.geometry.type) {
      case POLYGON:
        verticeCount = geojson.geometry.coordinates[0].length;
        break;

      case MULTI_POLYGON:
        verticeCount = 0;
        for (let geo of geojson.geometry.coordinates) {
          for (let coords of geo) {
            verticeCount += coords.length;
          }
        }
        break;

      default:
        verticeCount = 0;
        break;
    }

    if (verticeCount >= limit) {
      geojson = simplify(geojson, { tolerance: 0.001, mutate: true });
    }
    return geojson;
  }

  convertFromSource(source, buffer, flatten)
  {
    let features = source.getFeatures();

    return this.convertFromOpenLayerFeatures(features, buffer, flatten);
  }

  convertFromLayer(layer, buffer)
  {
    return this.convertFromSource(layer.source, buffer, true);
  }

  convertFromOpenLayersFeature(feature) {

    //Convert OpenLayers feature into GeoJSON feature
    let polyized = this.convertFeatureIntoPolygon(feature);
    let geoJSON = this.format.geojson.readFeature(polyized);

    //Convert GeoJSON to WKT
    let wkt = this.convertFeatureToWKT(geoJSON.getGeometry());

    return wkt;
  }

  convertFromOpenLayerFeatures(features, buffer, flatten, skipRemove) {
    let wkts = [];
    let geoms = [];

    let max = 0;
    for (let feature of features) {
      feature.setId(max++);
      let poly = this.convertFeatureIntoPolygon(feature, buffer); //this.convertPolygon(feature.getGeometry());
      geoms.push(poly);
    }

    if (!flatten) {
      
      for(let geo of geoms)
      {
        let toNom = this.format.geojson.readFeature(geo);
        let wkt = this.convertFeatureToWKT(toNom.getGeometry());
        wkts.push(wkt);
      }
      // for (let feature of features) {
      //   let wkt = this.convertFeatureToWKT(feature.getGeometry());

      //   wkts.push(wkt);
      // }
    } else {

      

      

      let flat = this.mergeFeatures(geoms, skipRemove);
      if(flat)
      {
        //TODO: Make this an option
        //flat = this.simplifyIfGreaterThan(500, flat);
        let toNom = this.format.geojson.readFeature(flat);
        let wkt = this.convertFeatureToWKT(toNom.getGeometry());
        wkts.push(wkt);
      }
    }

    return wkts;
  }

  flattenOpenLayerFeatures(features, skipRemove) {
    let geoms = [];

    let max = 0;
    for (let feature of features) {
      feature.setId(max++);
      let poly = this.convertFeatureIntoPolygon(feature, undefined, skipRemove);
      geoms.push(poly);
    }

    let flat = this.mergeFeatures(geoms, skipRemove);
      
    return this.format.geojson.readGeometryFromObject(flat.geometry);
  }

  flattenOpenLayerFeaturesToGeoJSON(features, skipRemove) {
    let geoms = [];

    let max = 0;
    for (let feature of features) {
      feature.setId(max++);
      let poly = this.convertFeatureIntoPolygon(feature, undefined, skipRemove);
      geoms.push(poly);
    }

    let flat = this.mergeFeatures(geoms, skipRemove);
      
    return flat; //this.format.geojson.readGeometryFromObject(flat.geometry);
  }




  geojsonToWKT(geojson) {
    geojson = this.simplifyIfGreaterThan(500, geojson);

    var feat = this.format.geojson.readGeometryFromObject(geojson.geometry, {
      featureProjection: "EPSG:3857"
    });

    var wkt = wkt = this.format.wkt.writeGeometry(feat);

    return wkt;
  }

  geojsonToOl(geojson) {
    
    geojson = this.simplifyIfGreaterThan(500, geojson);
    var feat = this.format.geojson.readGeometryFromObject(geojson.geometry, {
      featureProjection: "EPSG:3857"
    });
    return feat;
  }

  wktToOl(wkt) {
    var feat = this.format.wkt.readGeometry(wkt);
    return feat;
  }

  olToWKT(ol) {
    return this.format.wkt.writeGeometry(ol.getGeometry());
  }

  olToGeoJSON(feature) {

    //Convert OpenLayers feature into GeoJSON feature
    let polyized = this.convertFeatureIntoPolygon(feature);
    //let geoJSON = this.format.geojson.readFeature(polyized);

    return polyized;
  }

  olToGeoJSONSimple(feature) {

    //Convert OpenLayers feature into GeoJSON feature
    //let polyized = this.convertFeatureIntoPolygon(feature, 0, true, true);

    var t = feature.getGeometry().getType();
    if(t == "Point" || t == "LineString")
      return this.olToGeoJSON(feature);

    let polyized = this.format.geojson.writeFeaturesObject([feature], {'featureProjection': 'EPSG:4326'});

    return polyized.features[0];
  }


  geojsonToWKTAndOl(geojson) {
    geojson = this.simplifyIfGreaterThan(500, geojson);

    var feat = this.format.geojson.readGeometryFromObject(geojson.geometry, {
      featureProjection: "EPSG:3857"
    });

    var wkt = wkt = this.format.wkt.writeGeometry(feat);
    
    return {ol: feat, wkt: wkt};
  }
}