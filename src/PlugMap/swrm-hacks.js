
import { transform } from "ol/proj";

import { findAllLayersById } from "./map-helpers"


export default class SWRMPlugin {
    constructor() {
        this.core = null;
        this.map = null;
        this.inViewLayers =
            [ "lyr_swrm_filters_projects"
            , "lyr_swrm_filters_treatments"
            ];
        this.inViewLayerMap = false;
    }


    apply(core) {
        this.core = core;
        this.map = core.getMap();

        core.mapCmd("updateLayersFiltersCmd", this.updateLayersFilters.bind(this));

        core.mapCmd("swrm_inview_highlight", this.highlight.bind(this));
        core.mapCmd("swrm_inview_unhighlight", this.unhighlight.bind(this));

        this.setup(this.map);
    }

    highlight(feat)
    {
        var lyr = this.inViewLayerMap[feat.layer]
        if(lyr)
        {
            if(lyr.getSource().highlight)
            {
                lyr.getSource().highlight(feat.id);
            }
        }
    }

    unhighlight(feat)
    {
        var lyr = this.inViewLayerMap[feat.layer]
        if(lyr)
        {
            if(lyr.getSource().unhighlight)
            {
                lyr.getSource().unhighlight(feat.id);
            }
        }
    }

    updateLayersFilters(categories) {
        let self = this;
        let map = this.core.getMap();

        var allLayersFlat = map
            .getLayers()
            .getArray()

        var allLayersDict = {};

        allLayersFlat.forEach(layer => {
            allLayersDict[layer.get('id')] = layer;
            if (layer.getLayers) {
                layer
                    .getLayers()
                    .getArray()
                    .forEach(l => {
                        allLayersDict[l.get('id')] = l;
                    });
            }
        })


        //TODO: Group by layer key

        var byLayerKey = {}

        categories.forEach(data => {
            data.filters.forEach(layer => {
                if(!Array.isArray(layer))
                {
                    if(!byLayerKey[layer.key])
                    {
                        byLayerKey[layer.key] = { mode : data.mode, filts : [] };
                    }

                    byLayerKey[layer.key].filts.push(layer)
                }
                    //this.applyFilter(allLayersDict,layer);
                else
                    layer.forEach(l =>
                        {
                            if(!byLayerKey[l.key])
                            {
                                byLayerKey[l.key] = { mode : data.mode, filts : [] }
                            }
        
                            byLayerKey[l.key].filts.push(l)
                            //this.applyFilter(allLayersDict,l);
                        });
            })
        });

        var layerKeys = Object.keys(byLayerKey);

        for(var layerkeyi = 0; layerkeyi < layerKeys.length; layerkeyi ++)
        {
            try
            {
                this.applyFilter(allLayersDict,layerKeys[layerkeyi], byLayerKey[layerKeys[layerkeyi]]);
            }
            catch(ex) //TODO: This crashes applyFilters on ESRI now
            {
                console.error("Failed to apply filters :(", { key : layerKeys[layerkeyi], error : ex } )
            }
        }
    }

    applyFilter(allLayersDict, layerKey, layerset)
    {
        debugger;
        var lyr = allLayersDict[layerKey];
        if (lyr) {
            var newdef = "";
            var olddef = "";

            var src = lyr.getSource()

            if(!src.hasListener("tileloadend"))
            {
                src.on("tileloadend", evt => 
                    { 
                        if(src.filterEngine)
                        {
                            var f = evt.tile.getFeatures();
                            f.forEach(src.filterEngine);
                        }
                    })
            }
            
            if (src.applyFilters) {
                src.applyFilters(layerset);
                this.scheduleSendFeats();
            }
            else {
                var def = {};
                def[layer.layerid] = layer.filter
                def = JSON.stringify(def);

                olddef = src.params_["layerDefs"];
                newdef = def;

                src.params_["layerDefs"] = def;
            }
        }
        else {
        }
    }

    setupLayerMapper() {
        this.inViewLayerMap = {};

        var map = this.map;

        var allLayers = [];

        this.inViewLayers.forEach(layer => {
            let lyr =
                findAllLayersById(layer, map.getLayers());


            if (lyr && lyr.length) {
                this.inViewLayerMap[layer] = lyr[0];
                var that = this;
                lyr[0].getSource().on("tileloadend", evt => 
                    { 
                        var o = lyr[0].getSource().get("tilesLoading");
                        if(o == undefined)
                            o = 0;

                        o--;

                        if(o < 0)
                            console.log("EXCEPTION: NEGATIVE TILES LOADING => " + o);

                        lyr[0].getSource().set("tilesLoading", o);

                        if(lyr[0].getSource().filterEngine)
                        {
                            var f = evt.tile.getFeatures();
                            f.forEach(lyr[0].getSource().filterEngine);
                        }

                        that.scheduleSendFeats();
                    })
                    lyr[0].getSource().on("tileloadstart", evt => 
                    { 
                        var o = lyr[0].getSource().get("tilesLoading");
                        if(o == undefined)
                            o = 0;

                        o++;

                        if(o > 100)
                            console.log("EXCEPTION: TOO MANY TILES LOADING => " + o);

                        lyr[0].getSource().set("tilesLoading", o);

                    })
                    lyr[0].getSource().on("tileloaderror", evt => 
                    { 
                        console.log("Tile Load Error");
                        var o = lyr[0].getSource().get("tilesLoading");
                        if(o == undefined)
                            o = 0;

                        o--;

                        if(o < 0)
                            console.log("EXCEPTION: NEGATIVE TILES LOADING => " + o);

                        lyr[0].getSource().set("tilesLoading", o);
                    });

                    lyr[0].getSource().inview = true;
            }
        });

        if (!Object.keys(this.inViewLayerMap).length)
            this.inViewLayerMap = false;
    }

    setup(map) {

        if (map) {
            map.getView().on("change", evt => {
                this.core.emit("swrm_features_inview_clear");

                this.scheduleSendFeats("change")
            }
            )


        }
    }


    scheduleSendFeats(event) {

        if (this.sendFeatsTO) {
            clearTimeout(this.sendFeatsTO);
        }
        var that = this;

        this.sendFeatsTO = setTimeout(function () { that.trySendFeats(event) }, 50);
    }


    trySendFeats(event) {
        var map = this.map;

        if (!this.inViewLayerMap) {
            this.setupLayerMapper()
        }
        else {
            var x = this.inViewLayerMap;

            var extent = map.getView().calculateExtent(map.getSize());

            var keys = Object.keys(x);

            var featMap = {};

            var stillLoading = false;

            var maxFeatsPer = 100; 

            keys.forEach(key => {
                var lyr = this.inViewLayerMap[key];
                var featuresInExtent = lyr.getSource().getFeaturesInExtent(extent);
                var tl = lyr.getSource().get("tilesLoading");

                if(tl > 0)
                {
                    stillLoading = true;
                    return;
                }

                for(var i = 0; i < featuresInExtent.length; i++)
                {
                    var feat = featuresInExtent[i];

                    var fev = feat.get("FilterEngine")
                    

                    if (!featMap[key])
                        featMap[key] = {};
                        feat.properties_["layer"] = key;

                    featMap[key][feat.properties_["id"]] = feat.properties_;

                    if(Object.keys(featMap[key]).length > maxFeatsPer)
                        break;
                }

                // featuresInExtent.forEach(feat => {
                //     var fev = feat.get("FilterEngine")
                    

                //     if (!featMap[key])
                //         featMap[key] = {};

                //         feat.properties_["layer"] = key;

                //     featMap[key][feat.properties_["id"]] = feat.properties_;
                    
                // })

            })

            var typeKeys = Object.keys(featMap);


            var allFeats = [];

            typeKeys.forEach(featType => {
                var feats = featMap[featType];

                var featKeys = Object.keys(feats);

                featKeys.forEach(feat => {
                    allFeats.push(feats[feat]);
                })
            })

            //if (allFeats.length > 0 || event != "change") //This is a hack
            debugger;
            this.core.emit("swrm_features_inview_update", allFeats);


        }
    }

    render() { }
}