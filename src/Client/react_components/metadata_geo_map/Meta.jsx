import React, {useState, useRef, useCallback, useEffect} from "react";
import classnames from "classnames";
import PropTypes from "prop-types";

import "./multiRangeSlider.css";

// layout for whole App
import "./layout.css";

// for Map component
import "./map.css";
import { GoogleMap, DrawingManager, Marker } from "@react-google-maps/api";
import dot from './dot.svg';
//import yel from './yellow.svg';
import green from './green.svg';

const polygonOptions = {
    fillColor: `#2196F3`,
    strokeColor: `#000000`,
    fillOpacity: 0.6,
    strokeWeight: 0.5,
    clickable: true,
    editable: true,
    draggable: true,
    zIndex: 1,
};
const options = {
    drawingControl: true,
    drawingControlOptions: {
	drawingModes: ["polygon"]
    },
    polygonOptions: polygonOptions
};
const polygonsContain = (polygons,coords) => {
    if(Object.keys(polygons).length === 0){
	return coords.map((coord) => {return coord.id});
    }
    let markers = [];
    for(var k in polygons){
	markers = markers.concat(polygonContains(polygons[k],coords));
    }
    markers = [...new Set(markers)];
    return markers;
}
const polygonContains = (polygon,coords) => {
    let markers = [];
    const google = window.google;
    let cl = google.maps.geometry.poly.containsLocation
    for(var j=0; j<coords.length; j++){
	const contains = cl(
	    coords[j].coords,
            polygon
	);
	if(contains){
	    markers.push(coords[j].id);
	}
    }
    return markers;
}

function Map({ apiKey, center, callback, coords, zoom }) {
    // Define refs for Polygon instance and listeners
    let enveloped = [];
    const polyHash = useRef({});

    const [state, setState] = useState({
	drawingMode: "polygon"
    });

    const [polyid, polyidinc] = useState(0);

//    const [ markers, setMarkers] = useState([...coords.values()]);

    const onPolygonComplete = React.useCallback(
	function onPolygonComplete(poly) {
	    const path = poly.getPath();
	    poly.id = polyid.toString();
	    poly.addListener("click", function(){
		poly.setMap(null);
		delete polyHash.current[poly.id];
		enveloped = polygonsContain(polyHash.current,coords);
		callback(enveloped);
	    });
//	    poly.addListener("drag", function(){console.log("what a drag"); });
	    path.addListener("set_at", function(){
		enveloped = polygonsContain(polyHash.current,coords);
		callback(enveloped);
	    });
	    path.addListener("remove_at", function(){
		enveloped = polygonsContain(polyHash.current,coords);
		callback(enveloped);
	    });
	    path.addListener("insert_at", function(){
		enveloped = polygonsContain(polyHash.current,coords);
		callback(enveloped);
	    });
	    polyHash.current[polyid.toString()] = poly;
	    polyidinc(polyid => polyid+1);
	    enveloped = polygonsContain(polyHash.current,coords);
	    callback(enveloped);
	    return;
	},
    );
    return (
	<div className="MapComp" style={{marginTop: 20}}>
		<GoogleMap
		    mapContainerClassName="Map"
		    center={center}
		    zoom={zoom}
		    version="weekly"
//		    disableDoubleClickZoom={true}
		>
		    <DrawingManager
			drawingMode={state.drawingMode}
			options={options}
			onPolygonComplete={onPolygonComplete} // the one we need
		    />
		{
		    coords ? (
			coords.map((marker) => {
			    return (
				<Marker
				    position={marker.coords}
				    key={marker.coords.id}
				    icon={{url: ((marker.polyselected && marker.selected)?green:dot), Size: 10}}
				    title={coords.id}
				    visible={marker.visibility}
				/>
			    )
			})
		    ) : null
		}
		</GoogleMap>
	</div>
    );
}

// END EF MAP

// MULTIRANGE SLIDER

const MultiRangeSlider = ({ cat, min, max, onChange }) => {
//    const [isInitialRender, setIsInitialRender] = useState(0);
    const [minVal, setMinVal] = useState(min);
    const [maxVal, setMaxVal] = useState(max);
    const minValRef = useRef(null);
    const maxValRef = useRef(null);
    const range = useRef(null);

    // Convert to percentage
    const getPercent = useCallback(
	(value) => Math.round(((value - min) / (max - min)) * 100),
	[min, max]
    );

    // Set width of the range to decrease from the left side
    useEffect(() => {
	if (maxValRef.current) {
	    const minPercent = getPercent(minVal);
	    const maxPercent = getPercent(+maxValRef.current.value); // Preceding with '+' converts the value from type string to type number

	    if (range.current) {
		range.current.style.left = `${minPercent}%`;
		range.current.style.width = `${maxPercent - minPercent}%`;
	    }
	}
    }, [minVal, getPercent]);

    // Set width of the range to decrease from the right side
    useEffect(() => {
	if (minValRef.current) {
	    const minPercent = getPercent(+minValRef.current.value);
	    const maxPercent = getPercent(maxVal);

	    if (range.current) {
		range.current.style.width = `${maxPercent - minPercent}%`;
	    }
	}
    }, [maxVal, getPercent]);

    // Get min and max values when their state changes
    useEffect(() => {
    	//onChange({ min: minVal, max: maxVal });
    //}, [minVal, maxVal, onChange]); // WTF?!?
	//}, [minVal, maxVal]);
    });
    return (
	<div className="divTableCell">
	    <div className="container">
		<input
		    type="range"
		    min={min}
		    max={max}
		    value={minVal}
		    ref={minValRef}
		    onChange={(event) => {
			const value = Math.min(+event.target.value, maxVal - 1);
			setMinVal(value);
			event.target.value = value.toString();
			onChange({ min: value, max: maxVal });
		    }}
		    className={classnames("thumb thumb--zindex-3", {
			"thumb--zindex-5": minVal > max - 100
		    })}
		/>
		<input
		    type="range"
		    min={min}
		    max={max}
		    value={maxVal}
		    ref={maxValRef}
		    onChange={(event) => {
			const value = Math.max(+event.target.value, minVal + 1);
			setMaxVal(value);
			event.target.value = value.toString();
			onChange({ min: minVal, max: value });
		    }}
		    className="thumb thumb--zindex-4"
		/>

		<div className="slider">
		    <div className="slider__track" />
		    <div ref={range} className="slider__range" />
		    <div className="slider__left-value">{minVal}</div>
		    <div className="slider__right-value">{maxVal}</div>
		</div>
	    </div>
	</div>
    );
};

MultiRangeSlider.propTypes = {
    min: PropTypes.number.isRequired,
    max: PropTypes.number.isRequired,
    onChange: PropTypes.func.isRequired
};


// END OF MULTIRANGE SLIDER


// MENU

const Checkbox = (props) => {
    return (
	<label>
	    <div className="nulls">{props.label}</div>
	    <div><input type="checkbox" defaultChecked onChange={props.onChange}/></div>
	</label>
    );
};
function ucfirst(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}

function Menu(props){
    let comps = [];
    let j = 0;
    props.meta.forEach(
	function(e){
	    let row = [];
	    row.push(
		<div className="divTableCell" key={"row_label"+j}>
		    {ucfirst(e.name)}
		</div>
	    );
	    if(e.type == "interval"){
		let k = e.key;
		let min = e.val.min;
		let max = e.val.max;
		row.push(
			<MultiRangeSlider
			    cat={k}
			    key={k}
			    min={min}
			    max={max}
			    onChange={({ min, max }) => props.interval([k,min,max])}
			/>
 		);
		let null_checkbox = [];
		if(e.nulls){
		    null_checkbox.push(
			    <Checkbox
				key={k+"_null"}
				label={"null"}
				onChange={(e) => props.discrete({cat: k,val: null,checked: e.target.checked})}
			    />
		    );
		}
		row.push(
		    <div className="divTableCell" key={k+"_null_div"}>
			{null_checkbox}
		    </div>);
		let rowID = "row_id"+j++;
		comps.push(<div key={rowID} className="divTableRow">{row}</div>);
		return;
	    }
	    let checks = [];
	    let k = e.key;
	    let nulls = [];
	    e.val.forEach(
		function(i){
		    let check = (
			<Checkbox
			    key={k+"_"+i}
			    label={i}
			    onChange={(e) => props.discrete({cat: k,val: i,checked: e.target.checked})}
			/>);
		    if(i == "null"){
			nulls.push(check);
			return;
		    }
		    checks.push(
			check
		    );
		}
	    );
	    row.push(
		<div key={k+"_checks"} className="divTableCell">
		    {checks}
		</div>
	    );
	    row.push(
		<div key={k+"_null"} className="divTableCell">
		    {nulls}
		</div>
	    );
	    let rowID = "row_id"+j++;
	    comps.push(<div key={rowID} className="divTableRow">{row}</div>);
	});
    return (
	comps
    );
}


// This is a static class

class Sets {

    static tid2loc = {};
    static SuperSet = {}; // SuperSet is a hash of hashes, where the innerhashes are key/set pairs. The sets are simply tids, on which set operations can be performed.

    static initSuperSet(meta, cats, id_key, loc_key){
	let column_names = {};
	for (const [k, v] of Object.entries(cats)) {
	    if(k !== id_key){
		column_names[v.COLUMN] = k;
		this.SuperSet[k] = {};
	    }
	}
	for (const [k, v] of Object.entries(meta)) {
	    let tid = v[cats[id_key].COLUMN]; //ALWAYS 0
	    for (const [i, e] of Object.entries(v)){
		if(i != 0 & e != 0){
		    let key = column_names[i];
		    if(key == loc_key){this.tid2loc[tid] = e}
		    if(!(e in this.SuperSet[key])){
			this.SuperSet[key][e] = new Set();
		    }
		    this.SuperSet[key][e].add(tid);
		}
		else{
		}
	    }
	}
    }

    static union(sets){
	let u = new Set();
	for(const [i, set] of Object.entries(sets)){
	    u = new Set([...u, ...set]);
	}
	return u;
    }
    static intersection(sets){
	let inter = false;
	for(const [k,v] of Object.entries(sets)){
	    if(!inter){inter = v}
	    inter = new Set([...inter].filter(x => v.has(x)))
	}
	if(!inter){return new Set();}
	return inter;
    }
    static difference(a,b){
	a.filter(x => !b.includes(x));
    }


    // updates the k values in activeSets {sex: {…}, agegroup: {…}, age: {…}}, in the range l - r, but checks each value is actually present in SuperSet

    static interval_add_set(k,l,r, activeSets = {}){
	let nullval = activeSets[k] && activeSets[k][null];
	activeSets[k] = {} // IE, this is going to be reset, so needs to be purged
	if(nullval){activeSets = this.add_set(k,null, activeSets)}
	var i;
	for(i = l; i <= r; i++){
	    if(i in this.SuperSet[k]){
		activeSets = this.add_set(k,i,activeSets);
	    }
	}
	return activeSets;
    }
    static add_set(k,v,activeSets){
	if(!(k in activeSets)){activeSets[k] = {}}
	activeSets[k][v] = true;
	return activeSets;
    }


    static rem_set(k,e,activeSets){
	delete activeSets[k][e];
	return activeSets;
    }
 // IMPORTANT! WITHIN SAME CAT, UNION; BETWEEN, INTERSECTION
    static select_tids(activeSets){
	let sets = [];
	for (const [k, v] of Object.entries(activeSets)){
	    let set = [];
	    let keys = Object.keys(v);
	    for (const [i, w] of Object.entries(keys)){
		set.push(this.SuperSet[k][w]);
	    }
	    set = this.union(set); // NB!! Could be empty set!
	    sets.push(set);
	}
	return this.intersection(sets); // AGAIN!! Could be empty set!
    }

    static remove_active_set(k, activeSets){
	delete activeSets[k];
	return activeSets;
    }
    static empty_active_set(k, activeSets){
	activeSets[k] = {};
	return activeSets;
    }
    static initActiveSets(data, activeSets = {}){
	for(const [i,v] of Object.entries(data)){
	    if(v.type == "discrete"){
		for(const [j,w] of Object.entries(v.val)){
		    activeSets = this.add_set(v.key, w, activeSets);
		}
	    }
	    if(v.type == "interval"){
		if(v.nulls){
		    activeSets = this.add_set(v.key,null, activeSets);
		}
		activeSets = this.interval_add_set(v.key,v.val.min,v.val.max, activeSets);
	    }
	}
	return activeSets;
    }

}

export default function Meta({ coords, config, meta, ok, cancel }){
    const location_key = config.LOCATOR;
    const id = config.ID;
    Sets.initSuperSet(meta, config.CATEGORY, id, location_key);
    const zoom = config.ZOOM;
    const API_KEY = config.API_KEY;
    const center = config.CENTER
    const locs = Object.keys(Sets.SuperSet[location_key]);
    let menu_data = initMenuData(Sets.SuperSet, config.CATEGORY, "");
    const orig = useRef(menu_data);
    const markers = locs.map(function(loc){
	return {id: loc, polyselected: true, selected: true, coords:{id: loc, lat: coords[loc][0],lng: coords[loc][1]}}});
//    let a = Sets.initActiveSets([{type: 'discrete', key: location_key, val: locs}]);
    //    a = Sets.initActiveSets(menu_data, a);
    let a = Sets.initActiveSets(menu_data);
    menu_data = menu_data.filter(e => e.key != location_key); // don't want the location data being rendered in the menu
    const [activeSets, aUpdate] = useState(a);
    const [c, cUpdate] = useState(markers);
    const [ntids, ntidsUpdate] = useState([...Sets.select_tids(activeSets)].length);
    const [nlocs, nlocsUpdate] = useState(locs.length);
    const updatePlaces = () =>{
	let locs =  [...Sets.select_tids(activeSets)].map(e => Sets.tid2loc[e]);
	locs = [...new Set(locs)];
	for(const [i,v] of Object.entries(c)){
	    if(!locs.includes(v.id)){
		v.selected = false;
		continue;
	    }
	    v.selected = true;
	}
	ntidsUpdate([...Sets.select_tids(activeSets)].length);
	nlocsUpdate(locs.length);
	cUpdate([...c.values()]);
    };

    const mapCallback = (d) => {
//	aUpdate(Sets.remove_active_set(location_key, activeSets));
	aUpdate(Sets.empty_active_set(location_key, activeSets));
	d.forEach(
	    function(e){
		aUpdate(Sets.add_set(location_key, e, activeSets));
	    }
	);
	for(const [k,v] of Object.entries(c)){
	    if(!d.includes(v.id)){
		c[k].polyselected = false;
	    }
	    else{
		c[k].polyselected = true
	    }
	}
	ntidsUpdate([...Sets.select_tids(activeSets)].length);
//	cUpdate([...c.values()]);
	updatePlaces();
    };
    const intervalCallback = (d) => {
	aUpdate(Sets.interval_add_set(d[0],d[1],d[2], activeSets));
	updatePlaces();
    };
    const discreteCallback = (d) => {
	d.checked ? aUpdate(Sets.add_set(d.cat,d.val,activeSets)) : aUpdate(Sets.rem_set(d.cat,d.val, activeSets));
	updatePlaces();
    };
    const okCallback = () => {
	let result = {}
	for(const e of Object.values(orig.current)){
	    if(e.type == 'interval'){
		let comp = getInterval(Object.keys(activeSets[e.key]));
		if(!(comp[0] == e.val.min && comp[1] == e.val.max && comp[2] == e.nulls)){
		    result[e.key] = comp;
		}
	    }
	    else{
		//
		let comp = Object.keys(activeSets[e.key]);
		if(comp.length != e.val.length){
		    result[e.key] = comp;
		}
	    }
	}
//	result[location_key] = Object.keys(activeSets[location_key]);
	ok(result);
    };
    return (
	    <div id="grid" style={{padding: 10}}>
		    <div className="head" id="head">
                <nav className="level">
                   <div className="level-left">
                     <div className="level-item">
                      <button className="button is-success" onClick={() => okCallback(activeSets)}>
                       {"OK"}
                      </button>
                    </div>
                    <div className="level-item">
                     <button className="button" onClick={() => cancel()}>{"CANCEL"}</button>
		        {/*
		        <button onClick={() => console.log([...Sets.select_tids(activeSets)])}>
		    	{"tids"}
		        </button>
		        <button onClick={() => console.log(activeSets)}>
		    	{"meta"}
		        </button>
		        <button onClick={() => console.log(activeSets[location_key])}>
		    	{"locs"}
		        </button>
		        */}
                    </div>
                     <div className="level-item">
                      <span>{ntids + " informants in " + nlocs + " locations"}</span>
                     </div>
                   </div>
                </nav>
            </div>
            <div className="inner-grid" style={{marginTop: 20}}>
                <div className="divTableBody">
                 <Menu
                     meta={menu_data}
                     interval={(d) => intervalCallback(d)}
                     discrete={(d) => discreteCallback(d)}
                 />
                </div>
            </div>
            <Map
                apiKey={API_KEY}
                center={center}
                callback={(d) => mapCallback(d)}
                coords={c}
                zoom={zoom}
            />
	    </div>
	);
}
/* Some auxfuns */

function initMenuData(meta, cats, ignore){
    let menu_data = [];
    for (const [key, value] of Object.entries(meta)) {
	if(key == ignore){ continue; }
	if(cats[key].TYPE == "interval"){
	    let [min,max,nulls] = getInterval(Object.keys(value));
	    menu_data.push({type:"interval",key:key,val:{min:min,max:max},nulls:nulls,name:cats[key].NAME});
	    continue;
	}
	menu_data.push({type:"discrete",key:key,val:Object.keys(value),name:cats[key].NAME});
    }
    return menu_data;
}
function getInterval(arr){
    let min = 99999999;
    let max = 0;
    var nulls = false;
    arr.forEach(
	function(e){
	    if(e == "null"){
		nulls = true;
		return;
	    }
	    e = parseInt(e); // the keys are string reps of ints (Javascript converts them to strings), so they must be correctly parsed to ints
	    if(e > max){max = e;}
	    if(e < min){min = e;}
	});
    return [min,max,nulls];
}

//const rootElement = document.getElementById("root");
//ReactDOM.render(<Meta />, rootElement);

