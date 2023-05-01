import React from 'react';
import './syntax_tree.css';

import { useEffect, useState, useRef } from "react";
function SyntaxTree({cnl}) {
    let tree = init(cnl);
    const svg = useRef(null);
    useEffect(()=>{
       if(svg.current){
           svg.current.appendChild(tree)
       }
   },[]);
    return (
       <div ref={svg}/>
       );
}

const range = (arr, step = 1) => Array(Math.ceil((arr[1] - arr[0]) / step)).fill(arr[0]).map((x, y) => x + y * step);

const init = ((arr) => {
    const tree = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "svg"
       );
    const svgheight = 220;
    const depend = {};
    const leaves = {};
    const edges = {};
    let has_deps = {};
    const fontsize = 12;
    let level = 0;
    let taken = {};
    let y_base = svgheight - 60; // This is the distance from the top (y = 0) at which the base of the tree is plotted
    tree.setAttribute("id","tree");
    tree.setAttribute("width",3600);
    tree.setAttribute("height",svgheight);
    
    let x = 4;
    depend[0] = false;
    for(const [i, e] of Object.entries(arr)){
       depend[e.index] = e.dep;
       // 
       if(e.dep === 0){leaves[0] = node(x,20,{"index":0,"pos":e.fun,"dep":-1,"match":false,"fun":"","ort":""},fontsize);}
       leaves[e.index] = node(x,y_base,e,fontsize);
	   x += fontsize*6.5; // IE horizontal node position
	/*
	 * adding direction of edge. Those with dependents in both directions, get LR, meaning edges will be spaced
	 * NB, we're looping through nodes, but it's their parent nodes we're adding dep_dir for
	 * also, the nodes in conll are NOT zero indexed, but the top node has 0 as an imaginary root node
	 */
	let dep_dir = e.index < e.dep ? "L" : "R";
	if(!(e.dep in has_deps)){
       has_deps[e.dep] = dep_dir;
       continue;
    }
    if(has_deps[e.dep] === dep_dir){continue;}
    has_deps[e.dep] = "LR";
   }

    for(const [i,v] of Object.entries(has_deps)){
	   if(i === 0){continue;}
	   if(v === "LR"){
	       leaves[i].setAttribute("L_R",true); // Its actually whether the node has children in both left and right direction
	   }
    }
    let sorted_dep = {};
    /*
     * will be like this sorted_dep = {1:[[1,2],[3,2]],...}
     * where key is distance and subarrays are child/parent pairs
     * needed a the nodes closest to one another should have the lowest edges to avoid crossing
     */
    for(const[i,v] of Object.entries(depend)){ // i = index, v = parent index
       let distance = Math.sqrt((i-v)**2);
       if(!(distance in sorted_dep)){
           sorted_dep[distance] = [[i*1,v]];
           continue;
       }
       sorted_dep[distance].push([i*1,v]);
    }
    // travers sorted_dep and add edges with connect
    for(const [i, v] of Object.entries(sorted_dep)){
       for(const [j, w] of Object.entries(v)){
           let incoming = w[0];
           let outgoing = w[1];
           if(outgoing === 0 && false){
              let g = rootEdge(leaves[w[0]].getAttribute("stalk")*1,200,100,0);
              tree.appendChild(g);
              continue;
	       } // this one actually needs a heavenly root edge thing!!!!!!!!!
	       let g = connect(leaves,incoming,outgoing,y_base,level,taken,fontsize);
	       edges[incoming] = g; // incoming edge
	       tree.appendChild(g);
	    //      let id = g.getAttribute("label");
	       g.addEventListener('mouseover', function(e) {
		//    e.currentTarget.setAttribute('fill', '#f00');
		//    console.log(id + " on");
	       });
	       g.addEventListener('mouseout', function(e) {
		//    e.currentTarget.setAttribute('fill', '#000');
		//    console.log(id + " off");
	       });
	   }
	   level+=fontsize/2;
    }
    // add node events and nodes to tree    
    for(const [i, v] of Object.entries(leaves)){
	//  let dep = v.getAttribute('dep');
	//  let id = v.getAttribute("id");
	   v.addEventListener('mouseover', function(e) {
        let cnode = e.currentTarget;
        while(cnode.getAttribute("dep") >= 0){
          let j = cnode.getAttribute('id').replace('leaf','');
		  cnode.setAttribute('class','highlight'); // node text
		  if(cnode.getAttribute("dep") < 1){break;}
		  cnode = leaves[cnode.getAttribute('dep')];
		  edges[j].firstElementChild.setAttribute('class','edge_highlight'); // the edges
		  edges[j].firstElementChild.nextElementSibling.setAttribute('class','edge_label_highlight'); // bit of an inelegance for you
        }
       });
	   v.addEventListener('mouseout', function(e) {
        let cnode = e.currentTarget;
        while(cnode.getAttribute("dep") >= 0){
          let j = cnode.getAttribute('id').replace('leaf','');
          cnode.classList.remove("highlight");
          if(cnode.getAttribute("dep") < 1){break;}
          cnode = leaves[cnode.getAttribute('dep')];
          edges[j].firstChild.setAttribute('class','edge');
		  edges[j].firstElementChild.nextElementSibling.classList.remove('edge_label_highlight'); // more inelegance
        }
       });
	   tree.appendChild(v);
    }
    return tree;
});

const connect = ((leaves,i,j,y_base,lev=0, taken={}, fontsize) => {
    if(j == 0){return rootEdge(leaves[i].getAttribute("stalk")*1,y_base,y_base-30,0);} // root
    
    let l = i<j?i:j;
    let r = i<j?j:i;
    let left = i<j?true:false;
    let match = leaves[i].getAttribute("match") === "true";
    let stalkR = "stalk";
    let stalkL = "stalk";
    stalkR = leaves[l].getAttribute("L_R") === "true" ? "stalkR" : "stalk";
    stalkL = leaves[r].getAttribute("L_R") === "true" ? "stalkL" : "stalk";
    if(l !== i){
	//  stalkR = "stalkR";
	//  stalkR = leaves[l].getAttribute("L_R") === "true" ? "stalkR" : "stalk";
    }
    else{
	//  stalkL = "stalkL";
	//  stalkL = leaves[r].getAttribute("L_R") === "true" ? "stalkL" : "stalk";
    }
    let s = leaves[l].getAttribute(stalkR)*1;
    let t = leaves[r].getAttribute(stalkL)*1;
    let lbl = leaves[i].getAttribute("fun");
    //    let distance = Math.sqrt((i-j)**2);
    //    let h = (distance * 6); // this is exessive
    let h = lev*2+(fontsize);              // so…
    let intersect = true;
    while(h in taken && intersect){
       intersect = false;
       for(const [i, e] of Object.entries(taken[h])){
           const sect = range(e).filter(value => range([s,t].sort(function(a, b){return a-b})).includes(value));
           if(sect.length !== 0){
              intersect = true;
          }
      }
      if(intersect){
	    h-=4; // and adjust accordingly
	}
}
if(h in taken){
	taken[h].push([s,t].sort(function(a, b){return a-b}));
}
else{
	taken[h] = [[s,t].sort(function(a, b){return a-b})];
}
return edge(s,t-s,y_base,h,lbl,s+"-"+t,left,fontsize,match,i);
});
/*
 * rootEdge is just the verticle one to root
 */
const rootEdge = ((s, y_base, h, edgeID) => {
    let g = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "g"
       );
    g.setAttribute("id","edge_label"+edgeID);
    const e = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "path"
       );
    e.setAttribute("d", "M"+s+", "+y_base+" v-" + h);
    e.setAttribute("id", "edge"+edgeID);
    e.setAttribute("stroke","#000");
    e.setAttribute("stroke-width","0.5");
    e.setAttribute("fill","none");
    g.appendChild(e);
    return g;
});
const edge = ((s,l,y_base,h,t,id,left,fontsize,match,edgeID) => {
    let ur = " a4,4 0 0 1 4,-4 ";
    let rd  = " a4,4 0 0 1 4,4 ";
    //    let ul = " a4,4 0 0 0 -4,-4 "; // not in use
    //    let ld = " a4,4 0 0 0 -4,4 ";  // not in use
    l -= 8;
    let g = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "g"
       );
    g.setAttribute("id","edge_label"+edgeID);
    g.setAttribute("label",id);
    const e = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "path"
       );
    e.setAttribute("d", "M"+s+", "+y_base+" v-" + h + ur +"h"+l+rd+"v" + h);
    e.setAttribute("id", "edge"+edgeID);
    //    let clr = "#"+ Math.floor(Math.random()*16777215).toString(16);
    //    clr = "#000";
    if(match && false){
       e.setAttribute("class","match_edge");
   }
   else{
       e.setAttribute("class","edge");
   }
    /*
      e.setAttribute("stroke",clr);
      e.setAttribute("stroke-width","0.5");
      e.setAttribute("fill","none");
    */
   g.appendChild(e);
    //    let larrow = "\u2190";
   let larrow = "\u140a";
    //    let rarrow = "\u2192";
   let rarrow = "\u1405";
   let arrow = left?larrow:rarrow;
    let lab = label(s+(l/2)-2,(y_base-7)-h,fontsize,arrow+t+arrow,id,match); //(y_base-7)-h seems a bit… esoteric. Let's fix this. Like, how to express 7?
    g.appendChild(lab);
    return g;
});

const label = ((x,y,fontsize,lab,id,match) => {
    let g = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "g"
       );
    g.setAttribute("id",id);
    let height = fontsize/2;
    let w = lab.length * (fontsize/2);
    const p = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "path"
       );
    x = x-w/2+4;
    p.setAttribute("d", "M"+x+", "+(y)+" h"+w+" a2,2 0 0 1 2,2 v"+height+" a2,2 0 0 1 -2,2 h-"+w+" a2,2 0 0 1 -2,-2 v-"+height+" a2,2 0 0 1 2,-2 z");
    //    p.setAttribute("stroke","black");
    p.setAttribute("stroke-width","0.0");
    p.setAttribute("fill","white");
    g.appendChild(p);

    const m = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "path"
       );
    m.setAttribute("d", "M"+(x-2)+", "+(y+(fontsize/2))+" h"+(w+4)+"");
    m.setAttribute("id", "mid"+id);
    g.appendChild(m);

    let txt = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "text"
       );
    if(match){
       txt.setAttribute("class","match");
   }
   txt.setAttribute("font-size",fontsize-2);
   txt.setAttribute("font-family","monospace");
   let tp = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "textPath"
       );
   tp.setAttribute("href","#mid"+id);
   tp.setAttribute("startOffset","50%");
   tp.setAttribute("text-anchor","middle");
   tp.textContent=lab;
   txt.appendChild(tp);
   g.appendChild(txt);
   return g;
});

const node = ((x,y,a,fontsize) => {
    let id = a.index;
    let lab = a.pos;
    let dep = a.dep;
    let fun = a.fun;
    let tok = a.ort;
    let hit = a.match;
    if(isNaN(x)){
       console.log("node: ",x,y,id,lab,dep);
       return;
   }
   let g = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "g"
       );
   let height = fontsize/2 + 4;
   let w = (lab.length+2) * (fontsize/2);
   let w2= (tok.length+2) * (fontsize/2);
   let w2_offset = (w2-w)/2;
   g.setAttribute("match",hit);
   g.setAttribute("id","leaf"+id);
   g.setAttribute("L_R", false);
   g.setAttribute("stalk",x+(w/2));
   g.setAttribute("stalkL",x+1);
   g.setAttribute("stalkR",(x+w-1));
   g.setAttribute("label",lab);
   g.setAttribute("dep",dep);
   g.setAttribute("fun",fun);
   const p = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "path"
       );
   p.setAttribute("d", "M"+x+", "+y+" h"+w+" a2,2 0 0 1 2,2 v"+height+" a2,2 0 0 1 -2,2 h-"+w+" a2,2 0 0 1 -2,-2 v-"+height+" a2,2 0 0 1 2,-2 z");
   p.setAttribute("stroke","black");
   p.setAttribute("stroke-width","0.3");
   p.setAttribute("fill","white");
   if(dep < 0){
       p.setAttribute("fill","#17c4e3");
   }
   if(hit){
       p.setAttribute("fill","#afe0c8");
   }
   g.appendChild(p);
   g.appendChild(txt(x-2,y+fontsize-2,w,lab,"mid"+id,fontsize,hit));
   let x2 = x - w2_offset;
    g.appendChild(txt(x2,y+fontsize*3,w2,tok,"tokm"+id,fontsize,hit)); // YYY t_base
    return g;
});

const txt = ((x,y,w,label,id,fontsize,match) => {
    //    let height = fontsize/2;
    //    let w = label.length*fontsize;
    let g = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "g"
       );
    const m = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "path"
	/* this path is just a text guide, not to be displayed */
       );
    m.setAttribute("d", "M"+x+", "+y+" h"+(w+4)+"");
    m.setAttribute("id", id);
    m.setAttribute("stroke","black");
    m.setAttribute("stroke-width","0");
    g.appendChild(m);
    const t = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "text"
       );
    t.setAttribute("font-size",fontsize);
    t.setAttribute("font-family","monospace");
    if(id.replace(/[a-z]+/,'')*1 == 0){
       t.setAttribute("class",'root');
   }
   if(match){
       t.setAttribute("class","match");
   }
   const tp = document.createElementNS(
       "http://www.w3.org/2000/svg",
       "textPath"
       );
   tp.setAttribute("href","#"+id);
   tp.setAttribute("startOffset","50%");
   tp.setAttribute("text-anchor","middle");
   tp.textContent=label;
   t.appendChild(tp);
   g.appendChild(t);
   return g;
});


export default SyntaxTree;
