import $ from 'jquery';
import 'jquery-ui';
import React from 'react';
import ReactDOM from 'react-dom';
import createReactClass from 'create-react-class';

var corpusIds = {
    "bb": 28,
    "cans3": 53,
    "ndc2": 52
};

export var WFplayer = createReactClass({
    displayName: "WFplayer",

    componentDidMount: function () {
        var $node, corpusId, mediaObj, start, stop;
        $node = $(ReactDOM.findDOMNode(this));
        mediaObj = this.props.mediaObj;
        corpusId = corpusIds[mediaObj.CorpusCode];
        $("#movietitle").text(mediaObj.title);
        start = this.props.divs[this.props.startAt || mediaObj.StartAt].From;
        stop = this.props.divs[this.props.endAt || mediaObj.EndAt].To;
        return $node.find("#waveframe").attr('src', "https://tekstlab.uio.no/wave/wfplayer-" + corpusId + "-" + mediaObj.Mov.MovieLoc + "-" + start + "-" + stop);
    },

    render: function () {
        return React.createElement("div", null, React.createElement("iframe", { height: "385", width: "100%", id: "waveframe", target: "_blank", className: "wfplayer", allow: "autoplay" }));
    }
});
