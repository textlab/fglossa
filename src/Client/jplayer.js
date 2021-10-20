/** @jsx React.DOM */

import $ from 'jquery';
import 'jquery-ui';
import React from 'react';
import ReactDOM from 'react-dom';
import createReactClass from 'create-react-class';
import PropTypes from 'prop-types';
import { WFplayer } from './wfplayer.js';

var TextBox = createReactClass({
    displayName: "TextBox",
    propTypes: {
        mediaObj: PropTypes.object.isRequired,
        divs: PropTypes.object.isRequired,
        startAtLine: PropTypes.number.isRequired,
        endAtLine: PropTypes.number.isRequired
    },
    getInitialState: function () {
        return {
            wfPlayer: null
        };
    },
    renderWord: function (word, index) {
        var att, attString;
        attString = "";
        for (att in word) {
            if (att === "pos") {
                word[att] = word[att].replace(/:/g, "/");
            }
            attString += att + " : " + word[att] + "<br>";
        }
        return React.createElement("a", { key: index, title: attString, style: word.match ? { color: '#b00', fontWeight: 'bold', fontSize: '0.9em' } : {} }, word[this.props.mediaObj.DisplayAttribute], " ");
    },
    renderAnnotation: function (annotation, lineNo) {
        var endTimecode, getStyle, i, segment, speaker, textDivs, innerDivs, timecode;
        timecode = annotation.From;
        endTimecode = annotation.To;
        speaker = annotation.Speaker;
        segment = (function () {
            var _results;
            _results = [];
            for (i in annotation.Line) {
                _results.push(this.renderWord(annotation.Line[i], i));
            }
            return _results;
        }).call(this);
        getStyle = (function (_this) {
            return function () {
                if (parseInt(lineNo) === parseInt(_this.props.highlightLine)) {
                    return {
                        display: 'table-row',
                        color: '#000',
                        backgroundColor: '#eea'
                    };
                } else if (lineNo >= _this.props.startAtLine && lineNo <= _this.props.endAtLine) {
                    return {
                        display: 'table-row',
                        color: '#000'
                    };
                } else if (annotation.From === _this.firstStart || annotation.To === _this.lastEnd) {
                    return {
                        display: 'table-row',
                        color: '#ccc'
                    };
                } else {
                    return {
                        display: 'none'
                    };
                }
            };
        })(this);
        textDivs = [];
        innerDivs = [];
        if (this.props.mediaObj.Mov.MovieLoc !== '_') {
            innerDivs.push(
                React.createElement("div", { key: "waveformBtnDiv", className: "waveformBtnDiv" }, React.createElement("button", { title: "Show waveform", className: "btn btn-xs btn-default", style: { marginRight: 10 }, onClick: this.toggleWFplayer.bind(this, lineNo) }, React.createElement("img", { src: "speech/waveform.png", style: { width: 12 } })))
            );
        }
        innerDivs.push(
            React.createElement("div", { key: "speakerDiv", className: "speakerDiv" }, React.createElement("a", { className: "speaker", title: speaker }, speaker)),
            React.createElement("div", { key: "segmentDiv", className: "segmentDiv" }, segment)
        );
        textDivs.push(React.createElement("div", {
            className: 'textDiv ' + timecode.replace(/\./, "_"),
            id: 'jp-' + lineNo,
            "data-start_timecode": timecode,
            "data-end_timecode": endTimecode,
            style: getStyle(),
        }, innerDivs));
        if (this.state.wfPlayer === lineNo) {
            textDivs.push(React.createElement("div", { className: "waveDiv" }, React.createElement(WFplayer, { mediaObj: this.props.mediaObj, divs: this.props.divs, startAt: lineNo, endAt: lineNo })));
        }
        return textDivs;
    },
    toggleWFplayer: function (line) {
        if (this.state.wfPlayer !== line) {
            this.setState({
                wfPlayer: line
            });
            return this.props.pauseJPlayer();
        } else {
            return this.setState({
                wfPlayer: null
            });
        }
    },
    render: function () {
        var annotation, annotations, displayAttribute, n;
        displayAttribute = this.props.mediaObj.DisplayAttribute;
        annotation = this.props.divs;
        this.firstStart = annotation[this.props.startAtLine].From;
        this.lastEnd = annotation[this.props.endAtLine].To;
        annotations = (function () {
            var _results;
            _results = [];
            for (n in annotation) {
                _results.push(this.renderAnnotation(annotation[n], n));
            }
            return _results;
        }).call(this);
        return React.createElement("div", { className: "jplayer-text autocue" }, annotations);
    }
});

export var Jplayer = createReactClass({
    displayName: "Jplayer",
    propTypes: {
        mediaObj: PropTypes.object,
        divs: PropTypes.object,
        mediaType: PropTypes.string,
        hasLocalMedia: PropTypes.bool,
        hasAudio: PropTypes.bool
    },
    getStartLine: function (mediaObj) {
        var minStart, startAt;
        startAt = parseInt(mediaObj.StartAt);
        minStart = parseInt(mediaObj.MinStart);
        if (!this.props.ctxLines) {
            return startAt;
        } else if (this.props.ctxLines === 'all') {
            return minStart;
        } else if (startAt - this.props.ctxLines >= minStart) {
            return startAt - this.props.ctxLines;
        } else {
            return minStart;
        }
    },
    getEndLine: function (mediaObj) {
        var endAt, maxEnd;
        endAt = parseInt(mediaObj.EndAt);
        maxEnd = parseInt(mediaObj.MaxEnd);
        if (!this.props.ctxLines) {
            return endAt;
        } else if (this.props.ctxLines === 'all') {
            return maxEnd;
        } else if (endAt + this.props.ctxLines <= maxEnd) {
            return endAt + this.props.ctxLines;
        } else {
            return maxEnd;
        }
    },
    getStartTime: function () {
        return parseFloat(this.props.divs[this.state.startLine].From);
    },
    getEndTime: function () {
        return parseFloat(this.props.divs[this.state.endLine].To);
    },
    getInitialState: function () {
        return {
            startLine: this.getStartLine(this.props.mediaObj),
            endLine: this.getEndLine(this.props.mediaObj),
            currentLine: this.getStartLine(this.props.mediaObj),
            restart: false
        };
    },
    componentDidMount: function () {
        return this.createPlayer();
    },
    componentWillReceiveProps: function (nextProps) {
        if (!_.isEqual(this.props.mediaObj, nextProps.mediaObj)) {
            this.setState({
                startLine: this.getStartLine(nextProps.mediaObj),
                endLine: this.getEndLine(nextProps.mediaObj),
                currentLine: this.getStartLine(nextProps.mediaObj)
            }, function () {
                this.destroyPlayer();
                this.createPlayer();
            });
        }
    },
    componentDidUpdate: function (prevProps, prevState) {
        if (this.state.restart) {
            return this.restartPlayer();
        }
    },
    componentWillUnmount: function () {
        return this.destroyPlayer();
    },
    pauseJPlayer: function () {
        var $node, $playerNode;
        $node = $(ReactDOM.findDOMNode(this));
        $playerNode = $node.find(".jp-jplayer");
        return $playerNode.jPlayer("pause");
    },
    createPlayer: function () {
        var $node, $playerNode, ext, lastLine, mediaObj, mov, path, supplied;
        $node = $(ReactDOM.findDOMNode(this));
        mediaObj = this.props.mediaObj;
        mov = mediaObj.Mov.MovieLoc;
        //path = "http://localhost:61054/" + mediaObj.Mov.Path + "/" + this.props.mediaType + "/";
        path = this.props.hasLocalMedia
            ? "http://localhost/" + mediaObj.Mov.Path.replace(/^media\//, '') + "/" + this.props.mediaType + "/"
            : "http://www.tekstlab.uio.no/glossa2/" + mediaObj.Mov.Path + "/" + this.props.mediaType + "/";
        supplied = mediaObj.Mov.Supplied;
        $("#movietitle").text(mov);
        lastLine = parseInt(mediaObj.LastLine);
        ext = this.props.mediaType === "audio" ? ".mp3" : ".mp4";
        $playerNode = $node.find(".jp-jplayer");
        $playerNode.jPlayer({
            solution: "flash, html",
            ready: (function (_this) {
                return function () {
                    $playerNode.jPlayer("setMedia", {
                        rtmpv: path + mov + ext,
                        m4v: path + mov + ext,
                        poster: "speech/_6.6-%27T%27_ligo.skev.graa.jpg"
                    });
                    return $playerNode.jPlayer("play", _this.getStartTime());
                };
            })(this),
            timeupdate: (function (_this) {
                return function (event) {
                    var ct;
                    ct = event.jPlayer.status.currentTime;
                    if (ct > _this.getEndTime()) {
                        $playerNode = $node.find(".jp-jplayer");
                        $playerNode.jPlayer("play", _this.getStartTime());
                        $playerNode.jPlayer("pause");
                        return _this.setState({
                            currentLine: _this.getStartLine(mediaObj),
                            restart: false
                        });
                    } else if (ct > _this.props.divs[_this.state.currentLine].To) {
                        return _this.setState({
                            currentLine: _this.state.currentLine + 1,
                            restart: false
                        });
                    }
                };
            })(this),
            swfPath: "",
            supplied: supplied,
            solution: 'html, flash',
            preload: 'metadata'
        });
        return $(".slider-range").slider({
            range: true,
            min: 0,
            max: lastLine,
            values: [this.state.startLine, this.state.endLine + 1],
            slide: (function (_this) {
                return function (event, ui) {
                    if (ui.values[1] - ui.values[0] < 1) {
                        return false;
                    }
                    $playerNode.jPlayer("stop");
                    return _this.setState({
                        restart: true,
                        currentLine: ui.values[0],
                        startLine: ui.values[0],
                        endLine: ui.values[1] - 1
                    });
                };
            })(this)
        });
    },
    destroyPlayer: function () {
        var $node;
        $node = $(ReactDOM.findDOMNode(this));
        return $node.find(".jp-jplayer").jPlayer('destroy');
    },
    restartPlayer: function () {
        var $node, $playerNode;
        $node = $(ReactDOM.findDOMNode(this));
        $playerNode = $node.find(".jp-jplayer");
        return $playerNode.jPlayer("play", this.getStartTime());
    },
    render: function () {
        return React.createElement("div", { style: { position: 'relative' } },
            this.props.hasAudio && React.createElement("div", { style: { float: 'right', width: 480 } },
                React.createElement("div", { className: "jp-video jp-video-270p", id: "jp_container_1" },
                    React.createElement("div", { className: "jp-type-single" },
                        React.createElement("div", { className: "jp-jplayer", style: this.props.mediaType == 'audio' ? { display: 'none' } : { width: 480, height: 270 } },
                            React.createElement("img", { id: "jp_poster_1", src: "speech/_6.6-%27T%27_ligo.skev.graa.jpg", style: { width: 480, height: 270, display: 'none' } }),
                            React.createElement("object", { id: "jp_flash_1", name: "jp_flash_1", data: "swf/Jplayer.swf", type: "application/x-shockwave-flash", width: "1", height: "1", tabIndex: "-1", style: { width: 1, height: 1 } },
                                React.createElement("param", { name: "flashvars", value: "jQuery=jQuery&id=jplayer&vol=0.8&muted=false" }),
                                React.createElement("param", { name: "allowscriptaccess", value: "always" }),
                                React.createElement("param", { name: "bgcolor", value: "#000000" }),
                                React.createElement("param", { name: "wmode", value: "opaque" })
                            )
                        ),
                        React.createElement("div", { className: "jp-gui" },
                            React.createElement("div", { className: "jp-video-play", style: this.props.mediaType == 'audio' ? { display: 'none', visibility: 'hidden' } : { display: 'none' } },
                                React.createElement("a", { href: "javascript:;", className: "jp-video-play-icon", tabIndex: "1" }, "play")
                            ),
                            React.createElement("div", { className: "jp-interface" },
                                React.createElement("div", null, " "),
                                React.createElement("div", { className: "jp-controls-holder" },
                                    React.createElement("ul", { className: "jp-controls" },
                                        React.createElement("li", null, React.createElement("a", { href: "javascript:;", className: "jp-play", tabIndex: "1", title: "play", style: { display: 'block' } }, "play")),
                                        React.createElement("li", null, React.createElement("a", { href: "javascript:;", className: "jp-pause", tabIndex: "1", title: "pause", style: { display: 'none' } }, "pause")),
                                        React.createElement("li", null, React.createElement("a", { href: "javascript:;", className: "jp-mute", tabIndex: "1", title: "mute" }, "mute")),
                                        React.createElement("li", null, React.createElement("a", { href: "javascript:;", className: "jp-unmute", tabIndex: "1", title: "unmute", style: { display: 'none' } }, "unmute")),
                                        React.createElement("li", null, React.createElement("a", { href: "javascript:;", className: "jp-volume-max", tabIndex: "1", title: "volume max" }, "volume-max"))
                                    ),
                                    React.createElement("div", { className: "jp-volume-bar" },
                                        React.createElement("div", { className: "jp-volume-bar-value", style: { width: '80%' } })
                                    )
                                ),
                                React.createElement("div", { className: "jp-title" }, React.createElement("ul", null, React.createElement("li", { id: "movietitle" }, "kristiansand_01um-02uk")))
                            )
                        ),
                        React.createElement("div", { className: "jp-no-solution", style: { display: 'none' } },
                            React.createElement("span", null, "Update required"), React.createElement("a", { href: "http://get.adobe.com/flashplayer/", target: "_blank" }, "Flash plugin")
                        )
                    )
                ),
                React.createElement("div", { className: "slider-range ui-slider ui-slider-horizontal ui-widget ui-widget-content ui-corner-all", "aria-disabled": "false" },
                    React.createElement("div", { className: "ui-slider-range ui-widget-header ui-corner-all", style: { left: '40%', width: '40%' } }),
                    React.createElement("a", { className: "ui-slider-handle ui-state-default ui-corner-all", href: "#", style: { left: '40%' } }),
                    React.createElement("a", { className: "ui-slider-handle ui-state-default ui-corner-all", href: "#", style: { left: '80%' } })
                )
            ),
            this.props.mediaObj && React.createElement(TextBox, { mediaObj: this.props.mediaObj, divs: this.props.divs, startAtLine: this.state.startLine, endAtLine: this.state.endLine, highlightLine: this.state.currentLine, pauseJPlayer: this.pauseJPlayer })
        );
    }
});