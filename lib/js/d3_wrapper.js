/*******************************************************************************
 * The MIT License
 *
 * Copyright � 2018-2021 Subhadeep Niogi <sndjones007@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the �Software�), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED �AS IS�, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 ******************************************************************************/

/**
 * A class which is a wrapper for the d3.js library methods and calls. This
 * class is needed because with every version change of d3.js the methods also
 * changes.
 * This class will consume any changes to d3.js library methods
 * @class
 */
var D3Wrapper = new function() {

  // private Methods

  /**
   * @param {String} key The key attribute name of the element
   * @param {String} seId The Svg element data matrix key (See the
   *                      constants file)
   * @return {Boolean} True if the attribute is valid for the element else false
   */
  var isValid = function(key, seId) {
    return key.match(/^(s|sw|slc|fl|t)$/) ||
      (seId === "t" && (key.match(/^(ta|ff|fs)$/)));
  };

  /**
   * Set default attributes value for the svg element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Object} dictAttributes A dictionary of attributes to set for the
   *                           svg element
   * @param {String} seId The Svg element data matrix key (See the
   *                      constants file)
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  var setDefaultAttribute = function(
    svgElement,
    dictAttributes,
    seId
  ) {
    dictAttributes = dictAttributes || {};

    if (!("st" in dictAttributes)) dictAttributes.st = "black";
    if (!("sw" in dictAttributes)) dictAttributes.sw = "1";
    if (!("slc" in dictAttributes)) dictAttributes.slc = "square";
    if (!("fl" in dictAttributes)) {
      if (seId === "t") dictAttributes.fl = "black";
      else dictAttributes.fl = "none";
    }
    if (seId === "t") {
      if ("ta" in dictAttributes) dictAttributes.ta = "start";
      if ("ff" in dictAttributes) dictAttributes.ff = "sans-serif";
      if ("fs" in dictAttributes) dictAttributes.fs = "11";
    }
  };

  /**
   * Get attribute data to set from the data matrix dictionary item
   * @param {String} key The key of the data matrix object
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  var getAttribute = function(key) {
    switch (key) {
      case "st":
        return "stroke";
      case "sw":
        return "stroke-width";
      case "slc":
        return "stroke-linecap";
      case "fl":
        return "fill";
      case "ta":
        return "text-anchor";
      case "ff":
        return "font-family";
      case "fs":
        return "font-size";
      default:
        return "";
    }
  }

  // Public functions

  /**
   * Create SVG Graphics container
   * @param {String} name The html element id text inside which SVG is to be
   *                      rendered
   * @param {Number} params The width of the SVG graphics OR
   *                      The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object}
   */
  this.createSvg = function(
    name,
    params,
    d3Parent
  ) {
    d3Parent = d3Parent || d3;
    return (d3Parent || d3).select("#" + name)
      .append("svg")
      .attr("id", name)
      .attr("viewbox", "0 0 {0} {1}".formatArray(params));
  };

  /**
   * Create SVG defs
   * @param {String} svgElement The d3 object which corresponds to the svg
   *                            element
   * @returns {Object} A svg defs d3 object
   */
  this.createDefs = function(
    svgElement
  ) {
    return svgElement.append("defs");
  }

  /**
   * Create SVG line element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createLine = function(
    svgElement,
    params
  ) {
    return svgElement.append("line")
      .attr("x1", params[0])
      .attr("y1", params[1])
      .attr("x2", params[2])
      .attr("y2", params[3]);
  };

  /**
   * Create SVG horizontal line element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createLineHoriz = function(
    svgElement,
    params
  ) {
    return D3Wrapper.createLine(svgElement, params[0],
            params[2], params[1], params[2]);
  };

  /**
   * Create SVG vertical line element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createLineVert = function(
    svgElement,
    params
  ) {
    return D3Wrapper.createLine(svgElement, params[0], params[1],
      params[0], params[2]);
  };

  /**
   * Set attributes value for the svg element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Object} dictAttributes A dictionary of attributes to set for the
   *                           svg element
   * @param {String} seId The Svg element data matrix key (See the
   *                      constants file)
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.setAttribute = function(
    svgElement,
    seId,
    dictAttributes
  ) {
    // First set the default attributes
    setDefaultAttribute(svgElement, dictAttributes, seId);

    // Loop through the attributes and set the values
    foreach(
      var key in
        dictAttributes
    ) {
      if (dictAttributes.hasOwnProperty(key)) {
        if (isValid(key, seId))
          svgElement.attr(getAttribute(key), dictAttributes[key]);
      }
    }
  };

  /**
   * Create SVG path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params An array of values
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createPath = function(svgElement, params) {
    var dvalue = "";
    for (var i = 0; i < params.length;) {
      if (params[i] === "M") {
        dvalue += "M{0},{1}".formatArray(params, i + 1, 2);
        i += 3;
      } else if (params[i] === "L") {
        dvalue += "L{0},{1}".formatArray(params, i + 1, 2);
        i += 3;
      } else if (params[i] === "H") {
        dvalue += "H{0}".formatArray(params, i + 1, 1);
        i += 2;
      } else if (params[i] === "V") {
        dvalue += "V{0}".formatArray(params, i + 1, 1);
        i += 2;
      } else if (params[i] === "C") {
        dvalue += "C{0},{1} {2},{3} {4},{5}".formatArray(params, i + 1, 6);
        i += 7;
      } else if (params[i] === "A") {
        dvalue += "A{0},{1} {2},{3},{4} {5},{6}".formatArray(params, i + 1, 6);
        i += 8;
      } else if (params[i] === "S") {
        dvalue += "S{0},{1} {2},{3}".formatArray(params, i + 1, 4);
        i += 5;
      } else if (params[i] === "Z") {
        dvalue += "Z";
        i += 1;
        break;
      } else {
        console.error("Unknown variable in path array: " + params[i]);
      }
    }

    return svgElement.append("path").attr("d", dvalue);
  }

  /**
   * Add a marker
   * @param {Object} defsElement The d3 object which corresponds to the svg
   *                            element
   * @param {jsonData} params The json data matrix item
   * @returns {Object} A svg defs d3 object
   */
  this.createMarkerObj = function(
    defsElement,
    params
  ) {
    return defsElement.append("marker")
      .attr("id", params.id)
      .attr("viewbox", "0 0 {0} {1}".formatArray(params.s))
      .attr("refX", params.rx)
      .attr("refY", params.ry)
      .attr("markerWidth", params.mw || "6")
      .attr("markerHeight", params.mh || "6")
      .attr("orient", params.or || "auto");
  }

  /**
   * Append arrow element to the end of a path
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createArrowHead = function(svgElement) {
    svgElement.append("svg:defs").append("svg:marker")
      .attr("id", "triangle")
      .attr("refX", 6)
      .attr("refY", 6)
      .attr("markerWidth", 30)
      .attr("markerHeight", 30)
      .attr("markerUnits", "userSpaceOnUse")
      .attr("orient", "auto")
      .append("path")
      .attr("d", "M 0 0 12 6 0 12 3 6")
      .style("fill", "black");
  };

  /**
   * Append arrow element to the end of a path
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Object} svgPath - The SVG path element
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.appendArrowHead = function(svgElement,
    svgPath) {
    return svgPath.attr("marker-end", "url(#triangle)");
  };

  /**
   * Create a SVG polygon
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createPolygon = function(svgElement,
    params
  ) {
    var pointsString = "";
    for (var i = 0; i < params.length - 1; i += 2) {
      pointsString += "{0},{1}".formatArray(params, i, 2);
    }
    return svgElement.append("polygon").attr("points", pointsString);
  };

  /**
   * Create a SVG path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Object} params The path string, the value for 'd' attribute in
   *                        path element
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createPolyline = function(
    svgElement,
    params
  ) {
    var pointsString = "";
    for (var i = 0; i < dValueArray.length - 1; i += 2) {
      pointsString += "{0},{1}".formatArray(params, i, 2);
    }
    return svgElement.append("polyline").attr("points", pointsString);
  };

  /**
   * Create a SVG circle element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createCircle = function(
    svgElement,
    params
  ) {
    return svgElement.append("circle")
                     .attr("cx", params[0])
                     .attr("cy", params[1])
                     .attr("r", params[2]);
  };

  /**
   * Create a SVG ellipse element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createEllipse = function(
    svgElement,
    params
  ) {
    return svgElement.append("ellipse")
                     .attr("cx", params[0])
                     .attr("cy", params[1])
                     .attr("rx", params[2])
                     .attr("ry", params[3]);
  };

  /**
   * Create a SVG rectangle element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createRect = function(
    svgElement,
    params
  ) {
    if(params.length === 2)
      return svgElement.append("rect")
                       .attr("width", params[0])
                       .attr("height", params[1]);
    else
      return svgElement.append("rect")
                       .attr("x", params[0])
                       .attr("y", params[1])
                       .attr("width", params[2])
                       .attr("height", params[3]);
  };

  /**
   * Create a SVG Text element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createText = function(
    svgElement,
    params
  ) {
    return svgElement.append("text")
                     .attr("x", params[0])
                     .attr("y", params[1])
                     .text(params[2]);
  };

  /**
   * Create a SVG Curve element using path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   * @see http://vanseodesign.com/web-design/svg-paths-curve-commands/
   */
  this.createCubicCurve = function(svgElement,
    params
  ) {
    return svgElement.append("path")
      .attr("d", "M{0},{1} C{2},{3} {4},{5} {6},{7}".formatArray(params);
  };

  /**
   * Create a SVG Arc element using path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} params The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createArc = function(
    svgElement,
    params
  ) {
    return svgElement.append("path")
      .attr("d", "M{0},{1} A{2},{3} {4},{5},{6} {7},{8}".formatArray(params);
  };

  /**
   * Make a SVG path element dashed
   * @param {Object} svgObject The Svg element item
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.appendDashed = function(svgObject) {
    return svgPath.attr("stroke-dasharray", "3,3");
  };

  /**
   * Update the SVG element stroke attribute
   * @param {Object} svgObject The Svg element item
   * @param {Color} stroke The stroke color
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.updateStroke = function(svgObject, stroke) {
    return svgObject.attr("stroke", stroke);
  };

  /**
   * Update the SVG element stroke width attribute
   * @param {Object} svgObject The Svg element item
   * @param {Number} strokeWidth The width of the stroke
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.updateStrokeWidth = function(svgObject, strokeWidth) {
    return svgObject.attr("stroke-width", strokeWidth);
  };

  /**
   * Update the SVG element fill attribute
   * @param {Object} svgObject The Svg element item
   * @param {Color} fill The fill
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.updateFill = function(svgObject, fill) {
    return svgObject.attr("fill", fill);
  };

  /**
   * Add Id to SVG element
   * @param {Object} svgObject The Svg element item
   * @param {String} id The Svg element id value
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.appendId = function(svg_element,
    id) {
    return svg_element.attr("id", id);
  };

};
