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
var D3Wrapper = new function(){

  // Public functions

  /**
   * Create SVG Graphics container
   * @param {String} name The html element id text inside which SVG is to be
   *                      rendered
   * @param {Number} svgWidthOrParams The width of the SVG graphics OR
   *                      The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} svgHeight The height of the SVG graphics
   * @returns {Object}
   */
  this.createSvg = function (name, svgWidthOrParams,
    svgHeight, d3Parent) {
      var svgWidth = svgWidthOrParams;

      if(Array.isArray(svgWidthOrParams)) {
        svgWidth = SvgCommon.getDefault(svgWidthOrParams[0], svgWidth);
        svgHeight = SvgCommon.getDefault(svgWidthOrParams[1], svgHeight);
      }

      d3Parent = d3Parent || d3;
      return d3Parent.select("#" + name)
               .append("svg")
               .attr("width", svgWidth)
               .attr("height", svgHeight);
  };

  /**
   * Create SVG line element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} x1OrParams The X-Coordinate of starting point of the line OR
   *                        The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} y1 The Y-Coordinate of starting point of the line
   * @param {Number} x2 The X-Coordinate of end point of the line
   * @param {Number} y2 The Y-Coordinate of end point of the line
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createLine = function (
    svgElement,
    x1OrParams,
    y1,
    x2,
    y2) {
      var x1 = x1OrParams;

      if(Array.isArray(x1OrParams)) {
        x1 = SvgCommon.getDefault(x1OrParams[0], x1);
        y1 = SvgCommon.getDefault(x1OrParams[1], y1);
        x2 = SvgCommon.getDefault(x1OrParams[2], x2);
        y2 = SvgCommon.getDefault(x1OrParams[3], y2);
      }

      return svgElement.append("line")
                          .attr("x1", x1)
                          .attr("y1", y1)
                          .attr("x2", x2)
                          .attr("y2", y2);
  };

  /**
   * Create SVG horizontal line element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} x1OrParams The X-Coordinate of starting point of the line OR
   *                  The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} x2 The X-Coordinate of end point of the line
   * @param {Number} y The Y-Coordinate of point of the line
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createLineHoriz = function (
    svgElement,
    x1OrParams,
    x2,
    y) {
      var x1 = x1OrParams;

      if(Array.isArray(x1OrParams)) {
        x1 = SvgCommon.getDefault(x1OrParams[0], x1);
        x2 = SvgCommon.getDefault(x1OrParams[1], x2);
        y = SvgCommon.getDefault(x1OrParams[2], y);
      }

      return D3Wrapper.createLine(svgElement, x1, y, x2, y);
  };


  /**
   * Create SVG vertical line element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} xOrParams The X-Coordinate of point of the line OR
   *                    The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} y1 The Y-Coordinate of start point of the line
   * @param {Number} y2 The Y-Coordinate of end point of the line
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createLineVert = function (
    svgElement,
    xOrParams,
    y1,
    y2) {
      var x = xOrParams;

      if(Array.isArray(xOrParams)) {
        x = SvgCommon.getDefault(xOrParams[0], x);
        y1 = SvgCommon.getDefault(xOrParams[1], y1);
        y2 = SvgCommon.getDefault(xOrParams[2], y2);
      }

      return D3Wrapper.createLine(svgElement, x, y1, x, y2);
  };

  this.createPath = function(svgElement, params){
    var dvalue = "";
    for(var i = 0; i < params.length;) {
      if(params[i] === "M") {
        dvalue += "M" + params[i + 1] + "," + params[i + 2] + " ";
        i += 3;
      } else if(params[i] === "L") {
        dvalue += "L" + params[i + 1] + "," + params[i + 2] + " ";
        i += 3;
      } else if(params[i] === "H") {
        dvalue += "H" + params[i + 1] + " ";
        i += 2;
      } else if(params[i] === "V") {
        dvalue += "V" + params[i + 1]+ " ";
        i += 2;
      } else if(params[i] === "C") {
        dvalue += "C" + params[i + 1] + "," + params[i + 2] + " " + params[i + 3] +
          "," + params[i + 4] + " " + params[i + 5] + "," + params[i + 6]+ " ";
        i += 7;
      } else if(params[i] === "A") {
        dvalue += "A" + params[i + 1] + "," + params[i + 2] + " " + params[i + 3] +
          "," + params[i + 4] + "," + params[i + 5] + " " + params[i + 6] + "," +
          params[i + 7];
        i += 8;
      } else if(params[i] === "S") {
        dvalue += "S" + params[i + 1] + "," + params[i + 2] + " " + params[i + 3] +
          "," + params[i + 4];
        i += 5;
      } else if(params[i] === "Z") break;
      else {
        console.error("Unknown variable in path array: " + params[i]);
      }
    }

    return svgElement.append("path").attr("d", dvalue);
  }

  /**
   * Append arrow element to the end of a path
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Object} svgPath - The SVG path element
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createArrowHead = function (svgElement) {
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
  this.appendArrowHead = function (
    svgElement,
    svgPath) {
      return svgPath.attr("marker-end", "url(#triangle)");
  };

  /**
   * Create a SVG triangle
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} x1OrParams The X-Coordinate of starting point of the triangle
   *                      OR The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} y1 The Y-Coordinate of starting point of the triangle
   * @param {Number} x2 The X-Coordinate of second point of the triangle
   * @param {Number} y2 The Y-Coordinate of second point of the triangle
   * @param {Number} x3 The X-Coordinate of last point of the triangle
   * @param {Number} y3 The Y-Coordinate of last point of the triangle
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createTriangle = function (
    svgElement,
    x1OrParams,
    y1,
    x2,
    y2,
    x3,
    y3) {
      var x1 = x1OrParams;

      if(Array.isArray(x1OrParams)) {
        x1 = SvgCommon.getDefault(x1OrParams[0], x1);
        y1 = SvgCommon.getDefault(x1OrParams[1], y1);
        x2 = SvgCommon.getDefault(x1OrParams[2], x2);
        y2 = SvgCommon.getDefault(x1OrParams[3], y2);
        x3 = SvgCommon.getDefault(x1OrParams[4], x3);
        y3 = SvgCommon.getDefault(x1OrParams[5], y3);
      }

      return svgElement.append("polygon").attr("points", x1 + "," + y1 + " " +
          x2 + "," + y2 + " " +
          x3 + "," + y3);
  };

  /**
   * Create a SVG path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Object} dValue The path string, the value for 'd' attribute in
   *                        path element
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createPolyline = function(
    svgElement,
    dValueArray) {
      var pointsString = "";
      for(var i = 0; i < dValueArray.length - 1; i += 2) {
          pointsString += dValueArray[i] + "," + dValueArray[i + 1] + " ";
      }
      return svgElement.append("polyline").attr("points", pointsString);
  };

  /**
   * Create a SVG circle element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} cxOrParams The X-Coordinate of center OR
   *                        The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} cy The Y-Coordinate of center
   * @param {Number} radius The radius of the circle
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createCircle = function(
    svgElement,
    cxOrParams,
    cy,
    radius) {
      var cx = cxOrParams;

      if(Array.isArray(cxOrParams)) {
        cx = SvgCommon.getDefault(cxOrParams[0], cx);
        cy = SvgCommon.getDefault(cxOrParams[1], cy);
        radius = SvgCommon.getDefault(cxOrParams[2], radius);
      }

      return svgElement.append("circle").attr("cx", cx).attr("cy", cy).attr("r", radius);
  };

  /**
   * Create a SVG rectangle element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} xOrParams The X-Coordinate of top left corner of the rectangle
   *                      OR The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} y The Y-Coordinate of top left corner of the rectangle
   * @param {Number} width The width of the rectangle
   * @param {Number} height The height of the rectangle
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createRect = function(
    svgElement,
    xOrParams,
    y,
    width,
    height) {
      var x = xOrParams;

      if(Array.isArray(xOrParams)) {
        x = SvgCommon.getDefault(xOrParams[0], x);
        y = SvgCommon.getDefault(xOrParams[1], y);
        width = SvgCommon.getDefault(xOrParams[2], width);
        height = SvgCommon.getDefault(xOrParams[3], height);
      }

      return svgElement.append("rect")
                  .attr("x", x).attr("y", y)
                  .attr("width", width).attr("height", height);
  };

  /**
   * Create a SVG Text element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} xOrParams The X-Coordinate of top left corner of the rectangle
   *                      OR The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} y The Y-Coordinate of top left corner of the rectangle
   * @param {Number} width The width of the rectangle
   * @param {Number} height The height of the rectangle
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createText = function(
    svgElement,
    xOrParams,
    y,
    text,
    fontSize,
    fontFamily) {
      var x = xOrParams;

      if(Array.isArray(xOrParams)) {
        x = SvgCommon.getDefault(xOrParams[0], x);
        y = SvgCommon.getDefault(xOrParams[1], y);
        text = SvgCommon.getDefault(xOrParams[2], text);
        fontSize = SvgCommon.getDefault(xOrParams[3], fontSize);
        fontFamily = SvgCommon.getDefault(
            SvgCommon.getDefault(xOrParams[4], fontFamily), 'KaTeX_Math');
      }

      return svgElement.append("text")
                  .attr("x", x).attr("y", y)
                  .attr("font-family", fontFamily)
                  .attr("font-size", fontSize + "px")
                  .attr("text-anchor", "start")
                  .text(text);
  };

  /**
   * Create a SVG Curve element using path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} MxOrParams MoveTo X-Coordinate OR
   *                      The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} My MoveTo Y-Coordinate
   * @param {Number} x1 The control point at beginning of curve (X-Axis)
   * @param {Number} y1 The control point at beginning of curve (Y-Axis)
   * @param {Number} x2 The control point at end of curve (X-Axis)
   * @param {Number} y2 The control point at end of curve (Y-Axis)
   * @param {Number} x The end point of curve (X-Axis)
   * @param {Number} y The end point of curve (Y-Axis)
   * @returns {Object} The d3 object which corresponds to the svg element
   * @see http://vanseodesign.com/web-design/svg-paths-curve-commands/
  */
  this.createCubicCurve = function (
    svgElement,
    MxOrParams,
    My,
    x1,
    y1,
    x2,
    y2,
    x,
    y) {
      var Mx = MxOrParams;

      if(Array.isArray(MxOrParams)) {
        Mx = SvgCommon.getDefault(MxOrParams[0], Mx);
        My = SvgCommon.getDefault(MxOrParams[1], My);
        x1 = SvgCommon.getDefault(MxOrParams[2], x1);
        y1 = SvgCommon.getDefault(MxOrParams[3], y1);
        x2 = SvgCommon.getDefault(MxOrParams[4], x2);
        y2 = SvgCommon.getDefault(MxOrParams[5], y2);
        x = SvgCommon.getDefault(MxOrParams[6], x);
        y = SvgCommon.getDefault(MxOrParams[7], y);
      }

      return svgElement.append("path")
                  .attr("d",
                  "M" + Mx + " " + My + " C " + x1 + " " + y1 +
                  ", " + x2 + " " + y2 + ", " + x + " " + y);
  };

  /**
   * Create a SVG Arc element using path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} MxOrParams MoveTo X-Coordinate OR
   *                        The array of rest of arguments. If the array is
   *                      present then use this than the next set of args.
   * @param {Number} My MoveTo Y-Coordinate
   * @param {Number} rx The x-radius of the ellipse
   * @param {Number} ry The y-radius of the ellipse
   * @param {Number} xrotate The degrees to rotate the x-axis
   * @param {Number} largeArcFlag A value of 0 means to use the smaller arc,
   *                              while a value of 1 means use the larger arc
   * @param {Number} sweepFlag Determines whether to use an arc (0) or its
   *                           reflection around the axis (1)
   * @param {Number} x The end point for the arc (X-Axis)
   * @param {Number} y The end point for the arc (Y-Axis)
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createArc = function (
    svgElement,
    MxOrParams,
    My,
    rx,
    ry,
    xrotate,
    largeArcFlag,
    sweepFlag,
    x,
    y) {
      var Mx = MxOrParams;

      if(Array.isArray(MxOrParams)) {
        Mx = SvgCommon.getDefault(MxOrParams[0], Mx);
        My = SvgCommon.getDefault(MxOrParams[1], My);
        rx = SvgCommon.getDefault(MxOrParams[2], rx);
        ry = SvgCommon.getDefault(MxOrParams[3], ry);
        xrotate = SvgCommon.getDefault(MxOrParams[4], xrotate);
        largeArcFlag = SvgCommon.getDefault(MxOrParams[5], largeArcFlag);
        sweepFlag = SvgCommon.getDefault(MxOrParams[6], sweepFlag);
        x = SvgCommon.getDefault(MxOrParams[7], x);
        y = SvgCommon.getDefault(MxOrParams[8], y);
      }

      return svgElement.append("path")
                  .attr("d",
                  "M" + Mx + " " + My + " A " + rx + " " + ry + " "
                  + xrotate + " " + largeArcFlag + " " + sweepFlag +
                  " " + x + " " + y);
  };

  /**
   * A generic attribute appender
   * @param {Object} svgObject The Svg element item
   * @param {Object} stroke The stroke of the element
   * @param {Number} strokeWidth The stroke width
   * @param {Object} fill The fill style
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.appendAttributeDefault1 = function (
    svgObject,
    stroke,
    strokeWidth,
    fill) {
      return svgObject.attr("stroke", (stroke === undefined) ? "black" : stroke)
          .attr("stroke-width", (strokeWidth === undefined) ? 1 : strokeWidth)
          .attr("fill", (fill === undefined) ? "none" : fill);
  }

  /**
   * Make a SVG path element dashed
   * @param {Object} svgObject The Svg element item
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.appendDashed = function (svgObject) {
      return svgPath.attr("stroke-dasharray", "3,3");
  };

  /**
   * Update the SVG element stroke attribute
   * @param {Object} svgObject The Svg element item
   * @param {Color} stroke The stroke color
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.updateStroke = function (svgObject, stroke) {
      return svgObject.attr("stroke", stroke);
  };

  /**
   * Update the SVG element stroke width attribute
   * @param {Object} svgObject The Svg element item
   * @param {Number} strokeWidth The width of the stroke
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.updateStrokeWidth = function (svgObject, strokeWidth) {
      return svgObject.attr("stroke-width", strokeWidth);
  };

  /**
   * Update the SVG element fill attribute
   * @param {Object} svgObject The Svg element item
   * @param {Color} fill The fill
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.updateFill = function (svgObject, fill) {
      return svgObject.attr("fill", fill);
  };

  /**
   * Add Id to SVG element
   * @param {Object} svgObject The Svg element item
   * @param {String} id The Svg element id value
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.appendId = function (
    svg_element,
    id) {
      return svg_element.attr("id", id);
  };

};
