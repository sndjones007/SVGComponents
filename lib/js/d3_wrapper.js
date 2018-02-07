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
var D3Wrapper = (function(){

  // Initialize class
  function D3Wrapper() {};

  // Public functions

  /**
   * Create SVG Graphics container
   * @param {String} name The html element id text inside which SVG is to be
   *                      rendered
   * @param {Number} svgWidth The width of the SVG graphics
   * @param {Number} svgHeight The height of the SVG graphics
   * @returns {Object}
   */
  D3Wrapper.prototype.createSvg = function (
    name,
    svgWidth,
    svgHeight) {
      return d3.select("#" + name)
               .append("svg")
               .attr("width", svgWidth)
               .attr("height", svgHeight);
  };

  /**
   * Create SVG line element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} x1 The X-Coordinate of starting point of the line
   * @param {Number} y1 The Y-Coordinate of starting point of the line
   * @param {Number} x2 The X-Coordinate of end point of the line
   * @param {Number} y2 The Y-Coordinate of end point of the line
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.createLine = function (
    svgElement,
    x1,
    y1,
    x2,
    y2) {
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
   * @param {Number} x1 The X-Coordinate of starting point of the line
   * @param {Number} x2 The X-Coordinate of end point of the line
   * @param {Number} y The Y-Coordinate of point of the line
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.createLineHoriz = function (
    svgElement,
    x1,
    x2,
    y) {
      return this.createLine(svgElement, x1, y, x2, y);
  };


  /**
   * Create SVG vertical line element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} x The X-Coordinate of point of the line
   * @param {Number} y1 The Y-Coordinate of start point of the line
   * @param {Number} y2 The Y-Coordinate of end point of the line
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.createLineVert = function (
    svgElement,
    x,
    y1,
    y2) {
      return this.createLine(svgElement, x, y1, x, y2);
  };

  /**
   * Append arrow element to the end of a path
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Object} svgPath - The SVG path element
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.appendArrowHead = function (
    svgElement,
    svgPath) {
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
      return svgPath.attr("marker-end", "url(#triangle)");
  };

  /**
   * Create a SVG triangle
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} x1 The X-Coordinate of starting point of the triangle
   * @param {Number} y1 The Y-Coordinate of starting point of the triangle
   * @param {Number} x2 The X-Coordinate of second point of the triangle
   * @param {Number} y2 The Y-Coordinate of second point of the triangle
   * @param {Number} x3 The X-Coordinate of last point of the triangle
   * @param {Number} y3 The Y-Coordinate of last point of the triangle
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.createTriangle = function (
    svgElement,
    x1,
    y1,
    x2,
    y2,
    x3,
    y3) {
      svgElement.append("path").attr("d", d3.line()([
          [x1, y1],
          [x2, y2],
          [x3, y3]
      ]));
  };

  /**
   * Create a SVG path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Object} dValue The path string, the value for 'd' attribute in
   *                        path element
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.createPath = function(
    svgElement,
    dValue) {
      svgElement.append("path").attr("d", dValue);
  };

  /**
   * Create a SVG circle element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} cx The X-Coordinate of center
   * @param {Number} cy The Y-Coordinate of center
   * @param {Number} radius The radius of the circle
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.createCircle = function(
    svgElement,
    cx,
    cy,
    radius) {
      return svgElement.append("circle").attr("cx", cx).attr("cy", cy).attr("r", radius);
  };

  /**
   * Create a SVG rectangle element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} x The X-Coordinate of top left corner of the rectangle
   * @param {Number} y The Y-Coordinate of top left corner of the rectangle
   * @param {Number} width The width of the rectangle
   * @param {Number} height The height of the rectangle
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.createRect = function(
    svgElement,
    x,
    y,
    width,
    height) {
      svgElement.append("rect")
                  .attr("x", x).attr("y", y)
                  .attr("width", width).attr("height", height);
  };

  /**
   * Create a SVG Text element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} x The X-Coordinate of top left corner of the rectangle
   * @param {Number} y The Y-Coordinate of top left corner of the rectangle
   * @param {Number} width The width of the rectangle
   * @param {Number} height The height of the rectangle
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.createText = function(
    svgElement,
    text,
    x,
    y,
    fontFamily,
    fontSize) {
      return svgElement.append("text")
                  .attr("x", x).attr("y", y)
                  .attr("font-family", fontFamily)
                  .attr("font-size", fontSize + "px")
                  .attr("text-anchor", "middle")
                  .text(text);
  };

  /**
   * Create a SVG Curve element using path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} Mx MoveTo X-Coordinate
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
  D3Wrapper.prototype.createCubicCurve = function (
    svgElement,
    Mx,
    My,
    x1,
    y1,
    x2,
    y2,
    x,
    y) {
      return svgElement.append("path")
                  .attr("d",
                  "M" + Mx + " " + My + " C " + x1 + " " + y1 +
                  ", " + x2 + " " + y2 + ", " + x + " " + y);
  };

  /**
   * Create a SVG Arc element using path element
   * @param {Object} svgElement The d3 object which corresponds to the svg
   *                            element
   * @param {Number} Mx MoveTo X-Coordinate
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
  D3Wrapper.prototype.createArc = function (
    svgElement,
    Mx,
    My,
    rx,
    ry,
    xrotate,
    largeArcFlag,
    sweepFlag,
    x,
    y) {
      return svgElement.append("path")
                  .attr("d",
                  "M" + Mx + " " + My + " A " + rx + " " + ry + " "
                  + xrotate + " " + largeArcFlag + " " + sweepFlag +
                  " " + endx + " " + endy);
  };

  /**
   * A generic attribute appender
   * @param {Object} svgObject The Svg element item
   * @param {Object} stroke The stroke of the element
   * @param {Number} strokeWidth The stroke width
   * @param {Object} fill The fill style
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.appendAttribute1 = function (
    svgObject,
    stroke,
    strokeWidth,
    fill) {
      return svg_object.attr("stroke", (stroke === undefined) ? "black" : stroke)
          .attr("stroke-width", (strokeWidth === undefined) ? 1 : strokeWidth)
          .attr("fill", (fill === undefined) ? "none" : fill);
  }

  /**
   * Make a SVG path element dashed
   * @param {Object} svgPath The Svg element item
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.appendDashed = function (svgPath) {
      return svgPath.attr("stroke-dasharray", "3,3");
  };

  /**
   * Add Id to SVG element
   * @param {Object} svgObject The Svg element item
   * @param {String} id The Svg element id value
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  D3Wrapper.prototype.appendId = function (
    svg_element,
    id) {
      return svg_element.attr("id", id);
  }

  return D3Wrapper;

})();

// export the class
module.exports = D3Wrapper;
