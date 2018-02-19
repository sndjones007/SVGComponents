/**
 * Created by sniogi on 2/8/2018.
 */
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
 * String format method
 * Inspired by http://bit.ly/juSAWl
 * https://gist.github.com/tbranyen/1049426
 * Augment String.prototype to allow for easier formatting.  This implementation
 * doesn't completely destroy any existing String.prototype.format functions,
 * and will stringify objects/arrays.
 * @param {Object} svgElement The d3 object which corresponds to the svg
 *                            element
 * @param {Array} params An array of values
 * @returns {Object} The d3 object which corresponds to the svg element
 */
String.prototype.formatArray = function(params, start, count) {
  var str = this,
    loopMax = (count != null && params.length > count)?
                  params.length : count,
    start = start || 0;

  // For each {0} {1} {n...} replace with the argument in that position.  If
  // the argument is an object or an array it will be stringified to JSON.
  for (var i = start; i < loopMax; ++i) {
    var arg = params[i];
    var safe = typeof arg === 'object' ? JSON.stringify(arg) : arg;
    str = str.replace(RegExp('\\{' + (i) + '\\}', 'g'), safe);
  }

  return str;
};

/**
 * A class which contains all common methods used throughout application
 * @class
 */
var DmCommon = new function() {

  // Public Methods

  /**
   * Help method for Svg elements
   * @return {Json} Dictionary of key value
   */
  this.HelpSvgElements = {
      l: ["line", "Svg Line element",
          [{
            x1: "The x-axis coordinate of the start of the line. Default 0",
            y1: "The y-axis coordinate of the start of the line. Default 0",
            x2: "The x-axis coordinate of the end of the line. Default 0",
            y2: "The y-axis coordinate of the end of the line. Default 0"
          }]],
      lh: ["line",
          "Svg Line element. Specific for Horizontal lines (single Y-Coordinate)",
          {
            x1: "The x-axis coordinate of the start of the line. Default 0",
            x2: "The x-axis coordinate of the end of the line. Default 0",
            y: "The y-axis coordinate of the line. Default 0"
          }],
      lv: ["line",
          "Svg Line element. Specific for vertical lines (single X-Coordinate)",
          {
            x: "The x-axis coordinate of the line. Default 0",
            y1: "The y-axis coordinate of the start of the line. Default 0",
            y2: "The y-axis coordinate of the end of the line. Default 0"
          }],
      pg: ["polygon", "Svg Polygon element",
          {
            points: "A list of points (x,y)"
          }],
      pl: ["polyline", "Svg Polyline element",
          {
            points: "A list of points (x,y)"
          }],
      t: ["text", "Svg Text element",
          {
            x: "The new absolute X coordinate for the current text position",
            y: "the new absolute Y coordinate for the current text position",
            value: "The element text defines the text to be written"
          }],
      c: ["circle", "Svg Circle element",
          {
            cx: "The x-axis coordinate of the center of the circle. Default 0",
            cy: "The y-axis coordinate of the center of the circle. Default 0",
            r: "The radius of the circle"
          }],
      el: ["ellipse", "Svg Ellipse element",
          {
            cx: "The x-axis coordinate of the center of the ellipse. Default 0",
            cy: "The y-axis coordinate of the center of the ellipse. Default 0",
            rx: "The x-axis radius of the ellipse",
            ry: "The y-axis radius of the ellipse"
          }],
      r: ["rect", "Svg Rectangle element",
         {
           x: "The x-axis coordinate of the side of the rectangle which has the smaller x-axis coordinate value in the current user coordinate system",
           y: "The y-axis coordinate of the side of the rectangle which has the smaller y-axis coordinate value in the current user coordinate system",
           width: "The width of the rectangle",
           height: "he height of the rectangle",
           rx: "For rounded rectangles, the x-axis radius of the ellipse used to round off the corners of the rectangle",
           ry: "For rounded rectangles, the y-axis radius of the ellipse used to round off the corners of the rectangle"
         }],
      cc: ["path", "A Svg Path element defines Cubic Beizer Curve. Start a new sub-path at the given (x,y) coordinate. Draws a cubic Bézier curve from the current point to (x,y) using (x1,y1) as the control point at the beginning of the curve and (x2,y2) as the control point at the end of the curve. S(x2,y2) next specifies the next curve with first control point is the reflection of the last control point.",
          {
            Mx: "Start from x-coordinate",
            My: "Start from y-coordinate",
            Cx1: "X-coordinate of the control point at the beginning of curve",
            Cy1: "Y-coordinate of the control point at the beginning of curve",
            Cx2: "X-coordinate of the control point at the end of curve",
            Cy2: "Y-coordinate of the control point at the end of curve",
            Sx2: "X-coordinate of the control point at the end of curve",
            Sy2: "Y-coordinate of the control point at the end of curve"
          }],
      ac: ["path", "A Svg path element defines Arc. Draws an elliptical arc from the current point to (x, y). The size and orientation of the ellipse are defined by two radii (rx, ry)",
          {
            Mx: "Start from x-coordinate",
            My: "Start from y-coordinate",
            rx: "The x-axis radius of the ellipse",
            ry: "The y-axis radius of the ellipse",
            xAxisRotation: "The x-axis of the ellipse is rotated by x-axis-rotation relative to the x-axis of the current coordinate system",
            largeArcFlag: "Of the four candidate arc sweeps, two will represent an arc sweep of greater than or equal to 180 degrees (the 'large-arc'), and two will represent an arc sweep of less than or equal to 180 degrees (the 'small-arc'). Allowed values 0 and 1",
            sweepFlag: "If sweep-flag is '1', then the arc will be drawn in a 'positive-angle' direction"
          }],
      df: ["defs", "A svg definitions tag"],
      pt: ["path", "A svg path element. A 'd' attribute of the path"],
      mk: ["marker", "The graphics that is to be used for drawing arrowheads or polymarkers on a given ‘path’, ‘line’, ‘polyline’ or ‘polygon’ element",
          {
            refX: "The x-axis coordinate of the reference point which is to be aligned exactly at the marker position",
            refY: "The y-axis coordinate of the reference point which is to be aligned exactly at the marker position",
            markerWidth: "The width of the viewport into which the marker is to be fitted when it is rendered",
            markerHeight: "The height of the viewport into which the marker is to be fitted when it is rendered",
            orient: "How the marker is rotated"
          }]
    };

  /**
   * Help method for Svg element attributes
   * @return {Json} Dictionary of key value
   */
  this.HelpSvgElements = {
      sz: ["size", "Defines size of the svg / viewbox"],
      st: ["stroke", "Defines the color of a line, text or outline of an element"],
      sw: ["stroke-width", "Defines the thickness of a line, text or outline of an element"],
      slc: ["stroke-linecap", "Defines different types of endings to an open path"],
      fl: ["fill", "A presentation attribute that define the color of the interior of the given graphical element"],
      ff: ["font-family", "Font family will be used to render the text"],
      fs: ["font-size", "Size of the font from baseline to baseline"],
      id: ["id", "Unique identifier attribute"],
      ta: ["text-anchor", "Used to align (start-, middle- or end-alignment) a string of text relative to a given point"],
      ms: ["marker-start", "The arrowhead or polymarker that will be drawn at the first vertex of the given <path> element or basic shape"],
      mm: ["marker-mid", "The arrowhead or polymarker that shall be drawn at every vertex other than the first and last vertex of the given <path> element or basic shape"],
      me: ["marker-end", "The arrowhead or polymarker that will be drawn at the final vertex of the given <path> element or basic shape"]
  };

  /**
   * @param {String} key The key attribute name of the element
   * @param {String} seId The Svg element data matrix key (See the
   *                      constants file)
   * @return {Boolean} True if the attribute is valid for the element else false
   */
  this.isSvgKeyValid = function(key, seId) {
    return key.match(/^(st|sw|slc|fl|id)$/) ||
      (seId === "t" && (key.match(/^(ta|ff|fs)$/))) ||
      (key.match(/^(ms|mm|me)$/) && seId.match(/^(l|lh|lv|pt\d+|pl|pg)$/))
        ;
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
  this.setDefaultAttribute = function(
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

    return dictAttributes;
  };

  /**
   * Generate unique Id
   * @return {String} Return an unique id string
   */
  var generateID = function() {
    // Math.random should be unique because of its seeding algorithm.
    // Convert it to base 36 (numbers + letters), and grab the first 9 characters
    // after the decimal.
    return '_' + Math.random().toString(36).substr(2, 9);
  };

  /**
   * Generate unique Id for svg
   * @return {String} Return an unique id string
   */
  this.generateSvgID = function() {
    // Math.random should be unique because of its seeding algorithm.
    // Convert it to base 36 (numbers + letters), and grab the first 9 characters
    // after the decimal.
    return 'svg' + generateID();
  };

  /**
   * Generate unique Id for div
   * @return {String} Return an unique id string
   */
  this.generateDivID = function() {
    // Math.random should be unique because of its seeding algorithm.
    // Convert it to base 36 (numbers + letters), and grab the first 9 characters
    // after the decimal.
    return 'div' + generateID();
  };

  /**
   * Generate unique Id for Svg item
   * @return {String} Return an unique id string
   */
  this.generateSvgItemID = function() {
    // Math.random should be unique because of its seeding algorithm.
    // Convert it to base 36 (numbers + letters), and grab the first 9 characters
    // after the decimal.
    return 'si' + generateID();
  };
};
