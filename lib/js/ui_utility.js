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
  * A class which is is used for display purpose.
  * @class
  */
var UiUtility = new function(){

  // Initialize class
  function UiUtility() {};

  // Private methods

  /**
   * Get the name of the UI card from the Id
   * @param {String} nameId The html id of the element inside which the Svg
   *                      element is to be rendered
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  var getName = function (nameId) {
      var name = "";
      var make_uppercase = true;
      for (var i = 0; i < nameId.length; i++) {
          var chr = nameId.charAt(i);

          if (make_uppercase) {
              name += chr.toUpperCase();
              make_uppercase = false;
          }
          else if (chr === '-') {
              name += ' ';
              make_uppercase = true;
          }
          else name += chr;
      }

      return name;
  }

  /**
   * Get the name of the UI card from the Id
   * @param {String} nameId The html id of the element inside which the Svg
   *                      element is to be rendered
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  var getSvgId = function (nameId) {
      return nameId + "-svg";
  }

  // Public methods

  /**
   * Add Id to SVG element
   * @param {String} name The html id of the element inside which the Svg
   *                      element is to be rendered
   * @param {Number} svgWidth The width of the SVG graphics
   * @param {Number} svgHeight The height of the SVG graphics
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.createDivCard = function (
    name,
    svgWidth,
    svgHeight) {
      var svgId = getSvgId(name);
      var divContainer = d3.select("#" + name).append("div")
                            .attr("class", "card text-center");
      var divContainerChild = divContainer.append("div")
                                  .attr("class", "card-block")
                                  .append("div").attr("id", svgId);

      divContainer.append("div").attr("class", "card-footer text-muted")
                   .append("small").text(getName(name));

      return D3Wrapper.createSvg(svgId, svgWidth, svgHeight);
  };

    /**
     * Create a section card
     * @param {String} name The html id of the element inside which the Svg
     *                      element is to be rendered
     * @param {Number} svgWidth The width of the SVG graphics
     * @param {Number} svgHeight The height of the SVG graphics
     * @returns {Object} The d3 object which corresponds to the svg element
     */
  this.createSvgDivCard = function (name, svgWidth,
  svgHeight) {
    return d3.select("#" + name).append("svg")
            .attr("width", svgWidth)
            .attr("height", svgHeight);
  }

  /**
   * Create a section card
   * @param {String} name The html id of the element inside which the Svg
   *                      element is to be rendered
   * @param {Number} svgWidth The width of the SVG graphics
   * @param {Number} svgHeight The height of the SVG graphics
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.getDivCard = function (name) {
    return d3.select("#" + name);
  }
};
