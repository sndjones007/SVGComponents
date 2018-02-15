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

  // Private methods

  /**
   * Get the name of the UI card from the Id. The Data matrix name id is
   * seperated by either '-' or '.'
   * @param {String} nameId The html id of the element inside which the Svg
   *                      element is to be rendered
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  var getName = function (nameId) {
      var name = "";
      nameId.split(/\.\-/).foreach(function(word){
        name += word.charAt(0).toUpperCase() + word.slice(1) + " ";
      });

      return name;
  }

  var selectElementObj = function()

  /**
   * Get the name of the UI card from the Id
   * @param {String} nameId The html id of the element inside which the Svg
   *                      element is to be rendered
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  this.getSvgId = function (nameId) {
      return nameId + "-svg";
  }

  // Public methods

  /**
   * Create a card UI to display the Svg element. It creates a footer to display
   * the name of the svg. The name is calculated from the data matrix id
   * @param {String} parentNameOrDiv The html id of the element inside which the card
   *                      element is to be rendered OR the container d3 object
   *                      itself is passed. Either a string or a DOM.
   * @param {Number} svgWidth The width of the SVG graphics
   * @param {Number} svgHeight The height of the SVG graphics
   * @param {String} dmId The data matrix Id
   * @returns {Object} The d3 object which corresponds to the svg element
   *
  */
  this.createCardType1 = function (
    parentNameOrDiv,
    svgWidth,
    svgHeight,
    dmId
  ) {
      // Check and select the parent div id to render svg
      var cardParentContainer = (typeof parentNameOrDiv == "string")?
            d3.select("#" + parentNameOrDiv) : parentNameOrDiv;

      // Get parent id
      var parentId = (typeof parentNameOrDiv == "string")?
            parentNameOrDiv : nameOrDiv.attr("id");

      // Create the div of card
      var divContainer = cardParentContainer.append("div")
                            .attr("class", "card text-center");

      // Card div ui
      var divContainerChild = divContainer.append("div")
                                  .attr("class", "card-block")
                                  .append("div").attr("id", parentId + "-div-svg");

      // Card footer div
      divContainer.append("div").attr("class", "card-footer text-muted")
                   .append("small").text(getName(dmId));

      // Create the svg element
      return D3Wrapper.createSvg(divContainerChild, svgWidth, svgHeight);
  };

  /**
   * Create a Svg element.
   * @param {String} parentIdOrObj The html id of the element to render svg OR
                                  the d3 svg object
   * @param {Number} svgWidth The width of the SVG graphics
   * @param {Number} svgHeight The height of the SVG graphics
   * @returns {Object} The d3 object which corresponds to the svg element
   *
  */
  this.createSvg = function(
    parentIdOrObj,
    svgWidth,
    svgHeight
  ) {
    var svgContainer = (typeof parentIdOrObj == "string")?
          d3.select("#" + parentIdOrObj).append("svg") : parentIdOrObj;
    return svgContainer.attr("viewbox", "0 0 " + svgWidth + " " + svgHeight);
  }

  /**
   * Create a simple div UI to contain the svg
   * @param {String} parentIdOrObj The html id of the element inside which the Svg
   *                      element is to be rendered OR the d3 object itself
   * @param {Number} svgWidth The width of the SVG graphics
   * @param {Number} svgHeight The height of the SVG graphics
   * @returns {Object} The d3 object which corresponds to the svg element
   */
  this.createDivType1 = function (parentIdOrObj, svgWidth,
    svgHeight) {
      // Get the div element
      var divContainer = (typeof parentIdOrObj == "string")?
            d3.select("#" + parentIdOrObj).append("div") : parentIdOrObj;

      // Get parent id
      var parentId = (typeof parentIdOrObj == "string")?
                  parentIdOrObj : parentIdOrObj.attr("id");

      // Add the div id
      divContainer.attr("id", parentId + "-div-svg");

      // Create the svg
      return UiUtility.createSvg(divContainer, svgWidth, svgHeight);
  }
};
