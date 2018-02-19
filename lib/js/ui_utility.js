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
var UiUtility = new function () {

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
        nameId.split(/\.\-/).forEach(function (word) {
            name += word.charAt(0).toUpperCase() + word.slice(1) + " ";
        });

        return name;
    };

    /**
     * Get the html d3 object from the input.
     * @param {String} parentNameOrDiv The html id of the element or the element object
     *                                  initialized using d3js library
     * @returns {Object} The d3 object which corresponds to the element
     */
    var selectElementObj = function (parentNameOrDiv) {
        return (typeof parentNameOrDiv == "string") ?
            d3.select("#" + parentNameOrDiv) : parentNameOrDiv;
    };

    /**
     * Get the id of the div element which is the immediate parent of the svg
     * @param {String} nameId The html id of the element inside which the Svg
     *                      element is to be rendered
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    var getDivSvgId = function (nameId) {
        return nameId + "-div";
    };

    /**
     * Get the id of the div element which is the immediate parent of the svg
     * @param {String} nameId The html id of the element inside which the Svg
     *                      element is to be rendered
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    var getSvgId = function (nameId) {
        return nameId + "-svg";
    }

    // Public methods

    /**
     * Get the id attribute of the element
     * @param {String} parentNameOrDiv The html id of the element or the element object
     *                                  initialized using d3js library
     * @returns {Object} The id value of the element
     */
    this.getElementId = function (parentNameOrDiv) {
        return (typeof parentNameOrDiv == "string") ?
            parentNameOrDiv : parentNameOrDiv.attr("id");
    };

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
    this.createCardType1 = function (parentNameOrDiv,
                                     svgWidth,
                                     svgHeight,
                                     dmId) {
        // Check and select the parent div id to render svg
        var cardParentContainer = selectElementObj(parentNameOrDiv);

        // Create the div of card
        var divContainer = cardParentContainer.append("div")
            .attr("class", "card text-center");

        // Card div ui
        var divContainerChild = divContainer.append("div")
            .attr("class", "card-block")
            .append("div").attr("id", DmCommon.generateDivID());

        // Card footer div
        divContainer.append("div").attr("class", "card-footer text-muted")
            .append("small").text(getName(dmId));

        // Create the svg element
        return D3Wrapper.createSvg(DmCommon.generateSvgID(), [svgWidth, svgHeight], divContainerChild);
    };

    /**
     * Create a Svg element.
     * @param {String} parentNameOrDiv The html id of the element to render svg OR
     the d3 svg object
     * @param {Number} svgWidth The width of the SVG graphics
     * @param {Number} svgHeight The height of the SVG graphics
     * @returns {Object} The d3 object which corresponds to the svg element
     *
     */
    this.createSvg = function (parentNameOrDiv,
                               svgWidth,
                               svgHeight) {
        var divContainer = selectElementObj(parentNameOrDiv);

        return D3Wrapper.createSvg(DmCommon.generateSvgID(), [svgWidth, svgHeight], divContainer);
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
        var divContainer = selectElementObj(parentIdOrObj);

        // Add the div id
        divContainer.attr("id", DmCommon.generateDivID());

        // Create the svg
        return D3Wrapper.createSvg(DmCommon.generateSvgID(), svgWidth, svgHeight, divContainer);
    }
};
